# ==============================================================================
# Seahorse XF analyser: Mito Stress Test & Glycolysis Stress Test
#
# Upload a Seahorse Wave / Report Generator export (the "Rate" sheet of the
# .xlsx, or a .csv with Measurement / Well / Group / Time / OCR / ECAR / PER),
# calculate the standard parameters per well, and plot them.
#
# Required packages: shiny, readxl, dplyr, tidyr, tibble, ggplot2, DT
# Optional packages: ggprism (Prism theme), pzfx (.pzfx export; desktop R only)
#
# Run locally:      shiny::runApp("path/to/folder/containing/app.R")
# Run in browser:   exported with {shinylive} and hosted on GitHub Pages (see README.md)
# ==============================================================================

# Plain top-level library() calls so shinylive can detect what to load.
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

options(shiny.maxRequestSize = 50 * 1024^2)

in_webr <- grepl("emscripten", R.version$os)

# ggprism / pzfx are optional and are looked up at run time (package names are
# passed as strings, never written as library()/pkg:: calls) so that shinylive
# does not need them to start. In the browser, a missing optional package is
# fetched on demand from the webR package repository; if that fails the app
# falls back gracefully.
optional_pkg <- local({
  cache <- list()
  function(pkg) {
    if (!is.null(cache[[pkg]])) return(cache[[pkg]])
    ok <- requireNamespace(pkg, quietly = TRUE)
    if (!ok && in_webr) {
      inst <- getExportedValue("webr", "install")
      ok <- tryCatch({ inst(pkg, quiet = TRUE); requireNamespace(pkg, quietly = TRUE) },
                     error = function(e) FALSE, warning = function(w) FALSE)
    }
    cache[[pkg]] <<- ok
    ok
  }
})

theme_choices <- c("Classic", "Minimal", "Prism")

# Stacked CSV blocks (one block per parameter, one column per group) that can be
# pasted straight into GraphPad Prism column tables.
prism_blocks <- function(pl) {
  out <- character(0)
  for (nm in levels(droplevels(pl$Parameter))) {
    x <- pl[pl$Parameter == nm, ]
    cols <- split(x$value, droplevels(x$Group))
    n <- max(lengths(cols))
    m <- do.call(cbind, lapply(cols, function(v) c(v, rep(NA_real_, n - length(v)))))
    out <- c(out, paste0("# ", nm),
             utils::capture.output(utils::write.csv(as.data.frame(m, check.names = FALSE),
                                                    row.names = FALSE, na = "")),
             "")
  }
  out
}

# Save a ggplot as PNG/PDF. In the browser (webR) large 300-dpi bitmaps can fail
# or come out empty, so PNG export retries with a plain grDevices::png() device and
# then at a lower resolution. Returns the dpi actually used (NA for PDF); stops with
# a readable message if nothing worked.
save_plot <- function(file, plot, w, h, fmt = c("png", "pdf"), dpi = 300) {
  fmt <- match.arg(fmt)
  good <- function() file.exists(file) && isTRUE(file.size(file) > 1000)
  if (fmt == "pdf") {
    ggsave(file, plot, device = "pdf", width = w, height = h)
    if (!good()) stop("PDF export produced an empty file.")
    return(invisible(NA_real_))
  }
  via_ggsave <- function(d) {
    ggsave(file, plot, device = "png", dpi = d, width = w, height = h, bg = "white")
  }
  via_device <- function(d) {
    grDevices::png(file, width = w, height = h, units = "in", res = d, bg = "white")
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)
  }
  for (d in unique(c(dpi, min(dpi, 150), 96))) {
    for (f in list(via_ggsave, via_device)) {
      if (file.exists(file)) unlink(file)
      ok <- tryCatch({ f(d); good() }, error = function(e) FALSE, warning = function(w) good())
      if (ok) return(invisible(d))
    }
  }
  stop("PNG export failed in this browser. Try the PDF button, or a smaller width/height.")
}

`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a[1])) b else a

# ------------------------------------------------------------------------------
# Parameter definitions
# ------------------------------------------------------------------------------
PARAMS <- list(
  mito = c("Non-mitochondrial respiration", "Basal respiration", "Proton leak",
           "ATP-linked respiration", "Maximal respiration",
           "Spare respiratory capacity", "Spare respiratory capacity (%)",
           "Coupling efficiency (%)"),
  glyco = c("Non-glycolytic acidification", "Glycolysis", "Glycolytic capacity",
            "Glycolytic reserve", "Glycolytic reserve (%)")
)

DEFAULT_LABELS <- list(
  mito  = "Basal, Oligomycin, FCCP, Rot/AA",
  glyco = "Basal, Glucose, Oligomycin, 2-DG"
)

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))

is_excel <- function(name) tolower(tools::file_ext(name)) %in% c("xlsx", "xls", "xlsm")

# "A1" / "a01" / "A01" -> "A01"
norm_well <- function(w) {
  w <- toupper(trimws(as.character(w)))
  ok <- !is.na(w) & grepl("^[A-Z]+[0-9]+$", w)
  out <- w
  out[ok] <- sprintf("%s%02d", sub("[0-9]+$", "", w[ok]),
                     as.integer(sub("^[A-Z]+", "", w[ok])))
  out
}

parse_wells <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) return(character(0))
  w <- trimws(strsplit(txt, "[,; \t\n]+")[[1]])
  norm_well(w[nzchar(w)])
}

# Strip zero-width / non-breaking characters Wave sometimes leaves in sample names
clean_label <- function(x) {
  x <- gsub("[\u200B\u200C\u200D\uFEFF]", "", as.character(x))
  trimws(gsub("[[:space:]\u00A0]+", " ", x))
}

std_names <- function(x) gsub("[^a-z0-9]", "", tolower(x))

parse_time <- function(x) {
  if (inherits(x, "POSIXt")) {
    lt <- as.POSIXlt(x, tz = "UTC")
    return(lt$hour * 60 + lt$min + lt$sec / 60)
  }
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  n <- num(x)
  if (!all(is.na(n))) return(n)
  vapply(strsplit(x, ":"), function(p) {
    p <- num(p)
    if (length(p) == 3 && !anyNA(p)) p[1] * 60 + p[2] + p[3] / 60 else NA_real_
  }, numeric(1))
}

smin  <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
smax  <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
smean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
slast <- function(x) { x <- x[!is.na(x)]; if (length(x)) x[length(x)] else NA_real_ }
safe_div <- function(a, b) if (is.na(a) || is.na(b) || b <= 0) NA_real_ else a / b

# ------------------------------------------------------------------------------
# Reading / tidying Seahorse exports
# ------------------------------------------------------------------------------
tidy_seahorse <- function(d) {
  nm <- std_names(names(d))
  pick <- function(pat) { i <- grep(pat, nm); if (length(i)) i[1] else NA_integer_ }
  i_w <- pick("^well$");        i_g <- pick("^group$")
  i_m <- pick("^measurement$"); i_t <- pick("^time")
  i_o <- pick("^ocr");          i_e <- pick("^ecar")
  i_p <- pick("^per$|^perpmol")

  if (is.na(i_w) || is.na(i_g) || (is.na(i_o) && is.na(i_e) && is.na(i_p)))
    stop("Could not find Well, Group and OCR/ECAR/PER columns. Use the 'Rate' sheet ",
         "from the Wave / Report Generator export (columns: Measurement, Well, Group, ",
         "Time, OCR, ECAR, PER).")

  col_or_na <- function(i, f = identity) if (is.na(i)) rep(NA_real_, nrow(d)) else f(d[[i]])

  out <- tibble::tibble(
    Well        = norm_well(d[[i_w]]),
    Group       = clean_label(d[[i_g]]),
    Measurement = col_or_na(i_m, num),
    Time        = col_or_na(i_t, parse_time),
    OCR         = col_or_na(i_o, num),
    ECAR        = col_or_na(i_e, num),
    PER         = col_or_na(i_p, num)
  ) %>%
    filter(!is.na(Well), !is.na(Group), nzchar(Group))

  if (all(is.na(out$Measurement))) {
    if (all(is.na(out$Time))) stop("Neither a Measurement nor a Time column was found.")
    out <- out %>% group_by(Well) %>%
      mutate(Measurement = dense_rank(Time)) %>% ungroup()
  }
  out
}

# Pull the measurement-cycles-per-command and normalisation unit from the
# 'Assay Configuration' sheet of a Wave export (if present)
read_config <- function(path) {
  cfg <- tryCatch(suppressMessages(readxl::read_excel(
    path, sheet = "Assay Configuration", col_names = FALSE, .name_repair = "minimal")),
    error = function(e) NULL)
  out <- list(cycles = NULL, norm_unit = NULL)
  if (is.null(cfg) || !nrow(cfg)) return(out)
  m <- as.matrix(cfg)
  cells <- trimws(as.character(m))
  cy <- regmatches(cells, regexpr("^Cycles:\\s*[0-9]+", cells))
  cy <- as.integer(sub("^Cycles:\\s*", "", cy))
  if (length(cy) == 4) out$cycles <- cy
  i <- which(trimws(m[, 1]) == "Normalization Unit:")
  if (length(i) && ncol(m) >= 2 && !is.na(m[i[1], 2])) out$norm_unit <- trimws(m[i[1], 2])
  out
}

read_seahorse <- function(path, name, sheet = NULL) {
  cfg <- list()
  if (is_excel(name)) {
    sheets <- readxl::excel_sheets(path)
    sh <- if (!is.null(sheet) && sheet %in% sheets) sheet
          else if ("Rate" %in% sheets) "Rate" else sheets[1]
    d <- suppressMessages(readxl::read_excel(path, sheet = sh, .name_repair = "minimal"))
    cfg <- read_config(path)
    cfg$sheet <- sh
  } else {
    d <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM")
  }
  out <- tidy_seahorse(d)
  attr(out, "config") <- cfg
  out
}

read_norm <- function(path, name) {
  d <- if (is_excel(name)) readxl::read_excel(path) else
    utils::read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  validate(need(ncol(d) >= 2, "Normalisation file needs two columns: Well, value."))
  tibble::tibble(Well = norm_well(d[[1]]), Norm = num(d[[2]])) %>%
    filter(!is.na(Well)) %>%
    mutate(Norm = ifelse(!is.na(Norm) & Norm > 0, Norm, NA_real_)) %>%
    distinct(Well, .keep_all = TRUE)
}

# ------------------------------------------------------------------------------
# Example data (so the app can be tried without a file)
# ------------------------------------------------------------------------------
sim_data <- function(assay) {
  set.seed(42)
  times <- seq(6.5, by = 6.5, length.out = 12)
  prof <- if (assay == "mito") c(100, 98, 96, 58, 54, 52, 210, 200, 190, 20, 16, 15)
          else                 c(8, 8, 7, 35, 38, 38, 55, 58, 57, 9, 8, 8)
  fac_treated <- if (assay == "mito") c(.75, .8, .55, 1) else c(1, .7, .6, 1)

  mk <- function(wells, grp, f, noise = TRUE) {
    do.call(rbind, lapply(wells, function(w) {
      v <- if (noise) prof * rep(f, each = 3) * rlnorm(1, 0, 0.08) * rlnorm(12, 0, 0.03)
           else rnorm(12, 0, 1.5)
      data.frame(Measurement = 1:12, Well = w, Group = grp, Time = times, Main = v)
    }))
  }
  d <- rbind(
    mk(c("A01", "A12", "H01", "H12"), "Background", 1, noise = FALSE),
    mk(sprintf("C%02d", 2:6), "Control", rep(1, 4)),
    mk(sprintf("D%02d", 2:6), "Treated", fac_treated)
  )
  n <- nrow(d)
  d$OCR  <- if (assay == "mito") d$Main else 120 * rlnorm(n, 0, 0.05)
  d$ECAR <- if (assay == "mito") 25 * rlnorm(n, 0, 0.1) else d$Main
  d$PER  <- d$ECAR * 2.1
  d$Main <- NULL
  tibble::as_tibble(d)
}

# ------------------------------------------------------------------------------
# Parameter calculation (one well)
#   v      numeric vector of rates ordered by measurement
#   counts integer(4): number of measurements in each of the 4 phases
#   method "agilent" = Seahorse report-generator definitions (last / min / max)
#          "mean"    = phase means
# ------------------------------------------------------------------------------
calc_well <- function(v, counts, assay, method) {
  v  <- v[seq_len(sum(counts))]
  ph <- rep(seq_along(counts), counts)
  p  <- lapply(1:4, function(k) v[ph == k])
  agi <- identical(method, "agilent")

  if (assay == "mito") {
    nonmito <- if (agi) smin(p[[4]])  else smean(p[[4]])
    pre     <- if (agi) slast(p[[1]]) else smean(p[[1]])   # last rate before oligo
    olig    <- if (agi) smin(p[[2]])  else smean(p[[2]])   # min rate after oligo
    fccp    <- if (agi) smax(p[[3]])  else smean(p[[3]])   # max rate after FCCP
    basal   <- pre - nonmito
    maxr    <- fccp - nonmito
    atp     <- pre - olig
    res <- c(nonmito, basal, olig - nonmito, atp, maxr, maxr - basal,
             100 * safe_div(maxr, basal), 100 * safe_div(atp, basal))
  } else {
    base <- if (agi) slast(p[[1]]) else smean(p[[1]])             # last rate before glucose
    gly  <- if (agi) smax(c(p[[1]], p[[2]])) else smean(p[[2]])   # max rate before oligo
    cap  <- if (agi) smax(p[[3]]) else smean(p[[3]])              # max rate after oligo
    glyc <- gly - base
    capc <- cap - base
    res <- c(base, glyc, capc, capc - glyc, 100 * safe_div(capc, glyc))
  }
  stats::setNames(res, PARAMS[[assay]])
}

compute_params <- function(d, counts, assay, method) {
  d %>%
    arrange(Well, Measurement) %>%
    group_by(Well, Group) %>%
    group_modify(function(.x, .y)
      tibble::as_tibble_row(calc_well(.x$value, counts, assay, method))) %>%
    ungroup() %>%
    arrange(Group, Well)
}

# ------------------------------------------------------------------------------
# Statistics (wells within the plate; see note in the UI)
# ------------------------------------------------------------------------------
run_pair_stats <- function(pl, pairs, adjust = "holm") {
  out <- lapply(split(pl, droplevels(pl$Parameter)), function(x) {
    x <- x[!is.na(x$value), ]
    rows <- lapply(seq_len(nrow(pairs)), function(i) {
      a <- x$value[x$Group == pairs$g1[i]]
      b <- x$value[x$Group == pairs$g2[i]]
      if (length(a) < 2 || length(b) < 2) return(NULL)
      p <- tryCatch(stats::t.test(b, a)$p.value, error = function(e) NA_real_)
      data.frame(`Group 1` = pairs$g1[i], `Group 2` = pairs$g2[i],
                 n1 = length(a), n2 = length(b),
                 `Mean 1` = mean(a), `Mean 2` = mean(b),
                 Difference = mean(b) - mean(a), p = p,
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    if (!length(rows)) return(NULL)
    r <- do.call(rbind, rows)
    r$`p adj` <- stats::p.adjust(r$p, method = adjust)   # across the comparisons for this parameter
    r
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(NULL)
  res <- do.call(rbind, Map(function(df, nm) cbind(Parameter = nm, df, stringsAsFactors = FALSE),
                            out, names(out)))
  rownames(res) <- NULL
  res$Sig <- as.character(cut(res$`p adj`, c(-Inf, 1e-4, 1e-3, 1e-2, 0.05, Inf),
                              labels = c("****", "***", "**", "*", "ns")))
  res
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Seahorse XF analyser: Mito & Glyco Stress Test"),
  tags$head(tags$style(HTML("
    .sec { font-weight:600; margin-top:14px; border-bottom:1px solid #ddd; padding-bottom:3px; }
    .note { color:#666; font-size:12px; }
    .dlrow { margin:8px 0 18px 0; }
    .dlrow .btn { margin-right:6px; }
  "))),
  sidebarLayout(
    sidebarPanel(
      width = 3, style = "max-height:95vh; overflow-y:auto;",

      div(class = "sec", "1. Data"),
      fileInput("file", NULL, accept = c(".xlsx", ".xls", ".csv"),
                placeholder = "Wave .xlsx ('Rate' sheet) or .csv"),
      uiOutput("sheet_ui"),
      actionLink("example", "Load example data"),

      div(class = "sec", "2. Assay"),
      radioButtons("assay", NULL, choices = c("Mito Stress Test" = "mito",
                                              "Glyco Stress Test" = "glyco")),
      selectInput("measure", "Rate to analyse", choices = c("OCR", "ECAR", "PER")),
      textInput("counts", "Measurements per phase", value = "3,3,3,3"),
      div(class = "note", "Four phases: baseline, then after each of the three injections."),
      textInput("labels", "Phase labels", value = DEFAULT_LABELS$mito),
      selectInput("method", "Parameter calculation",
                  choices = c("Agilent report generator (last / min / max)" = "agilent",
                              "Phase means" = "mean")),

      div(class = "sec", "3. Samples"),
      selectizeInput("groups", "Groups to include (order = plot order)",
                     choices = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"))),
      checkboxInput("strip_suffix", "Drop 'Mito' / 'Glyco' from group labels", TRUE),
      textInput("exclude", "Exclude wells", placeholder = "e.g. C03, D05"),
      checkboxInput("use_norm", "Normalise to a per-well value (cells, protein...)", FALSE),
      conditionalPanel(
        "input.use_norm",
        fileInput("norm_file", NULL, accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Two columns: Well, value"),
        div(class = "note", "Leave off if you already normalised in Wave (e.g. by using the 'Normalized Rate' sheet).")
      ),
      textInput("norm_unit", "Normalisation unit for axis labels",
                placeholder = "e.g. ug protein"),

      div(class = "sec", "4. Display"),
      selectInput("xaxis", "Kinetic x-axis", c("Time (min)", "Measurement")),
      selectInput("err", "Error bars", c("SEM", "SD")),
      checkboxInput("show_wells", "Show individual wells on kinetic plot", FALSE),
      selectInput("ptype", "Parameter plot", c("Bar", "Box")),
      uiOutput("param_ui"),
      numericInput("ncol", "Parameter plot columns", 4, min = 1, max = 8),
      selectInput("palette", "Palette",
                  c("Set1", "Dark2", "Set2", "Paired", "Okabe-Ito", "Viridis")),
      selectInput("theme", "Theme", theme_choices),
      numericInput("base_size", "Font size", 13, min = 8, max = 24),
      fluidRow(
        column(6, numericInput("dl_w", "Kinetic export width (in)", 8, min = 2, max = 20)),
        column(6, numericInput("dl_h", "height (in)", 5, min = 2, max = 20))
      ),
      numericInput("dl_dpi", "PNG resolution (dpi)", if (in_webr) 150 else 300,
                   min = 72, max = 600, step = 50)
    ),

    mainPanel(
      width = 9,
      uiOutput("data_info"),
      tabsetPanel(
        tabPanel("Kinetic profile",
          plotOutput("kin_plot", height = "500px"),
          div(class = "dlrow",
              downloadButton("dl_kin_png", "PNG"),
              downloadButton("dl_kin_pdf", "PDF"),
              downloadButton("dl_kin_csv", "Group means (CSV)"))),
        tabPanel("Parameters",
          plotOutput("par_plot", height = "auto"),
          div(class = "dlrow",
              downloadButton("dl_par_png", "PNG"),
              downloadButton("dl_par_pdf", "PDF"),
              uiOutput("prism_ui", inline = TRUE))),
        tabPanel("Tables",
          h4("Per-well parameters"),
          DTOutput("tbl_wells"),
          div(class = "dlrow", downloadButton("dl_wells", "Per-well CSV")),
          h4("Group summary"),
          DTOutput("tbl_sum"),
          div(class = "dlrow", downloadButton("dl_sum", "Summary CSV"))),
        tabPanel("Stats",
          div(style = "margin-top:10px;", uiOutput("stat_ui")),
          div(class = "note", style = "margin:6px 0 10px 0;",
              "Welch t-test for each chosen comparison (Difference = Group 2 \u2212 Group 1). ",
              "The correction is applied across the comparisons listed for each parameter, ",
              "not across parameters. Wells within one plate are technical replicates, so ",
              "these p-values overstate significance; for publication, average wells per ",
              "experiment and test across independent experiments (e.g. in Prism)."),
          DTOutput("tbl_stats"),
          div(class = "dlrow", downloadButton("dl_stats", "Stats CSV"))),
        tabPanel("QC",
          h4("Injection check"),
          div(class = "note",
              "Mito: flags wells where oligomycin lowers OCR by less than the threshold, or ",
              "FCCP does not raise it above baseline. Glyco: flags wells where glucose raises ",
              "ECAR by less than the threshold. A failed injection invalidates the parameters ",
              "that depend on it (e.g. proton leak, ATP-linked, coupling efficiency)."),
          fluidRow(
            column(4, numericInput("flag_thr", "Flag if response below (%)", 10, min = 0, max = 100)),
            column(8, div(style = "margin-top:25px;",
                          actionButton("excl_flagged", "Add flagged wells to 'Exclude wells'")))),
          DTOutput("tbl_flags"),
          h4("All wells"),
          div(class = "note", style = "margin:10px 0;",
              "Every well, labelled at its last point."),
          plotOutput("qc_plot", height = "480px"),
          h4("Data (as analysed)"),
          DTOutput("tbl_raw")),
        tabPanel("Methods", uiOutput("methods"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(example = FALSE)

  observeEvent(input$file,    rv$example <- FALSE)
  observeEvent(input$example, rv$example <- TRUE)

  output$sheet_ui <- renderUI({
    req(input$file, !rv$example)
    if (is_excel(input$file$name)) {
      s <- readxl::excel_sheets(input$file$datapath)
      tagList(
        selectInput("sheet", "Worksheet", s, selected = if ("Rate" %in% s) "Rate" else s[1]),
        div(class = "note", style = "margin-top:-10px; margin-bottom:8px;",
            "'Rate' = un-normalised; 'Normalized Rate' = divided by the per-well values entered in Wave.")
      )
    }
  })

  # ---- data in -----------------------------------------------------------------
  raw_df <- reactive({
    if (isTRUE(rv$example)) return(sim_data(input$assay))
    validate(need(!is.null(input$file),
                  "Upload a Seahorse export (or click 'Load example data') to begin."))
    tryCatch(
      read_seahorse(input$file$datapath, input$file$name, input$sheet),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })

  # Reset selections when data or assay type changes
  observeEvent(list(raw_df(), input$assay), {
    req(input$assay)
    r <- raw_df()
    g <- unique(r$Group)
    cand <- g[!grepl("^(background|unassigned)$", g, ignore.case = TRUE)]
    # Mixed plates: pick the groups whose names mention the chosen assay
    kw  <- if (input$assay == "mito") "mito" else "glyco"
    hit <- cand[grepl(kw, cand, ignore.case = TRUE)]
    updateSelectizeInput(session, "groups", choices = g,
                         selected = if (length(hit)) hit else cand)

    has_vals <- function(k) any(!is.na(r[[k]]) & r[[k]] != 0)
    avail <- c("OCR", "ECAR", "PER")[vapply(c("OCR", "ECAR", "PER"), has_vals, logical(1))]
    want <- if (input$assay == "mito") "OCR" else "ECAR"
    updateSelectInput(session, "measure", choices = avail,
                      selected = if (want %in% avail) want else avail[1])
    updateTextInput(session, "labels", value = DEFAULT_LABELS[[input$assay]])

    cfg <- attr(r, "config")
    n_meas <- n_distinct(r$Measurement)
    cy <- cfg$cycles
    if (!is.null(cy) && sum(cy) == n_meas) {
      updateTextInput(session, "counts", value = paste(cy, collapse = ","))
    } else if (n_meas %% 4 == 0) {
      updateTextInput(session, "counts", value = paste(rep(n_meas / 4, 4), collapse = ","))
    }
    nu <- if (!is.null(cfg$sheet) && grepl("^Normalized", cfg$sheet)) cfg$norm_unit else ""
    updateTextInput(session, "norm_unit", value = nu %||% "")
  })

  counts <- reactive({
    x <- trimws(strsplit(input$counts %||% "", "[,; ]+")[[1]])
    x <- num(x[nzchar(x)])
    validate(need(length(x) == 4 && !anyNA(x) && all(x >= 1) && all(x == round(x)),
                  "'Measurements per phase' needs four positive whole numbers, e.g. 3,3,3,3."))
    n_meas <- n_distinct(raw_df()$Measurement)
    validate(need(sum(x) <= n_meas,
                  sprintf("Phases add up to %d measurements but the file only has %d.",
                          sum(x), n_meas)))
    as.integer(x)
  })

  phase_labels <- reactive({
    l <- trimws(strsplit(input$labels %||% "", ",")[[1]])
    if (length(l) == 4 && all(nzchar(l))) l else paste("Phase", 1:4)
  })

  use_time <- reactive(identical(input$xaxis, "Time (min)") && any(!is.na(raw_df()$Time)))

  dat_base <- reactive({
    r <- raw_df()
    req(input$measure, input$groups)
    validate(need(input$measure %in% names(r) && any(!is.na(r[[input$measure]])),
                  paste(input$measure, "is not in this file.")))
    d <- r %>% filter(Group %in% input$groups)
    d$value <- d[[input$measure]]

    if (isTRUE(input$use_norm)) {
      validate(need(!is.null(input$norm_file),
                    "Upload a normalisation file, or untick normalisation."))
      nf <- read_norm(input$norm_file$datapath, input$norm_file$name)
      d <- d %>% left_join(nf, by = "Well") %>% mutate(value = value / Norm)
    }
    d$X <- if (use_time()) d$Time else d$Measurement
    lab <- if (isTRUE(input$strip_suffix))
      trimws(sub("[ _-]*(mito|glyco)\\w*$", "", input$groups, ignore.case = TRUE)) else input$groups
    if (anyDuplicated(lab)) lab <- input$groups
    d$Group <- factor(d$Group, levels = input$groups, labels = lab)
    d
  })

  dat <- reactive({
    d <- dat_base()
    ex <- parse_wells(input$exclude)
    if (length(ex)) d <- d %>% filter(!Well %in% ex)
    validate(need(nrow(d) > 0, "No wells left after filtering."))
    d
  })

  # ---- injection QC: flag wells that did not respond ---------------------------
  flag_tbl <- reactive({
    d <- dat_base(); cn <- counts(); thr <- input$flag_thr %||% 10
    ex <- parse_wells(input$exclude)
    d %>% arrange(Well, Measurement) %>% group_by(Well, Group) %>%
      group_modify(function(.x, .y) {
        v  <- .x$value[seq_len(sum(cn))]
        ph <- rep(1:4, cn)
        p  <- lapply(1:4, function(k) v[ph == k])
        pre <- slast(p[[1]])
        if (input$assay == "mito") {
          inh <- 100 * safe_div(pre - smin(p[[2]]), pre)
          fc  <- safe_div(smax(p[[3]]), pre)
          bad <- c(if (!is.na(inh) && inh < thr) "no oligomycin response",
                   if (!is.na(fc) && fc <= 1) "no FCCP response")
          tibble::tibble(`Oligo inhibition (%)` = inh, `FCCP / baseline` = fc,
                         Flag = paste(bad, collapse = "; "))
        } else {
          gr <- 100 * safe_div(smax(p[[2]]) - pre, pre)
          tibble::tibble(`Glucose response (%)` = gr,
                         Flag = if (!is.na(gr) && gr < thr) "no glucose response" else "")
        }
      }) %>% ungroup() %>%
      mutate(Excluded = Well %in% ex) %>% arrange(Group, Well)
  })

  observeEvent(input$excl_flagged, {
    f <- flag_tbl() %>% filter(Flag != "")
    updateTextInput(session, "exclude",
                    value = paste(union(parse_wells(input$exclude), f$Well), collapse = ", "))
  })

  output$tbl_flags <- renderDT({
    f <- flag_tbl() %>% filter(Flag != "")
    validate(need(nrow(f) > 0, "No wells flagged at this threshold."))
    datatable(f, rownames = FALSE, options = list(pageLength = 10, dom = "tp")) %>%
      formatRound(setdiff(names(f), c("Well", "Group", "Flag", "Excluded")), 2)
  })

  params_wide <- reactive(compute_params(dat(), counts(), input$assay, input$method))

  params_long <- reactive({
    params_wide() %>%
      pivot_longer(-c(Well, Group), names_to = "Parameter", values_to = "value") %>%
      mutate(Parameter = factor(Parameter, levels = PARAMS[[input$assay]]))
  })

  # ---- labels / styling --------------------------------------------------------
  unit_str <- reactive({
    base <- switch(input$measure, OCR = "pmol/min", ECAR = "mpH/min", PER = "pmol H+/min")
    nu <- input$norm_unit %||% ""
    if (nzchar(nu)) paste0(base, "/", nu) else base
  })

  gcols <- function(lv) {
    n <- length(lv)
    cols <- switch(input$palette,
      "Okabe-Ito" = rep_len(c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                              "#0072B2", "#D55E00", "#CC79A7", "#999999"), n),
      "Viridis"   = scales::viridis_pal(end = 0.9)(max(n, 1)),
      rep_len(scales::brewer_pal(palette = input$palette)(
        c(Set1 = 9, Dark2 = 8, Set2 = 8, Paired = 12)[[input$palette]]), n))
    stats::setNames(cols, lv)
  }

  noted <- character(0)
  note_once <- function(msg) {
    if (!msg %in% noted) {
      noted <<- c(noted, msg)
      showNotification(msg, type = "warning", duration = 8)
    }
  }

  theme_choice <- function() {
    bs <- input$base_size %||% 13
    base <- switch(input$theme,
      Minimal = theme_minimal(base_size = bs),
      Prism   = if (optional_pkg("ggprism")) getExportedValue("ggprism", "theme_prism")(base_size = bs)
                else { note_once("Prism theme (ggprism) is not available here; using Classic.")
                       theme_classic(base_size = bs) },
      theme_classic(base_size = bs))
    base + theme(legend.position = "top")
  }

  output$param_ui <- renderUI({
    p <- PARAMS[[input$assay]]
    checkboxGroupInput("params_show", "Parameters to plot", choices = p, selected = p)
  })

  output$data_info <- renderUI({
    d <- dat()
    nw <- d %>% distinct(Well, Group) %>% count(Group)
    div(class = "note", style = "margin-bottom:8px;",
        paste0("Wells per group: ",
               paste0(nw$Group, " (", nw$n, ")", collapse = ", "),
               "  |  ", n_distinct(d$Measurement), " measurements  |  analysing ",
               input$measure, " (", unit_str(), ")"))
  })

  # ---- kinetic plot ------------------------------------------------------------
  kin_summary <- reactive({
    dat() %>%
      group_by(Group, Measurement) %>%
      summarise(X = mean(X, na.rm = TRUE), m = mean(value, na.rm = TRUE),
                s = sd(value, na.rm = TRUE), n = sum(!is.na(value)), .groups = "drop") %>%
      mutate(err = replace_na(if (input$err == "SEM") s / sqrt(n) else s, 0))
  })

  kin_plot <- reactive({
    d <- dat(); kin <- kin_summary(); cn <- counts(); lab <- phase_labels()
    xm <- kin %>% group_by(Measurement) %>%
      summarise(X = mean(X), .groups = "drop") %>% arrange(Measurement)
    Xv <- xm$X
    ends <- cumsum(cn)
    validate(need(ends[3] + 1 <= length(Xv), "Not enough measurements for the 4 phases."))
    bounds <- (Xv[ends[1:3]] + Xv[ends[1:3] + 1]) / 2
    xr <- range(Xv); pad <- diff(xr) * 0.03
    ph <- tibble::tibble(xmin = c(xr[1] - pad, bounds), xmax = c(bounds, xr[2] + pad),
                         label = lab, k = 1:4) %>% mutate(xc = (xmin + xmax) / 2)

    p <- ggplot() +
      geom_rect(data = filter(ph, k %% 2 == 1),
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "grey90", alpha = 0.6) +
      geom_vline(xintercept = bounds, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
      geom_text(data = ph, aes(x = xc, y = Inf, label = label),
                vjust = 1.5, size = 3.2, colour = "grey25")
    if (isTRUE(input$show_wells))
      p <- p + geom_line(data = d, aes(x = X, y = value, group = Well, colour = Group),
                         alpha = 0.25, linewidth = 0.3, na.rm = TRUE)
    p +
      geom_line(data = kin, aes(x = X, y = m, colour = Group), linewidth = 0.9) +
      geom_errorbar(data = kin, aes(x = X, ymin = m - err, ymax = m + err, colour = Group),
                    width = diff(xr) * 0.012, linewidth = 0.5) +
      geom_point(data = kin, aes(x = X, y = m, colour = Group), size = 2.2) +
      scale_colour_manual(values = gcols(levels(d$Group)), name = NULL) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.04, 0.12))) +
      labs(x = if (use_time()) "Time (min)" else "Measurement",
           y = paste0(input$measure, " (", unit_str(), ")"),
           caption = paste0("Mean \u00b1 ", input$err)) +
      theme_choice()
  })
  output$kin_plot <- renderPlot(kin_plot(), res = 110)

  # ---- parameter plot ----------------------------------------------------------
  par_plot <- reactive({
    req(input$params_show)
    pl <- params_long() %>%
      filter(Parameter %in% input$params_show) %>%
      arrange(Parameter) %>%
      mutate(plab = ifelse(grepl("%", Parameter), as.character(Parameter),
                           paste0(Parameter, "\n(", unit_str(), ")")))
    pl$plab <- factor(pl$plab, levels = unique(pl$plab))

    ef <- function(x) {
      x <- x[!is.na(x)]
      m <- mean(x)
      e <- if (length(x) > 1) { if (input$err == "SEM") sd(x) / sqrt(length(x)) else sd(x) } else 0
      data.frame(y = m, ymin = m - e, ymax = m + e)
    }

    p <- ggplot(pl, aes(x = Group, y = value))
    if (input$ptype == "Bar") {
      p <- p +
        stat_summary(aes(fill = Group), fun = mean, geom = "col", width = 0.7,
                     alpha = 0.85, colour = "black", linewidth = 0.3) +
        stat_summary(fun.data = ef, geom = "errorbar", width = 0.25, linewidth = 0.5)
    } else {
      p <- p + geom_boxplot(aes(fill = Group), width = 0.6, alpha = 0.8,
                            outlier.shape = NA, linewidth = 0.4, na.rm = TRUE)
    }
    p +
      geom_point(position = position_jitter(width = 0.12, height = 0, seed = 1),
                 size = 1.8, shape = 21, fill = "white", colour = "black",
                 stroke = 0.5, na.rm = TRUE) +
      facet_wrap(~plab, scales = "free_y", ncol = input$ncol) +
      scale_fill_manual(values = gcols(levels(pl$Group)), guide = "none") +
      labs(x = NULL, y = NULL,
           caption = if (input$ptype == "Bar") paste0("Bars: mean \u00b1 ", input$err,
                                                     "; points: individual wells") else NULL) +
      theme_choice() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            strip.background = element_blank(),
            strip.text = element_text(face = "bold"))
  })
  output$par_plot <- renderPlot(par_plot(), res = 100, height = function() {
    n <- length(input$params_show)
    if (!n) 300 else 300 * ceiling(n / max(1, input$ncol))
  })

  par_dims <- function() {
    n <- max(1, length(input$params_show)); nc <- min(n, input$ncol)
    list(w = 2.7 * nc + 0.8, h = 3.1 * ceiling(n / input$ncol) + 0.3)
  }

  # ---- tables ------------------------------------------------------------------
  output$tbl_wells <- renderDT({
    datatable(params_wide(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(PARAMS[[input$assay]], 2)
  })

  summary_tbl <- reactive({
    params_long() %>%
      group_by(Parameter, Group) %>%
      summarise(n = sum(!is.na(value)), Mean = mean(value, na.rm = TRUE),
                SD = sd(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(SEM = SD / sqrt(n)) %>%
      arrange(Parameter, Group)
  })
  output$tbl_sum <- renderDT({
    datatable(summary_tbl(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatRound(c("Mean", "SD", "SEM"), 2)
  })

  # ---- stats: which groups to compare -----------------------------------------
  stat_levels <- reactive(levels(droplevels(dat()$Group)))

  output$stat_ui <- renderUI({
    lv <- stat_levels()
    validate(need(length(lv) >= 2, "Need at least two groups to compare."))
    cm <- utils::combn(lv, 2)
    pair_vals <- paste(cm[1, ], cm[2, ], sep = " || ")
    pair_labs <- paste(cm[1, ], "vs", cm[2, ])
    keep <- function(x, ch, default) if (!is.null(x) && length(x) && x[1] %in% ch) x[1] else default
    fluidRow(
      column(4, radioButtons("stat_mode", "Comparisons",
        c("Each group vs a reference" = "ref", "Specific pairs" = "pairs",
          "All pairwise" = "all"),
        selected = keep(isolate(input$stat_mode), c("ref", "pairs", "all"), "ref"))),
      column(4,
        conditionalPanel("input.stat_mode == 'ref'",
          selectInput("stat_ref", "Reference group", lv,
                      selected = keep(isolate(input$stat_ref), lv, lv[1]))),
        conditionalPanel("input.stat_mode == 'pairs'",
          selectizeInput("stat_pairs", "Pairs to compare",
                         choices = stats::setNames(pair_vals, pair_labs),
                         selected = intersect(isolate(input$stat_pairs), pair_vals),
                         multiple = TRUE,
                         options = list(plugins = list("remove_button"),
                                        placeholder = "Search and select pairs...")))),
      column(4, selectInput("stat_adj", "Multiple-comparison correction",
        c("Holm" = "holm", "Bonferroni" = "bonferroni",
          "Benjamini-Hochberg (FDR)" = "BH", "None" = "none"),
        selected = keep(isolate(input$stat_adj), c("holm", "bonferroni", "BH", "none"), "holm")))
    )
  })

  stat_pairs <- reactive({
    lv <- stat_levels()
    validate(need(length(lv) >= 2, "Need at least two groups to compare."))
    mode <- input$stat_mode %||% "ref"
    if (mode == "all") {
      cm <- utils::combn(lv, 2)
      data.frame(g1 = cm[1, ], g2 = cm[2, ], stringsAsFactors = FALSE)
    } else if (mode == "ref") {
      ref <- input$stat_ref
      validate(need(!is.null(ref) && ref %in% lv, "Choose a reference group."))
      data.frame(g1 = ref, g2 = setdiff(lv, ref), stringsAsFactors = FALSE)
    } else {
      sel <- input$stat_pairs
      validate(need(length(sel) > 0, "Select at least one pair to compare."))
      sp <- strsplit(sel, " || ", fixed = TRUE)
      data.frame(g1 = vapply(sp, `[`, "", 1), g2 = vapply(sp, `[`, "", 2),
                 stringsAsFactors = FALSE)
    }
  })

  stats_tbl <- reactive({
    pl <- params_long()
    if (!is.null(input$params_show)) pl <- pl %>% filter(Parameter %in% input$params_show)
    res <- run_pair_stats(pl, stat_pairs(), input$stat_adj %||% "holm")
    validate(need(!is.null(res),
                  "None of the chosen comparisons has two or more wells in both groups."))
    res
  })

  output$tbl_stats <- renderDT({
    datatable(stats_tbl(), rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatRound(c("Mean 1", "Mean 2", "Difference"), 2) %>%
      formatSignif(c("p", "p adj"), 3)
  })

  output$dl_stats <- downloadHandler(
    filename = function() paste0("seahorse_", input$assay, "_stats.csv"),
    content = function(file) utils::write.csv(stats_tbl(), file, row.names = FALSE))

  # ---- QC ----------------------------------------------------------------------
  output$qc_plot <- renderPlot({
    d <- dat()
    last <- d %>% filter(!is.na(value)) %>% group_by(Well) %>%
      slice_max(X, n = 1, with_ties = FALSE) %>% ungroup()
    ggplot(d, aes(x = X, y = value, group = Well, colour = Group)) +
      geom_line(alpha = 0.7, na.rm = TRUE) +
      geom_text(data = last, aes(label = Well), hjust = -0.15, size = 3,
                show.legend = FALSE) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.1))) +
      scale_colour_manual(values = gcols(levels(d$Group)), name = NULL) +
      labs(x = if (use_time()) "Time (min)" else "Measurement",
           y = paste0(input$measure, " (", unit_str(), ")")) +
      theme_choice()
  }, res = 110)

  output$tbl_raw <- renderDT({
    d <- dat() %>% select(Well, Group, Measurement, Time, value) %>%
      rename(!!input$measure := value)
    datatable(d, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })

  # ---- downloads ---------------------------------------------------------------
  tag <- reactive(paste0("seahorse_", input$assay))

  png_dpi <- reactive({
    d <- suppressWarnings(as.numeric(input$dl_dpi))
    if (length(d) != 1 || is.na(d)) 150 else min(max(d, 72), 600)
  })
  dl_size <- function() {
    w <- suppressWarnings(as.numeric(input$dl_w)); h <- suppressWarnings(as.numeric(input$dl_h))
    list(w = if (length(w) != 1 || is.na(w)) 8 else min(max(w, 2), 20),
         h = if (length(h) != 1 || is.na(h)) 5 else min(max(h, 2), 20))
  }
  run_save <- function(file, plot, w, h, fmt, dpi = 300) {
    tryCatch({
      used <- save_plot(file, plot, w, h, fmt, dpi)
      if (!is.na(used) && used < dpi)
        showNotification(sprintf("PNG saved at %d dpi (%d dpi failed in this browser).",
                                 as.integer(used), as.integer(dpi)), type = "warning", duration = 8)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
      stop(e)
    })
  }

  output$dl_kin_png <- downloadHandler(
    filename = function() paste0(tag(), "_kinetic.png"),
    content = function(file) { z <- dl_size()
      run_save(file, kin_plot(), z$w, z$h, "png", png_dpi()) })
  output$dl_kin_pdf <- downloadHandler(
    filename = function() paste0(tag(), "_kinetic.pdf"),
    content = function(file) { z <- dl_size()
      run_save(file, kin_plot(), z$w, z$h, "pdf") })
  output$dl_kin_csv <- downloadHandler(
    filename = function() paste0(tag(), "_kinetic_group_means.csv"),
    content = function(file) utils::write.csv(
      kin_summary() %>% rename(Mean = m, SD = s) %>% select(-err), file, row.names = FALSE))

  output$dl_par_png <- downloadHandler(
    filename = function() paste0(tag(), "_parameters.png"),
    content = function(file) { z <- par_dims()
      run_save(file, par_plot(), z$w, z$h, "png", png_dpi()) })
  output$dl_par_pdf <- downloadHandler(
    filename = function() paste0(tag(), "_parameters.pdf"),
    content = function(file) { z <- par_dims()
      run_save(file, par_plot(), z$w, z$h, "pdf") })

  output$dl_wells <- downloadHandler(
    filename = function() paste0(tag(), "_per_well_parameters.csv"),
    content = function(file) utils::write.csv(params_wide(), file, row.names = FALSE))
  output$dl_sum <- downloadHandler(
    filename = function() paste0(tag(), "_group_summary.csv"),
    content = function(file) utils::write.csv(summary_tbl(), file, row.names = FALSE))

  # GraphPad Prism export (one table per parameter, one column per group)
  output$prism_ui <- renderUI({
    tagList(
      downloadButton("dl_prism_csv", "Prism-ready CSV"),
      # .pzfx needs the pzfx package (desktop R); not offered in the browser build
      if (!in_webr && optional_pkg("pzfx")) downloadButton("dl_prism", "Prism (.pzfx)")
    )
  })
  output$dl_prism_csv <- downloadHandler(
    filename = function() paste0(tag(), "_parameters_for_prism.csv"),
    content = function(file) writeLines(prism_blocks(params_long()), file))
  output$dl_prism <- downloadHandler(
    filename = function() paste0(tag(), "_parameters.pzfx"),
    content = function(file) {
      pl <- params_long()
      tabs <- lapply(split(pl, droplevels(pl$Parameter)), function(x) {
        cols <- split(x$value, x$Group)
        n <- max(lengths(cols))
        padded <- lapply(cols, function(v) c(v, rep(NA_real_, n - length(v))))
        data.frame(padded, check.names = FALSE)
      })
      names(tabs) <- gsub("%", "pct", names(tabs), fixed = TRUE)
      getExportedValue("pzfx", "write_pzfx")(tabs, file, row_names = FALSE)
    })

  # ---- methods text ------------------------------------------------------------
  output$methods <- renderUI({
    HTML("
    <div style='max-width:820px; line-height:1.5;'>
    <h4>Input</h4>
    <p>The app reads the <b>Rate</b> sheet of a Seahorse Wave / Report Generator .xlsx export
    (or an equivalent .csv) with columns <code>Measurement, Well, Group, Time, OCR, ECAR, PER</code>.
    Rates are used as exported, so any background correction applied in Wave is retained.
    Groups named 'Background' are unticked by default. Time is assumed to be in minutes.</p>
    <p>Each assay has four phases (baseline plus one after each injection). Set how many
    measurements belong to each phase; the default splits the run into four equal parts.</p>

    <h4>Mito Stress Test (OCR), Agilent definitions</h4>
    <ul>
    <li><b>Non-mitochondrial respiration</b> = minimum rate after rotenone/antimycin A</li>
    <li><b>Basal respiration</b> = last baseline rate &minus; non-mitochondrial</li>
    <li><b>Proton leak</b> = minimum rate after oligomycin &minus; non-mitochondrial</li>
    <li><b>ATP-linked respiration</b> = last baseline rate &minus; minimum rate after oligomycin</li>
    <li><b>Maximal respiration</b> = maximum rate after FCCP &minus; non-mitochondrial</li>
    <li><b>Spare respiratory capacity</b> = maximal &minus; basal</li>
    <li><b>Spare respiratory capacity (%)</b> = maximal / basal &times; 100</li>
    <li><b>Coupling efficiency (%)</b> = ATP-linked / basal &times; 100</li>
    </ul>

    <h4>Glyco Stress Test (ECAR or PER), Agilent definitions</h4>
    <ul>
    <li><b>Non-glycolytic acidification</b> = last rate before glucose</li>
    <li><b>Glycolysis</b> = maximum rate before oligomycin &minus; non-glycolytic</li>
    <li><b>Glycolytic capacity</b> = maximum rate after oligomycin &minus; non-glycolytic</li>
    <li><b>Glycolytic reserve</b> = capacity &minus; glycolysis</li>
    <li><b>Glycolytic reserve (%)</b> = capacity / glycolysis &times; 100</li>
    </ul>

    <p>'Phase means' replaces every last / min / max above with the mean of the
    relevant phase, which is less sensitive to a single noisy measurement but
    underestimates peaks such as FCCP-stimulated respiration.</p>
    <p>Ratios return NA when the denominator is zero or negative.</p>

    <h4>Normalisation</h4>
    <p>Optional. Upload a two-column table (Well, value) and every rate is divided by
    that well's value before parameters are calculated. Wells missing from the table
    or with a value &le; 0 become NA.</p>
    </div>")
  })
}

shinyApp(ui, server)
