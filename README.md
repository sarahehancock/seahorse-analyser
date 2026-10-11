# Seahorse XF analyser (Mito & Glyco Stress Test)

A Shiny app that reads a Seahorse Wave export (`.xlsx`, "Rate" or "Normalized Rate" sheet, or an
equivalent `.csv`), calculates the Mito Stress Test and Glycolysis Stress Test parameters per well,
and plots/exports them. It supports plates where half the wells ran Mito and half ran Glyco.

It is deployed with [shinylive](https://posit-dev.github.io/r-shinylive/), so it runs **entirely in
your browser** (R is compiled to WebAssembly). There is no server, and uploaded data never leaves
your computer.

**Live app:** `https://<your-github-username>.github.io/<repo-name>/`

## Repository layout

```
app.R                          the whole app (must sit at the repo root)
.github/workflows/deploy.yml   builds + publishes to GitHub Pages on every push to main
README.md
.gitignore
```

## One-time setup

1. **Create the repo.** On GitHub: New repository, e.g. `seahorse-analyser`. It must be **public**
   (GitHub Pages is not available on private repos for free accounts). Leave it empty.
2. **Push these files** (from the folder containing `app.R`):
   ```bash
   git init -b main
   git add .
   git commit -m "Seahorse analyser"
   git remote add origin https://github.com/<your-github-username>/<repo-name>.git
   git push -u origin main
   ```
   Make sure the hidden `.github` folder is included. If you upload through the GitHub web page
   instead, use *Add file > Create new file*, type `.github/workflows/deploy.yml` as the name and
   paste the file contents.
3. **Switch on Pages from Actions.** Repo *Settings > Pages > Build and deployment > Source:
   GitHub Actions*. (Do this before, or immediately after, the first push; the first deploy fails
   if Pages is not enabled. If it already failed, re-run it from the Actions tab.)
4. **Wait for the build.** *Actions* tab > "Deploy app to GitHub Pages". The first run takes a few
   minutes. When both jobs are green the app is live at the URL above (also shown on the deploy job).

## Updating the app

Edit `app.R`, commit and push to `main`. The workflow rebuilds and redeploys automatically.

## Testing the browser build on your own machine (optional)

```r
# install.packages("shinylive"); install.packages("httpuv")
unlink("site", recursive = TRUE)           # remove any old export first
shinylive::export(".", "site")
httpuv::runStaticServer("site/")           # then open the URL it prints
```

You can also run the plain app without shinylive: `shiny::runApp(".")`.

## Things to know about the browser version

- **First load is slow** (tens of MB of WebAssembly; roughly 30-60 s on a normal connection),
  faster afterwards because the browser caches it. Use a current Chrome, Edge, Firefox or Safari.
- **Package availability:** shinylive can only use packages that exist as WebAssembly builds on
  <https://repo.r-wasm.org>. This app needs `shiny, readxl, dplyr, tidyr, tibble, ggplot2, DT`.
  If the app fails at startup with a message about a package, check that package there.
- **Optional extras:** the `ggprism` theme is fetched on demand if available, otherwise the app
  falls back to the Classic theme. The `.pzfx` export is only offered in desktop R; in the browser
  use "Prism-ready CSV" (one block per parameter, one column per group, paste into Prism).
- **Large files** work but are slower than desktop R; a typical Seahorse export is fine.
- Everything runs in one browser tab's memory: refreshing the page clears the loaded data.
