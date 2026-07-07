# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

This is an R package (`rmanuals`) whose purpose is to convert the [official R manuals](https://cran.r-project.org/manuals.html) from their source `texinfo` format into a lightly re-styled Quarto website (published at https://rstudio.github.io/r-manuals/). It is a build/conversion toolchain, not a library consumed by others at runtime.

The six manuals processed (in navbar order) are: `r-intro`, `r-data`, `r-admin`, `r-exts`, `r-lang`, `r-ints`. The source `.texi` files are pulled from the SVN-mirror repo `https://github.com/wch/r-source` (folder `doc/manual`).

## Commands

```r
# Full build (download → makeinfo → pandoc → quarto). This is what CI runs.
Rscript scripts/build_website.R

# Interactive development
pkgload::load_all()                     # load package functions
process_manual("R-intro.texi")          # download + convert one manual to markdown
process_manual("R-intro.texi", .quicktest = TRUE)  # skip download/makeinfo, limit to 9 md files

# Tests (testthat 3e)
Rscript -e 'devtools::test()'
Rscript -e 'testthat::test_file("tests/testthat/test-1-process-manual.R")'

# Regenerate NAMESPACE/man from roxygen after editing R/ docs
Rscript -e 'devtools::document()'
```

Regenerate `README.md` by knitting `README.Rmd` (never edit `README.md` directly).

## System dependencies

- **`makeinfo`** (from `texinfo`) — required; `stop_if_makeinfo_not_installed()` guards every entry point. Install with `sudo apt install texinfo` on Linux; on Windows it ships with RTools40 and is invoked via `perl`.
- **`pandoc`** — used via `rmarkdown::pandoc_convert`.
- **`quarto`** — used via the `quarto` R package.
- Package dependencies are pinned with **renv** (`renv.lock`); `.Rprofile` activates it.

## Pipeline architecture

The conversion is a staged pipeline. Each manual gets its own working tree under `manuals/<manual>/` with four sub-folders that map directly to pipeline stages:

- `data/` — downloaded `.texi` source + images + generated `version.texi`
- `prep/` — intermediate HTML from `makeinfo`, then `.md` from pandoc
- `book/` — assembled Quarto book input (`.md` + `_quarto.yml` + `custom.scss`)
- `docs/` — rendered book HTML (Quarto `output-dir`)

The orchestration in `scripts/build_website.R` runs three phases in order:

1. **`process_manual()`** (`R/process_manual.R`) — the per-manual driver. For each manual it calls, in sequence:
   - `download_manuals()` (`R/download_manuals.R`) — fetches `.texi`, `R-defs.texi`, referenced images, and builds `version.texi`. Downloads are cached for 1 hour (`already_downloaded()`).
   - `pre_process_manuals()` — strips `@group`/`@end group` (they break HTML `<pre>` blocks).
   - `make_info()` (`R/make_info.R`) — runs `makeinfo` to split `.texi` into per-chapter HTML.
   - `convert_to_md()` → `convert_html_to_md()` (`R/convert.R`) — pandoc HTML→markdown with Lua filters, then `regex_replace_md()` applies a large set of regex cleanups to fix conversion artifacts (footnotes, definition lists, code fences, etc.).

2. **`build_books()`** (`R/build_book.R`) — for each manual, generates its `book/_quarto.yml` from `book_template/_quarto.yml` by (a) reading chapter order and title out of the makeinfo `prep/index.html`, (b) separating appendices (parsed from `@appendix` lines in the `.texi`), and (c) building a cross-manual navbar. Then renders each book with `quarto::quarto_render`.

3. **`build_main_website()`** (`R/build_site.R`) — renders the top-level `website/` project, then copies each manual's rendered `docs/` into `website/_site/<manual>/`. Final output is `website/_site`.

### Key cross-cutting details

- **Templates are the source of truth for styling/nav:** `book_template/_quarto.yml` and `book_template/custom.scss` are copied into each book; `website/_quarto.yml` drives the top site. The R code programmatically rewrites the `navbar` and `chapters`/`appendices` in these YAML files at build time.
- **YAML boolean fixups:** after `yaml::write_yaml`, both build steps run `xfun::gsub_file` to convert ` yes`/` no` back to ` true`/` false` (the `yaml` package serializes logicals as yes/no, which Quarto misreads).
- **File renaming:** makeinfo emits `_002d` in filenames where the manual has `-`; these are renamed to `-` (see `process_manual.R` and `glue_quarto_yaml`).
- **Lua filters** live in `inst/rmarkdown/lua/` (`filter.lua`, `index.lua`) and are located at runtime via `rmarkdown::pkg_file_lua(..., package = "rmanuals")`. `index.lua` is only applied to `index.html`.
- **`extract_title_from_index()`** (`R/utils.R`) reads the manual title from the first `<h1>` of `prep/index.html` — this drives book titles and navbar text throughout.

## Tests

`tests/testthat/` contains a self-contained fixture manual (`manuals/test/data/test.texi` + `R-defs.texi` + `version.texi`) and its own `book_template/`. Tests run `process_manual("test.texi", .quicktest = TRUE)`. `in_testthat()` in `R/utils.R` sets the working directory to `tests/testthat/` for interactive test debugging.

## CI

`.github/workflows/build-website.yaml` runs `scripts/build_website.R` on push/PR to `main`, weekly (Sunday cron, to track upstream R changes), and manually. On non-PR events it deploys `website/_site` to the `gh-pages` branch. It installs pandoc explicitly and restores the renv library.

## Licensing note

The conversion code (R scripts + Lua filters) is MIT. The R manuals themselves carry the R Core Team's own verbatim/modified-copy permission notice — do not relicense manual content.
