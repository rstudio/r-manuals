already_downloaded <- function(file) {
  fs::file_exists(file) &&
    difftime(
      Sys.time(),
      fs::file_info(file)$modification_time,
      units = "hours"
    ) <
      1
}

# The wch/r-source GitHub mirror of the R SVN repository.
R_SOURCE_REPO <- "https://github.com/wch/r-source"

# Used only as a fallback if `latest_r_release_ref()` cannot reach the network.
R_RELEASE_REF_FALLBACK <- "tags/R-4-5-1"

#' Turn an R source ref into the raw.githubusercontent base URL.
#'
#' @param r_ref A ref in the wch/r-source mirror. Examples:
#'   `"trunk"` (R-devel), `"R-4-6-branch"` (latest patched 4.6.x),
#'   `"tags/R-4-6-1"` (exact 4.6.1 release).
#' @noRd
r_source_base_url <- function(r_ref = "trunk") {
  glue::glue(
    "https://raw.githubusercontent.com/wch/r-source/refs/heads/{r_ref}"
  )
}

#' Determine the ref of the latest released version of R.
#'
#' Queries the wch/r-source mirror for release tags (stored as branches named
#' `tags/R-x-y-z`) and returns the highest version, e.g. `"tags/R-4-6-1"`.
#' Memoised for the duration of the session. Falls back to
#' [R_RELEASE_REF_FALLBACK] (with a warning) if `git ls-remote` fails.
#'
#' @noRd
latest_r_release_ref <- local({
  cached <- NULL
  function(repo = R_SOURCE_REPO) {
    if (!is.null(cached)) return(cached)
    result <- tryCatch(
      {
        refs <- system2(
          "git",
          c("ls-remote", "--heads", repo, "refs/heads/tags/R-*"),
          stdout = TRUE,
          stderr = FALSE
        )
        tags <- sub(".*refs/heads/tags/(R-[0-9-]+)$", "\\1", refs)
        tags <- grep("^R-[0-9]+-[0-9]+-[0-9]+$", tags, value = TRUE)
        if (length(tags) == 0) stop("no release tags found")
        ver <- numeric_version(gsub("-", ".", sub("^R-", "", tags)))
        paste0("tags/", tags[order(ver)][length(tags)])
      },
      error = function(e) {
        cli::cli_warn(c(
          "Could not detect the latest R release ref: {conditionMessage(e)}",
          i = "Falling back to {.val {R_RELEASE_REF_FALLBACK}}."
        ))
        R_RELEASE_REF_FALLBACK
      }
    )
    cached <<- result
    result
  }
})

#' Download manual files from a given URL
#'
#' @param filename of manual file (also the filename). This gets appended to `base_url`
#' @param destdir Destination directory
#' @param base_url Base URL
#'
#' @return Downloads the file.
#' @importFrom magrittr `%>%`
#' @importFrom utils download.file
#'
#' @noRd
#'
download_manual_file <- function(
  filename,
  destdir = ".",
  base_url = "https://raw.githubusercontent.com/wch/r-source/refs/heads/trunk"
) {
  output_file <- glue::glue("{destdir}/{filename}")
  if (already_downloaded(output_file)) return(invisible())

  base_url <- paste0(base_url, "/doc/manual")
  download.file(
    glue::glue("{base_url}/{filename}"),
    destfile = output_file,
    quiet = TRUE
  )
}

download_manual_images <- function(
  filename,
  destdir = ".",
  base_url = "https://raw.githubusercontent.com/wch/r-source/refs/heads/trunk"
) {
  output_file <- glue::glue("{destdir}/images/{filename}")

  if (already_downloaded(output_file)) return(invisible())

  # base_url <- paste0(base_url, "/doc/manual/images")
  download.file(
    glue::glue("{base_url}/doc/manual/images/{filename}"),
    destfile = output_file,
    quiet = TRUE,
    mode = "wb"
  )
}


#' @importFrom brio read_lines write_lines
#' @importFrom stringr str_c
#' @noRd
create_version_file <- function(
  destdir = "data",
  base_url = "https://raw.githubusercontent.com/wch/r-source/refs/heads/trunk"
) {
  output_file <- glue::glue("{destdir}/version.texi")
  if (already_downloaded(output_file)) return(invisible())

  filename <- "VERSION"
  download.file(
    glue::glue("{base_url}/{filename}"),
    destfile = glue::glue("{destdir}/{filename}"),
    quiet = TRUE
  )

  # The VERSION file holds a single line, e.g.
  #   "4.6.1"                                  (a released version)
  #   "4.5.3 Patched"                          (a release branch)
  #   "4.7.0 Under development (unstable)"     (trunk / R-devel)
  version_raw <- read_lines(glue::glue("{destdir}/VERSION"))[[1]]
  # Drop any trailing parenthetical, keeping the "Under development"/"Patched"
  # status so the rendered manual matches upstream.
  version <- trimws(gsub("\\s*\\(.*", "", version_raw))
  # RWVERSION reflects the *target* R version (from the source), not the R that
  # happens to be running this build.
  version_number <- sub("^([0-9.]+).*", "\\1", version_raw)
  rwversion <- paste0("R-", version_number)
  # RWTVERSION is RWVERSION with its final dot-component stripped (as upstream's
  # doc/manual/Makefile does), e.g. "R-4.6.1" -> "R-4.6". It feeds the Windows
  # "howto-<RWTVERSION>.html" links in R-admin; without it makeinfo warns
  # "undefined flag: RWTVERSION".
  rwtversion <- sub("\\.[^.]*$", "", rwversion)
  version_template <- str_c(
    "@set VERSION {version}\n",
    "@set VERSIONno {version}\n",
    "@set RWVERSION {rwversion}\n",
    "@set RWTVERSION {rwtversion}",
    collapse = "\n"
  )
  glue::glue(
    version_template,
    version = version,
    rwversion = rwversion,
    rwtversion = rwtversion
  ) %>%
    write_lines(path = output_file)
}


#' Download manuals
#'
#' @param manual Filename of manual to download. This should be the `.texi` file, e.g. `R-exts.texi`
#' @param destdir Destination directory
#' @param r_ref Ref in the wch/r-source mirror to download from. `NULL` (the
#'   default) resolves to the latest released version of R. Use `"trunk"` for
#'   R-devel, a release branch like `"R-4-6-branch"`, or an exact tag like
#'   `"tags/R-4-6-1"`.
#'
#' @importFrom stats na.omit
#' @return downloads the file
#' @export
#'
download_manuals <- function(
  manual = "R-exts.texi",
  destdir = "data",
  r_ref = NULL
) {
  if (is.null(r_ref)) r_ref <- latest_r_release_ref()
  base_url <- r_source_base_url(r_ref)

  # If the requested ref differs from a previous download, wipe the cache so we
  # don't mix sources (`already_downloaded()` keys on mtime only, not on ref).
  ref_marker <- glue::glue("{destdir}/.r_ref")
  if (
    fs::file_exists(ref_marker) &&
      !identical(read_lines(ref_marker)[[1]], as.character(r_ref)) &&
      fs::dir_exists(destdir)
  ) {
    cli::cli_alert_info("R ref changed to {.val {r_ref}}; clearing {.file {destdir}}")
    fs::dir_delete(destdir)
  }

  # download original texinfo file
  fs::dir_create(destdir)
  write_lines(as.character(r_ref), path = ref_marker)
  cli::cli_progress_step("Downloading {.file {manual}}")
  download_manual_file(manual, destdir = destdir, base_url = base_url)
  needed <- "R-defs.texi"
  cli::cli_progress_step("Downloading {.file {needed}}")
  download_manual_file(needed, destdir = destdir, base_url = base_url)

  image_names <-
    readr::read_lines(glue::glue("{destdir}/{manual}")) %>%
    stringr::str_match("@image\\{images/(.*?),.*\\}") %>%
    na.omit() %>%
    .[, 2]

  fs::dir_create(glue::glue("{destdir}/images"))
  for (i in seq_along(image_names)) {
    download_manual_images(
      paste0(image_names[i], ".png"),
      destdir = destdir,
      base_url = base_url
    )
  }

  # download VERSION file and create version.texi
  cli::cli_progress_step("Creating {.file version.texi}")
  create_version_file(base_url = base_url, destdir = destdir)
}


#' Pre-process the texi files.
#'
#' At the moment the only pre-processing is to remove `@group` statements.
#' In PDF format this ensures line breaks don't occur in the middle of code blocks,
#' but in HTML this causes multiple `pre` statements.
#'
#' @inheritParams download_manuals
pre_process_manuals <- function(
  destdir = "data"
) {
  cli::cli_progress_step("Pre-processing .texi files")
  fs::dir_ls(destdir, glob = "*.texi") %>%
    purrr::walk(function(x) {
      filename <- x
      content <- x %>%
        read_lines()
      group <- grep("^@(end )?group$", content)
      if (length(group) == 0) return(invisible(FALSE))
      write_lines(content[-group], path = filename)
      invisible(TRUE)
    })
}
