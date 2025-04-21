already_downloaded <- function(file) {
  fs::file_exists(file) &&
    difftime(
      Sys.time(),
      fs::file_info(file)$modification_time,
      units = "hours"
    ) <
      1
}

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

  version <- read_lines(glue::glue("{destdir}/VERSION")) %>%
    gsub("\\([^ ]*\\).*", "\\1", .)
  rwversion <- paste0(
    "R-",
    R.version$major,
    ".",
    R.version$minor,
    tolower(R.version$status)
  )
  version_template <- str_c(
    "@set VERSION {version}\n",
    "@set VERSIONno {version}\n",
    "@set RWVERSION {rwversion}",
    collapse = "\n"
  )
  glue::glue(version_template, version = version, rwversion = rwversion) %>%
    write_lines(path = output_file)
}


#' Download manuals
#'
#' @param manual Filename of manual to download. This should be the `.texi` file, e.g. `R-exts.texi`
#' @param destdir Destination directory
#' @param base_url Base URL
#'
#' @importFrom stats na.omit
#' @return downloads the file
#' @export
#'
download_manuals <- function(
  manual = "R-exts.texi",
  destdir = "data",
  base_url = "https://raw.githubusercontent.com/wch/r-source/refs/heads/trunk"
) {
  # download original texinfo file
  fs::dir_create(destdir)
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
    download_manual_images(paste0(image_names[i], ".png"), destdir = destdir)
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
