utils::globalVariables(c("."))


#' @importFrom xml2 xml_find_all read_html xml_attr xml_text xml_remove
extract_title_from_index <- function(filename) {
  h1 <- xml2::read_html(filename) %>%
    xml_find_all("//h1") %>%
    .[[1]]
  # makeinfo appends a copiable-link anchor (" ¶") inside the <h1>; drop it so
  # the pilcrow does not leak into the book/navbar/page titles.
  xml_find_all(h1, ".//a[@class='copiable-link']") %>%
    xml_remove()
  trimws(xml_text(h1))
}


#' Read a short R-version label for a manual from its `version.texi`.
#'
#' Parses the `@set VERSION` line written by [create_version_file()] and returns
#' a display label of the form `v<number>` for a released version
#' (e.g. `"v4.6.1"`), `v<number>-devel` for R-devel (e.g. `"v4.7.0-devel"`), or
#' `v<number>-patched` for a release branch. Returns `NA_character_` if the file
#' is missing so callers can fall back to a version-less title.
#' @noRd
extract_version_label <- function(version_file) {
  if (length(version_file) != 1 || !fs::file_exists(version_file)) {
    return(NA_character_)
  }
  version_line <- read_lines(version_file) %>%
    stringr::str_subset("^@set VERSION ")
  if (length(version_line) == 0) return(NA_character_)
  version_raw <- trimws(sub("^@set VERSION\\s+", "", version_line[[1]]))
  number <- sub("^([0-9.]+).*", "\\1", version_raw)
  label <- if (grepl("Under development", version_raw, ignore.case = TRUE)) {
    paste0(number, "-devel")
  } else if (grepl("Patched", version_raw, ignore.case = TRUE)) {
    paste0(number, "-patched")
  } else {
    number
  }
  paste0("v", label)
}


in_testthat <- function(code) {
  withr::with_dir(here::here("tests/testthat/"), code)
}
