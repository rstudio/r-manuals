utils::globalVariables(c("."))


#' @importFrom xml2 xml_find_all read_html xml_attr xml_text
extract_title_from_index <- function(filename) {
  xml2::read_html(filename) %>%
    xml_find_all("//h1") %>%
    xml_text() %>%
    .[1]
}

#' Glue elements intro quarto yaml file.
#'
#' @param manual Name of the manual to download and process (a `.texi`) file
#' @param template Location of _quarto.yml
#' @param verbose If TRUE, prints progress messages
#'
#' @keywords Internal
#' @noRd
#'
#' @examples
#' glue_quarto_yaml("r-exts")
#' glue_quarto_yaml("r-intro")
glue_quarto_yaml <- function(
  manual = "r-exts",
  template = "book_template/_quarto.yml",
  verbose = FALSE
) {

  if (verbose) cli::cli_progress_step("Glueing title and chapters into _quarto.yml")
  template <-
    readr::read_lines(template) %>%
    paste(collapse = "\n")

  manual <- glue::glue("manuals/{manual}/prep/index.html", manual = manual)

  chapters <-
    xml2::read_html(manual) %>%
    xml_find_all("//div/ul/li/a") %>%
    xml_attr("href") %>%
    sub("(.*)#.*", "\\1", .) %>%
    c("index.html", .) %>%
    sub(".html$", ".md", .) %>%
    gsub("_002d", "-", .)

  title <- extract_title_from_index(manual)

  glue::glue(
    template,
    chapters = paste0(paste("      -", chapters), collapse = "\n"),
    title = title
  )
}

