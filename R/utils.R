utils::globalVariables(c("."))


#' @importFrom xml2 xml_find_all read_html xml_attr xml_text
extract_title_from_index <- function(filename) {
  xml2::read_html(filename) %>%
    xml_find_all("//h1") %>%
    xml_text() %>%
    .[1]
}


in_testthat <- function(code) {
  withr::with_dir(here::here("tests/testthat/"), code)
}
