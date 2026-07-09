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


in_testthat <- function(code) {
  withr::with_dir(here::here("tests/testthat/"), code)
}
