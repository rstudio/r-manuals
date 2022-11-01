
# Build each book --------------


href <- function(href, text) {
  list(href = href, text = text)
}

#' Build a book
#'
#' @param x
#' @param index
#' @param all_manuals Used to make the navbar, i.e. cross-referencing all the other books
#'
#' @keywords Internal
#' @return
#'
update_quarto_yaml <- function(x, index, all_manuals) {
  all_manuals <- basename(all_manuals)
  book <- fs::path(x, "book")
  yaml_file <- fs::path(book, "_quarto.yml")
  manual <- fs::path_file(x)

  # browser()
  index2 <-
    # gsub("index.html", "index.md", index)
    gsub(glue::glue("../{name}/index.html", name = manual), "index.md", index)

  # navbar <-
  #   index2 %>%
  #   purrr::imap(
  #     ~ href(.x,
  #            extract_title_from_index(
  #              glue::glue("manuals/{name}/prep/index.html", name = .y)
  #            )
  #     ))

  navbar <-
    index2 %>%
    purrr::imap(
      ~ href(
        # fs::path(manual, "index.html")
        fs::path("../", glue::glue("{name}", name = .y), "index.html"),
             extract_title_from_index(
               glue::glue("manuals/{name}/prep/index.html", name = .y)
             )
      ))
  # browser()



  navbar <- navbar[all_manuals]
  # navbar <- navbar[!is.null(navbar)]
  navbar <- unname(navbar)
  navbar <- navbar[!vapply(navbar, is.null, FUN.VALUE = logical(1))]

  yaml <- yaml::read_yaml(yaml_file)
  yaml$book$navbar$right <- c(
    list(list(href = "../index.html", text = "Home")),
    list(list(text = "Manuals", menu = navbar)),
    list(list(href = "../about.html", text = "About"))
  )
  yaml::write_yaml(yaml, yaml_file)
  xfun::gsub_file(yaml_file, "\\syes\\s*$", " true")
  xfun::gsub_file(yaml_file, "\\sno\\s*$", " false")
}

# Build books ----------------
#
# This will modify the _quarto.yml navbar to include all manuals and then
# build each manual as a Quarto book project

#' Build books.
#'
#' @param manuals_folder Defaults to `manuals`
#' @param manuals Vector of manuals to build, e.g. `"R-intro.texi"`
#' @param all_manuals Vector of all manuals, e.g. `c("R-intro.texi", "R-data.texi")`
#'
#' @return
#' @export
#'
build_books <- function(manuals_folder = "manuals", manuals, all_manuals) {
  if (!missing(manuals) && !is.null(manuals)) {
    manuals <- tolower(gsub("\\.texi", "", manuals))
    manuals <- file.path(manuals_folder, manuals)
  } else {
    manuals <- fs::dir_ls(manuals_folder)
  }
  all_manuals <- tolower(all_manuals)

  cli::cli_h2("Building book...")

  # browser()
  index <- fs::path(manuals_folder, fs::path_file(all_manuals), "prep/index.html")
  names(index) <- fs::path_file(all_manuals)


  purrr::walk(manuals, update_quarto_yaml, index = index, all_manuals = all_manuals)

  purrr::walk(manuals, ~ {
    cli::cli_alert_info("Building book {.file {.x}}.")
    quarto::quarto_render(fs::path(.x, "book"), as_job = FALSE)
  })
}
