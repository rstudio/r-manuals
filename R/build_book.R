
# Build each book --------------


href <- function(href, text) {
  list(href = href, text = text)
}


appendices <- function(manual, manual_path = "manuals") {
  texi <- fs::dir_ls(fs::path(manual_path, manual, "data"),
                     regexp = glue::glue("(?i){manual}.texi"), perl = TRUE)
  z <- read_lines(texi) %>%
    stringr::str_extract("^@appendix (.*)$") %>%
    na.omit() %>%
    c() %>%
    # replace all punctuation except hyphen
    gsub(r"([!\"#$%&'()*+,./:;<=>?@[\\]^_`\{|\}~])", "", .) %>%
    gsub("@appendix ", "", .) %>%
    gsub("[[:space:]]", "-", .)

  if (length(z)) {
    paste0(z, ".md")
  } else {
    z
  }
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

  manual_name <- manual
  manual <- glue::glue("manuals/{manual}/prep/index.html", manual = manual)


  all_chapters <-
    xml2::read_html(manual) %>%
    xml_find_all("//div/ul/li/a") %>%
    xml_attr("href") %>%
    sub("(.*)#.*", "\\1", .) %>%
    c("index.html", .) %>%
    sub(".html$", ".md", .) %>%
    gsub("_002d", "-", .)

  appendices <- appendices(manual_name, manual_path = "manuals")

  chapters <- setdiff(all_chapters, appendices)

  title <- extract_title_from_index(manual)

  appendices <- if (length(appendices)) {
    appx <- paste0(paste("      -", appendices), collapse = "\n")
    paste0("  appendices:\n", appx, collapse = "\n")
  } else {
    character()
  }




  glue::glue(
    template,
    chapters   = paste0(paste("      -", chapters), collapse = "\n"),
    appendices = appendices,
    title = title
  )
}


#' Build a book
#'
#' @param x manual name
#' @param index index file
#' @param all_manuals Used to make the navbar, i.e. cross-referencing all the other books
#'
#' @keywords Internal
#' @return A modified yaml object
#'
update_quarto_yaml <- function(x, index, all_manuals, verbose = FALSE) {

  all_manuals <- basename(all_manuals)
  book <- fs::path(x, "book")
  yaml_file <- fs::path(book, "_quarto.yml")
  manual <- fs::path_file(x)


  glue_quarto_yaml(manual = manual, verbose = verbose) %>%
    readr::write_lines(file = glue::glue("{x}/book/_quarto.yml"))

  index2 <-
    # gsub("index.html", "index.md", index)
    gsub(glue::glue("../{name}/index.html", name = manual), "index.md", index)

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
  navbar <- navbar[all_manuals]
  navbar <- unname(navbar)
  navbar <- navbar[!vapply(navbar, is.null, FUN.VALUE = logical(1))]



  yaml <- yaml::read_yaml(yaml_file)
  yaml$book$navbar$right <- c(
    list(list(href = "../index.html", text = "Home")),
    list(list(text = "Manuals", menu = navbar)),
    list(list(href = "../about.html", text = "About"))
  )
  appendices <- yaml$book$appendices
  yaml$book$appendices <- if(length(appendices) == 1) {
    list(appendices)
  } else {
    appendices
  }

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
#' @return NULL
#' @export
#'
build_books <- function(manuals_folder = "manuals", manuals, all_manuals, verbose = TRUE) {
  if (!missing(manuals) && !is.null(manuals)) {
    manuals <- tolower(gsub("\\.texi", "", manuals))
    manuals <- file.path(manuals_folder, manuals)
  } else {
    manuals <- fs::dir_ls(manuals_folder)
  }
  all_manuals <- tolower(all_manuals)

  cli::cli_h2("Building book...")


  index <- fs::path(manuals_folder, fs::path_file(all_manuals), "prep/index.html")
  names(index) <- fs::path_file(all_manuals)


  purrr::walk(manuals, update_quarto_yaml, index = index, all_manuals = all_manuals, verbose = verbose)

  purrr::walk(manuals, ~ {
    cli::cli_progress_step("Building book {.file {.x}}.\n")
    message()
    quarto::quarto_render(fs::path(.x, "book"), as_job = FALSE)
  })

  invisible(NULL)
}

