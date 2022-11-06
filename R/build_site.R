#' Build main website with all books.
#'
#' @param manuals_folder Defaults to "manuals"
#'
#' @return Path to rendered site
#' @export
build_main_website <- function(manuals_folder = "manuals") {
  cli::cli_h2("Building website")
  manuals <- fs::dir_ls(manuals_folder)
  # copy rendered book to books folder
  books <- fs::path(manuals, "docs")

  # Copy style
  fs::file_copy("book_template/custom.scss", "website/", overwrite = TRUE)

  site_output <- "website/_site"
  unlink(site_output, recursive = TRUE)

  cli::cli_alert_info("Tweaking {.file .quarto.yml} file")
  yaml <- yaml::read_yaml("website/_quarto.yml")

  index <- purrr::map(books, ~ {
    manual <- fs::path_file(fs::path_dir(.x))
    list(
      href = fs::path(manual, "index.html"),
      text = extract_title_from_index(
        glue::glue("manuals/{manual}/prep/index.html", manual = manual)
      )
    )
  })

  yaml$website$navbar$right <- c(
    list(list(href = "index.qmd", text = "Home")),
    list(list(text = "Manuals", menu = unname(index))),
    list(list(href = "about.qmd", text = "About"))
  )

  yaml::write_yaml(yaml, "website/_quarto.yml")
  ## Fix YAML
  xfun::gsub_file("website/_quarto.yml", "\\syes\\s*$", " true")
  xfun::gsub_file("website/_quarto.yml", "\\sno\\s*$", " false")

  cli::cli_alert_info("Running quarto to build website")
  res <- quarto::quarto_render("website", as_job = FALSE)

  # browser()

  # Move books after quarto render because it will now clean output folder `_site` by default
  purrr::walk(books, ~ {
    fs::dir_copy(
      .x,
      fs::path(site_output, fs::path_file(fs::path_dir(.x))),
      overwrite = TRUE
    )
  })

  cli::cli_alert_info("Done.")
  invisible(res)
}
