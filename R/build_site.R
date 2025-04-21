#' Build main website with all books.
#'
#' @param manuals_folder Defaults to "manuals"
#'
#' @return Path to rendered site
#' @export
build_main_website <- function(manuals_folder = "manuals", all_manuals) {
  cli::cli_h2("Building website")
  manuals <- all_manuals
  # manuals <- fs::dir_ls(manuals_folder)
  # copy rendered book to books folder
  books <- fs::path(manuals, "docs")

  # Copy style
  fs::file_copy("book_template/custom.scss", "website/", overwrite = TRUE)

  site_output <- "website/_site"
  unlink(site_output, recursive = TRUE)
  fs::dir_create(site_output)

  cli::cli_progress_step("Tweaking {.file .quarto.yml} file")
  yaml <- yaml::read_yaml("website/_quarto.yml")

  index <- purrr::map(
    books,
    ~ {
      manual <- fs::path_file(fs::path_dir(.x))
      list(
        href = fs::path(manual, "index.html"),
        text = extract_title_from_index(
          glue::glue("manuals/{manual}/prep/index.html", manual = manual)
        )
      )
    }
  )

  yaml$website$navbar$right <- c(
    list(list(href = "index.qmd", text = "Home")),
    list(list(text = "Manuals", menu = unname(index))),
    list(list(href = "about.qmd", text = "About"))
  )

  yaml::write_yaml(yaml, "website/_quarto.yml")
  ## Fix YAML
  xfun::gsub_file("website/_quarto.yml", "\\syes\\s*$", " true")
  xfun::gsub_file("website/_quarto.yml", "\\sno\\s*$", " false")

  cli::cli_progress_step("Running quarto to build website")
  res <- quarto::quarto_render("website", as_job = FALSE, quiet = FALSE)

  # Move books after quarto render because it will now clean output folder
  # `_site` by default
  purrr::walk(
    books,
    ~ {
      new_path <- fs::path(site_output, fs::path_file(fs::path_dir(.x)))
      from_path <- fs::path(manuals_folder, .x)
      cli::cli_progress_step("Moving book from {from_path} to {new_path}")
      if (!dir.exists(from_path)) {
        cli::cli_alert_info("Dir {from_path} doesn't exist")
        stop("Dir doesn't exist")
      }
      fs::dir_create(new_path)
      fs::dir_copy(from_path, new_path, overwrite = TRUE)
    }
  )

  cli::cli_progress_step("Done.")
  invisible(res)
}
