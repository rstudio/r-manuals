on_ci <- isTRUE(as.logical(Sys.getenv("CI")))

devtools::load_all()

##### Process Manuals ----------------
#
# This will download source file and create the intermediate structure and files to build the book
#
# folder structure:
#
# - manuals
#   - r-exts
#     - data
#     - prep
#     - books
#     - docs
#   - r-intro
#     - data
#     - prep
#     - books
#     - docs
#   - etc.
#
# - books
#   - r-intro
#   - r-data
#   - r-admin
#   - r-exts
#   - r-lang
#   - r-ints
#

# Determine the correct sequence for navbar links, etc.
all_manuals <- c(
  "r-intro",
  "r-data",
  "r-admin",
  "r-exts",
  "r-lang",
  "r-ints"
)

# Determine which manuals to build.
# Comment out some of these lines for quick testing.
# For production, should be identical to all_manuals.
manuals <- c(
  "R-intro.texi",
  "R-data.texi",
  "R-admin.texi",
  "R-exts.texi",
  "R-lang.texi",
  "R-ints.texi",
  NULL
)

# purrr::walk(manuals, process_manual, .quicktest = TRUE)
purrr::walk(manuals, process_manual)


# Build each book --------------

build_a_book <- function(x, index, all_manuals) {
  all_manuals <- basename(all_manuals)
  book <- fs::path(x, "book")
  yaml_file <- fs::path(book, "_quarto.yml")
  manual <- fs::path_file(x)


  index2 <-
    gsub(glue::glue("../{name}/index.html", name = manual), "index.md", index)
  yaml <- yaml::read_yaml(yaml_file)

  navbar <- purrr::imap(index2, ~ {
    list(
      href = .x,
      text = extract_title_from_index(glue::glue("manuals/{name}/prep/index.html", name = .y))
    )
  })

navbar <- navbar[all_manuals]
  
  yaml$book$navbar$right <- c(
    list(list(href = "../index.html", text = "Home")),
    list(list(text = "Manuals", menu = unname(navbar))),
    list(list(href = "../about.html", text = "About"))
  )
  yaml::write_yaml(yaml, yaml_file)
  xfun::gsub_file(yaml_file, "\\syes\\s*$", " true")
  xfun::gsub_file(yaml_file, "\\sno\\s*$", " false")
}

#
# This will modify the _quarto.yml navbar to include all manuals and then
# build each manual as a Quarto book project
build_books <- function(manuals_folder = "manuals", manuals, all_manuals) {
  if (!missing(manuals) && !is.null(manuals)) {
    manuals <- tolower(gsub("\\.texi", "", manuals))
    manuals <- file.path(manuals_folder, manuals)
  } else {
    manuals <- fs::dir_ls(manuals_folder)
  }

  index <- fs::path("..", fs::path_file(manuals), "index.html")
  names(index) <- fs::path_file(manuals)

  cli::cli_h2("Tweaking Manuals...")

  purrr::walk(manuals, build_a_book, index = index, all_manuals = all_manuals)

  purrr::walk(manuals, ~ {
    cli::cli_alert_info("Building book {.file {.x}}.")
    quarto::quarto_render(fs::path(.x, "book"), as_job = FALSE)
  })
}

build_books(manuals = manuals, all_manuals = all_manuals)

# Build website --------------
#
# This will build the main website with a shared navbar with all the book
#
# website is in `website` folder and will be build in `website/_site`

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

  purrr::walk(books, ~ {
    fs::dir_copy(
      .x,
      fs::path(site_output, fs::path_file(fs::path_dir(.x))),
      overwrite = TRUE
    )
  })

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
  quarto::quarto_render("website", as_job = FALSE)
}

build_main_website()

if (!on_ci) servr::httd("website/_site")