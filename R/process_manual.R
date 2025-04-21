#' Process manual source file to prepare book to build.
#'
#' Downloads, pre-processes and converts the source `.texi` files into the necessary
#' structure and files are there to build the book with Quarto
#'
#' @param manual Name of the manual to download and process (a `.texi`) file
#'
#' @param folder sub-folder for intermediaries files per manual. Default to name
#'   of manual
#'
#' @param manuals_folder folder where all the intermediary folders are stored.
#'   `manuals_folder` is the parent `folder`.
#'
#' @param .make_info If TRUE, convert `.texi` to `.html`
#'
#' @param .download If TRUE, download the manual files
#'
#' @param .quicktest If TRUE, removes some of the HTML files after `makeinfo`
#'   step, for faster quarto rendering on partial site
#'
#' @param verbose If TRUE, prints progress messages
#'
#' @return NULL.
#' @export
process_manual <- function(
  manual = "R-exts.texi",
  folder = tolower(sub(".texi", "", manual)),
  manuals_folder = "manuals",
  .quicktest = FALSE,
  .make_info = !.quicktest,
  .download = !.quicktest,
  verbose = TRUE
) {
  cli::cli_h2("Processing {manual}")

  data_folder <- glue::glue("{manuals_folder}/{folder}/data")
  prep_folder <- glue::glue("{manuals_folder}/{folder}/prep")
  book_folder <- glue::glue("{manuals_folder}/{folder}/book")
  docs_folder <- glue::glue("{manuals_folder}/{folder}/docs")

  # Download the .texi files from github

  downloads_empty <- !fs::dir_exists(data_folder) ||
    length(fs::dir_ls(data_folder)) == 0

  if (downloads_empty || .download) {
    download_manuals(manual = manual, destdir = data_folder)
  }

  # Compile the .texi to HTML
  if (.make_info) {
    # pre-process texi
    pre_process_manuals(data_folder)

    # create folder for intermediate output
    if (fs::dir_exists(prep_folder)) fs::dir_delete(prep_folder)
    fs::dir_create(prep_folder)
    make_info(
      manual,
      input_dir = data_folder,
      output_dir = prep_folder,
      verbose = verbose
    )

    # Rename files containing "_002d" to "-"
    fs::dir_ls(prep_folder, glob = "*_002d*") %>%
      fs::file_move(new_path = gsub("_002d", "-", .))
  } else {
    fs::dir_create(prep_folder)
  }

  # convert to markdown and fix conversion errors
  convert_to_md(path = prep_folder, verbose = verbose)

  fs::dir_create(book_folder)
  # delete all markdown files in book folder
  fs::dir_ls(book_folder, glob = "*.md") %>% fs::file_delete()
  fs::dir_ls(book_folder, glob = "*.html") %>% fs::file_delete()

  # copy markdown files to book folder
  md_files <- fs::dir_ls(prep_folder, glob = "*.md")
  if (.quicktest) {
    n_max <- min(length(md_files), 9)
    if (n_max > 0) {
      md_files[1:n_max] %>%
        fs::file_copy(book_folder, overwrite = TRUE)
    }
  } else {
    md_files %>%
      fs::file_copy(book_folder, overwrite = TRUE)
  }

  # copy images if present
  images_present <- dir_ls(data_folder, glob = "*images")
  if (length(images_present)) {
    fs::dir_copy(
      glue::glue("{data_folder}/images"),
      glue::glue("{book_folder}/images"),
      overwrite = TRUE
    )
  }

  # replace regular expressions
  regex_replace_md(path = book_folder)

  # copy template files to book folder
  fs::file_copy("book_template/custom.scss", book_folder, overwrite = TRUE)
  invisible()
}
