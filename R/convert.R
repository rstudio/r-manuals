#' Convert intermediate HTML to markdown, using a lua filter
#'
#' @param input_file Name of input file
#' @inheritParams process_manual
#' @return Invisible null - writes into a set of HTML files
#' @export
#'
#' @importFrom rmarkdown pandoc_convert
#'
convert_html_to_md <- function(input_file, verbose = FALSE) {

  input_file <- normalizePath(input_file)
  output_file <- gsub("\\.html$", ".md", input_file)

  temp_html <- tempfile(fileext = ".html")
  temp_md <- tempfile(fileext = ".md")

  make_lua_filter <- function(filter) {
    rmarkdown::pandoc_lua_filter_args(
      rmarkdown::pkg_file_lua(filter, package = "rmanuals")
    )
  }

  lua_filters <- make_lua_filter("filter.lua")

  if (basename(input_file) == "index.html") {
    book_title  <- extract_title_from_index(input_file)
    meta_data   <- glue::glue("--metadata=book_title:\"{book_title}\"")
    lua_filters <- c(make_lua_filter("index.lua"), lua_filters)
  } else {
    meta_data = character()
  }

  input_file %>%
    read_lines() %>%
    gsub("^<span .*?></span>", "", .) %>%
    write_lines(temp_html)


  # capture.output({
    pandoc_convert(
      temp_html,
      output = temp_md,
      from = "html",
      to = "markdown+definition_lists",
      options = c(
        meta_data,
        lua_filters,
        "--shift-heading-level-by=-1"
        ),
      verbose = verbose
    )
  # })

  temp_md %>%
    read_lines() %>%
    # remove empty hyperlinks [](...)
    gsub("^\\[\\]\\{.*?\\}$", "", .)  %>%
    # remove {.sample}
    gsub("\\{\\.sample\\}", "", .) %>%
    # fix footnote cross-references
    gsub("\\[\\^(\\d+)\\^\\]\\(#FOOT\\d+\\)\\{#DOCF\\d+\\}", "[^\\1]", .) %>%
    gsub("\\[\\((\\d+)\\)\\]\\(#DOCF\\d+\\)\\{#FOOT\\d+\\}", "[^\\1]:", .) %>%
    # Fix function definition lists
    gsub("^(Function: .*)\\\\$", "\\1\n:    \n", .) %>%
    # Fix indented codeblocks by adding a newline in front of them
    gsub("(^    ``` c)", "\n\\1", .) %>%
    # remove quote around backticks
    gsub("('`|`')", "`", .) %>%
    write_lines(path = output_file)

  # Remove Table of Contents heading from index.md
  if (basename(input_file) == "index.html") {
    read_lines(temp_md) %>%
      sub("^Table of Contents \\{.*\\}$", "", .) %>%
      write_lines(output_file)
  }

  invisible()

}



#' Convert all html files in a directory to markdown
#'
#' @param path Directory containing the HTML files
#' @inheritParams process_manual
#'
#' @return Invisible NULL - writes a set of markdown files
#' @export
#'
#' @importFrom dplyr progress_estimated
#' @importFrom purrr walk
#' @importFrom fs dir_ls
#'
convert_to_md <- function(path = "temp", verbose = FALSE) {
  file_list <- fs::dir_ls(path = path, glob = "*.html")

  if (verbose) cli::cli_progress_step("Converting HTML to markdown")

  # create progress bar
  withr::local_options(list(cli.progress_show_after = 0))
  cli::cli_progress_bar(
    total = length(file_list),
    extra = list(file = ""),
    format = "{cli::pb_spin} Converting {cli::pb_extra$file} {cli::pb_current}/{cli::pb_total}",
    clear = FALSE
  )

  for (i in seq_along(file_list)) {
    cli::cli_progress_update(extra = list(file = file_list[i]))
    convert_html_to_md(file_list[i])
  }
  cli::cli_progress_done()
  invisible()
}
