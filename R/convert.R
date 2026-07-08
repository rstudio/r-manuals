make_lua_filter <- function(filter) {
  rmarkdown::pandoc_lua_filter_args(
    rmarkdown::pkg_file_lua(filter, package = "rmanuals")
  )
}


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

  lua_filters <- make_lua_filter("filter.lua")

  if (basename(input_file) == "index.html") {
    book_title <- extract_title_from_index(input_file)
    meta_data <- glue::glue("--metadata=book_title:\"{book_title}\"")
    lua_filters <- c(lua_filters, make_lua_filter("index.lua"))
  } else {
    meta_data = character()
  }

  input_file %>%
    read_lines() %>%
    gsub("^<span .*?></span>", "", .) %>%
    write_lines(temp_html)

  pandoc_convert(
    temp_html,
    output = output_file,
    from = "html",
    to = "markdown+definition_lists",
    options = c(
      meta_data,
      lua_filters,
      "--shift-heading-level-by=-1"
    ),
    verbose = verbose
  )

  #

  # Remove Table of Contents heading from index.md
  if (basename(input_file) == "index.html") {
    read_lines(output_file) %>%
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

  # if (verbose) cli::cli_progress_step("Converting HTML to markdown")

  # create progress bar
  withr::local_options(list(cli.progress_show_after = 0))
  cli::cli_progress_bar(
    total = length(file_list),
    extra = list(file = ""),
    format = paste(
      "{cli::pb_spin} Converting {cli::pb_extra$file}",
      "{cli::pb_current}/{cli::pb_total} to markdown"
    ),
    clear = TRUE
  )

  for (i in seq_along(file_list)) {
    convert_html_to_md(file_list[i])
    cli::cli_progress_update(extra = list(file = file_list[i]))
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Converted all HTML files to markdown")
  invisible()
}


regex_replace <- function(x) {
  # Multibyte characters are built with intToUtf8() rather than written as
  # literals, so the patterns survive package loading in a non-UTF-8 locale
  # (raw UTF-8 source literals get transcoded to "<U+XXXX>" escape text there).
  ellipsis <- intToUtf8(0x2026) # …
  pilcrow  <- intToUtf8(0x00b6) # ¶

  # Ordered substitution rules applied to each line. Each rule is
  # list(pattern, replacement) plus an optional fixed = / perl = / useBytes =
  # flag. useBytes = TRUE makes multibyte patterns match regardless of locale.
  rules <- list(
    # remove empty hyperlinks [](...)
    list("^\\[\\]\\{.*?\\}$", ""),
    # remove {.variable}/{.sample} markers, with or without surrounding backticks
    list("`?\\{\\.(variable|sample)\\}`?", "", perl = TRUE),
    # fix footnote cross-references
    list("\\[\\^(\\d+)\\^\\]\\(#FOOT\\d+\\)\\{#DOCF\\d+\\}", "[^\\1]"),
    list("\\[\\((\\d+)\\)\\]\\(#DOCF\\d+\\)\\{#FOOT\\d+\\}", "[^\\1]"),
    # fix function definition lists
    list("^(Function: .*)\\\\$", "\\1\n:    \n"),
    list("^(`.*`)\\\\$", "\\1\n:    \n"),
    # fix indented codeblocks by adding a newline in front of them (4- or 8-space indent)
    list("(^\\s{4}(?:\\s{4})?``` [cR]$)", "\n\\1"),
    # remove quote around backticks
    list("('`|`')", "`"),
    # insert missing colon in footnote references
    list("^(\\[\\^\\d+\\])$", "\\1:"),
    # remove double backtick `` occurrences
    list(r"{(?<!`)``(?!`)}", "", perl = TRUE),
    # replace ellipsis (…); useBytes keeps this locale-independent
    list(ellipsis, "...", useBytes = TRUE),
    # remove named sections
    list("(^#+ .*?) {#.*? \\.(.*)}$", "\\1", perl = TRUE),
    # remove copiable-link pilcrow (¶) anchors, regardless of locale
    list(paste0("\\[", pilcrow, "\\]\\(.*?\\)\\{\\.copiable-link\\}"), "",
         perl = TRUE, useBytes = TRUE),
    # remember to remove this line and deal with it using Lua filters.
    list("^:::.*$", "")
  )

  for (rule in rules) {
    x <- gsub(
      rule[[1]], rule[[2]], x,
      fixed    = isTRUE(rule$fixed),
      perl     = isTRUE(rule$perl),
      useBytes = isTRUE(rule$useBytes)
    )
  }
  x
}

regex_replace_md <- function(path = "temp", verbose = TRUE) {
  file_list <- fs::dir_ls(path = path, glob = "*.md")
  # browser()

  if (verbose) cli::cli_progress_bar("Replacing regular expressions")

  # create progress bar
  for (filename in file_list) {
    filename %>%
      read_lines() %>%
      regex_replace() %>%
      readr::write_lines(file = filename)
  }
  invisible()
}
