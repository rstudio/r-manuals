# These tests pin the behaviour of regex_replace() so the substitution rules
# can be refactored safely. regex_replace() is an internal function.

test_that("regex_replace removes markup artifacts", {
  # empty hyperlinks
  expect_equal(regex_replace("[]{#foo}"), "")
  # {.variable}/{.sample} markers, with or without surrounding backticks
  expect_equal(regex_replace("`{.variable}`"), "")
  expect_equal(regex_replace("{.variable}"), "")
  expect_equal(regex_replace("`{.sample}`"), "")
  expect_equal(regex_replace("{.sample}"), "")
  # named-section attributes
  expect_equal(regex_replace("## Title {#sec-foo .unnumbered}"), "## Title")
  # ::: fenced-div lines
  expect_equal(regex_replace(":::"), "")
  expect_equal(regex_replace("::: foo"), "")
})

test_that("regex_replace fixes footnote references", {
  # forward and reverse cross-references both collapse to [^N], and because
  # the resulting line is a bare footnote ref, the colon rule then appends ":"
  expect_equal(regex_replace("[^3^](#FOOT7){#DOCF7}"), "[^3]:")
  expect_equal(regex_replace("[(3)](#DOCF7){#FOOT7}"), "[^3]:")
  # a bare footnote reference gains a trailing colon
  expect_equal(regex_replace("[^12]"), "[^12]:")
})

test_that("regex_replace fixes definition lists", {
  expect_equal(regex_replace("Function: foo\\"), "Function: foo\n:    \n")
  expect_equal(regex_replace("`bar`\\"), "`bar`\n:    \n")
})

test_that("regex_replace fixes code fences and backticks", {
  # indented code fences gain a leading blank line (4- and 8-space indents)
  expect_equal(regex_replace("    ``` R"), "\n    ``` R")
  expect_equal(regex_replace("        ``` c"), "\n        ``` c")
  # only the c/R language tags are matched, not a literal pipe
  expect_equal(regex_replace("    ``` |"), "    ``` |")
  # quotes around backticks are removed
  expect_equal(regex_replace("'`x`'"), "`x`")
  # stray double backticks are removed
  expect_equal(regex_replace("a``b"), "ab")
})

test_that("regex_replace normalises UTF-8 characters regardless of locale", {
  # these use useBytes = TRUE so they must fire even in a non-UTF-8 locale
  # ellipsis -> three dots
  expect_equal(regex_replace("a … b"), "a ... b")
  # copiable-link pilcrow anchors are removed
  expect_equal(regex_replace("[¶](url){.copiable-link}"), "")
})
