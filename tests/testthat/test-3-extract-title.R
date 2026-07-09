# Pins the behaviour of extract_title_from_index(): makeinfo appends a
# copiable-link anchor (" ¶") inside the manual's <h1>, and that pilcrow must
# not leak into the extracted title (which feeds the book/navbar/page titles).

test_that("extract_title_from_index drops the copiable-link pilcrow", {
  title <- extract_title_from_index("manuals/test/prep/index.html")
  expect_equal(title, "An Introduction to R")
  expect_false(grepl(intToUtf8(0x00b6), title, useBytes = TRUE))
})