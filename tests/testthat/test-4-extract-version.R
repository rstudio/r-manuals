# Pins the behaviour of extract_version_label(), which parses the `@set VERSION`
# line of version.texi into a short label used in the book title
# ("R v4.6.1 Manuals :: ...").

test_that("extract_version_label reads a released version", {
  f <- withr::local_tempfile()
  brio::write_lines(
    c(
      "@set VERSION 4.6.1",
      "@set VERSIONno 4.6.1",
      "@set RWVERSION R-4.6.1"
    ),
    f
  )
  expect_equal(extract_version_label(f), "v4.6.1")
})

test_that("extract_version_label labels R-devel as -devel", {
  f <- withr::local_tempfile()
  brio::write_lines("@set VERSION 4.7.0 Under development", f)
  expect_equal(extract_version_label(f), "v4.7.0-devel")
})

test_that("extract_version_label labels a release branch as -patched", {
  f <- withr::local_tempfile()
  brio::write_lines("@set VERSION 4.5.3 Patched", f)
  expect_equal(extract_version_label(f), "v4.5.3-patched")
})

test_that("extract_version_label returns NA for a missing file", {
  expect_true(is.na(extract_version_label("does-not-exist.texi")))
})
