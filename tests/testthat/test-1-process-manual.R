test_that("process manual", {
  manuals <- "test.texi"
  expect_null(
    process_manual(manuals, .quicktest = TRUE)
  )
  expect_null(
    process_manual(manuals, .quicktest = TRUE, .make_info = TRUE)
  )
})
