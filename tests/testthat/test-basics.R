test_that("Output of fetch_recessions() matches recessions.rda", {
  skip_if_offline()
  expect_identical(recessions, fetch_recessions(quietly = TRUE))
})
