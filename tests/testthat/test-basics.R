test_that("Output of fetch_recessions() matches recessions.rda", {
  skip_if_offline()
  expect_identical(recessions, fetch_recessions(quietly = TRUE))
})


test_that("Filter recessions works: numbers", {
  expect_silent({df <- filter_recessions(1950, 2009)}) # no warnings or errors
  expect_equal(nrow(df), 10)        # right number of recessions
  expect_type(df[[1,1]], "double")  # table returns numeric
  expect_equal(df[[10,2]], 2009)    # recession is trimmed
})

test_that("Filter recessions works: dates", {
  expect_silent({df <- filter_recessions(lubridate::ymd(19500101),
                                         lubridate::ymd(20090201),
                                         date_scale = TRUE)})
  expect_equal(nrow(df), 10)
  expect_s3_class(df[[1,1]], "Date")
  expect_equal(df[[10,2]], lubridate::ymd(20090201))
})

