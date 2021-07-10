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


test_that("Current test code", {
  library(tidyverse)
  df <- data.frame(year_dec=1950:1999, value=rnorm(100), var=c(rep("A", 50), rep("B", 50)))
  df$year_date <- as.Date(lubridate::date_decimal(df$year_dec))

  devtools::document()
  devtools::load_all()


  ggplot(df, mapping = aes(x = year_date, y = value)) +
    geom_line(aes(color = var)) +
    geom_rect(aes(xmin = after_stat(start),
                  xmax = after_stat(end),
                  ymin = after_stat(-1),
                  ymax = after_stat(1)),
              from_data = FALSE,
              show_ongoing = FALSE,
              stat = "recessions") +
    geom_text(mapping = aes(x = after_stat(end)),
              label = "recession",
              stat = "recessions",
              y = 2,
              angle = 270,
              vjust = "bottom", # actually horiz, bc rotation
              hjust = "left", # actually vertical, bc rotation
              from_data = FALSE,
              show_ongoing = FALSE) +
    scale_x_date("Year") +
    theme_minimal()

})
