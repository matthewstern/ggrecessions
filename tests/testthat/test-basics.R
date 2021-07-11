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

## TEST CODE BELOW
# library(tidyverse)
#
# df <- bind_rows(
#   data.frame(year = 1980:1999, value = rnorm(20), type = "A", frame = "X"),
#   data.frame(year = 1970:1989, value = rnorm(20), type = "B", frame = "X"),
#   data.frame(year = 1995:2021, value = rnorm(27), type = "C", frame = "Y"),
#   data.frame(year = 1995:2009, value = rnorm(15), type = "D", frame = "Y")
# ) %>%
#   mutate(year_date = as.Date(lubridate::date_decimal(year)))
#
#
# devtools::load_all()
#
#
# ggplot(df, mapping = aes(x = year, y = value)) +
#   #scale_x_date("Year") +
#   scale_x_continuous("Year") +
#   facet_wrap("frame", scales = "free") +
#   theme_minimal() +
#   geom_recessions() +
#   # geom_rect(aes(xmin = after_stat(start),
#   #               xmax = after_stat(end),
#   #               ymin = after_stat(bottom),
#   #               ymax = after_stat(top)),
#   #           fill = "gray90",
#   #           xlimits = c("limits", "limits"),
#   #           ylimits = "breaks",
#   #           xscale = "detect",
#   #           show_ongoing = TRUE,
#   #           stat = "recessions") +
#   geom_text(mapping = aes(x = after_stat(end),
#                           y = after_stat(top)),
#             label = "recession",
#             stat = "recessions",
#             angle = 270,
#             vjust = "bottom", # actually horiz, bc rotation
#             hjust = "left", # actually vertical, bc rotation
#             xlimits = "limits",
#             ylimits = "breaks",
#             xscale = "detect",
#             show_ongoing = TRUE) +
#   geom_line(aes(color = type))

