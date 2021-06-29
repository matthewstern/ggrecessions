#' Internal function to filter the built-in recessions table
#'
#' @param min,max the top and bottom cutoffs
#' @param date_scale Format of `min` and `max`: FALSE presumes numeric.
#' @param show_ongoing FALSE removes an ongoing recession, TRUE leaves it in to
#'   be trimmed by `max`
#' @param rtable the (recessions) table to clip. Must have, at a minimum,
#'   columns named `start` and `end`.
#'
#' @return data.frame with two columns, start and end. These columns are
#'   numeric, and contain either decimal dates or the numeric equivalent of an R
#'   date depending on the value of \code{date_scale}
#'
#' @importFrom lubridate decimal_date
#' @import dplyr
#'
#' @noRd
#'
filter_recessions <- function(min, max, date_scale, show_ongoing, rtable){

  # Bind local variables to function
  end <- start <- ongoing <- NULL

  # Filtering out ongoing recessions if specified
  if (!show_ongoing & "ongoing" %in% colnames(rtable)) {
    rtable <- filter(rtable, !ongoing)
  }

  # If requested scale is numeric, convert dates to numbers
  if (!date_scale & class(rtable$start) == "Date") {
    rtable <- mutate(
      rtable,
      start = lubridate::decimal_date(start),
      end = lubridate::decimal_date(end)
    )
  }

  # Remove recessions outside of range
  rtable <- filter(rtable, end > min, start < max)

  # If `min` or `max` fall in  middle of a recession, modify recession to end at specified term.
  rtable <- transmute(
    rtable,
    start = as.numeric(if_else(start < min, min, start)),
    end = as.numeric(if_else(end > max, max, end))
  )

  return(rtable)
}
