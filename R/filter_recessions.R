#' Internal function to filter the built-in recessions table
#'
#' @param min,max the top and bottom cutoffs
#' @param date_scale Format of `min` and `max`: FALSE presumes numeric.
#' @param show_ongoing FALSE removes an ongoing recession, TRUE leaves it in to
#'   be trimmed by `max`
#' @param rtable the (recessions) table to clip. Must have, at a minimum,
#'   columns named `start` and `end`.
#'
#' @return data.frame with two columns, start and end. Column formats are
#'   determined by \code{date_scale}.
#'
#' @importFrom lubridate decimal_date
#' @importFrom lubridate as_date
#' @import dplyr
#'
#' @noRd
#'
filter_recessions <- function(min, max, date_scale = FALSE, show_ongoing = FALSE, rtable = get("recessions")){

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
      start = decimal_date(start),
      end = decimal_date(end)
    )
  }

  # Remove recessions outside of range
  rtable <- filter(rtable, end > min, start < max)

  # If `min` or `max` fall in  middle of a recession, modify recession to end at specified term.
  rtable <- transmute(
    rtable,
    start = ifelse(start < min, min, start),
    end = ifelse(end > max, max, end)
  )

  # coerce `start` and `date` into preferred format
  rtable <- mutate(
    rtable,
    start = do.call(ifelse(date_scale, "as_date", "as.numeric"), list(start)),
    end = do.call(ifelse(date_scale, "as_date", "as.numeric"), list(end))
  )

  return(rtable)
}
