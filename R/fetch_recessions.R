#' Recessions
#'
#' The \code{fetch_recessions()} function downloads the current recessions table
#' from the NBER website and formats it for use by other functions in this
#' package. The \code{recessions} table is the function's output, stored within
#' the package for easy access.
#'
#' @format A data frame with the following variables: \itemize{ \item
#'   \code{start_char, end_char}: Easily readable labels for the beginning and
#'   end of the recession. \item \code{start_date, end_date}: Recession Dates
#'   expressed in R datetime format, using the first day of the specified month.
#'   \item \code{ongoing}: Logical. Whether or not the recession is ongoing as
#'   of the latest available NBER data. }
#'
#' @source \url{https://www.nber.org/data/cycles/cycle dates pasted.csv}
#'
#'
"recessions"

#' Fetch current recession table
#'
#' @describeIn recessions Function for fetching recessions from NBER website
#'
#' @param url Char, the web location of the NBER machine-readable CSV file. The
#'   default, \code{NULL}, uses the most recently identified URL known to the
#'   package development team.
#' @param quietly Logical, suppresses messages.
#'
#' @examples
#' recessions <- fetch_recessions()
#'
#' \dontrun{
#'   # package maintainers run:
#'   recessions <- fetch_recessions()
#'   usethis::use_data(recessions, overwrite = TRUE)
#' }
#'
#' @import dplyr
#' @importFrom utils read.csv
#'
#' @export
fetch_recessions <- function(url = NULL, quietly = FALSE){

  # Use default URL if user does not override
  if (is.null(url) | missing(url)) {
    url <- "http://data.nber.org/data/cycles/cycle%20dates%20pasted.csv"
  }

  # locally bind variable names
  start_char <- end_char <- start_date <- end_date <- ongoing <- index <- peak <- trough <- NULL

    # attempt to download and format recessions table
    tryCatch({
      recessions <- read.csv(url)

      recessions <- recessions %>%
        # drop first row trough
        slice(-1) %>%
        # convert peaks and troughs...
        mutate(
          # ...to R dates
          start_date = as.Date(peak),
          end_date = as.Date(trough),
          # ... and clean char strings
          start_char = format(start_date, "%b %Y"),
          end_char = format(end_date, "%b %Y")) %>%
        # confirm ascending and create row number
        arrange(start_date) %>%
        mutate(index = row_number()) %>%
        mutate(
          # Flag unfinished recessions
          ongoing = case_when(
            is.na(end_date) & index == max(.$index) ~ T,
            TRUE ~ F),
          # set ongoing recession to arbitrary future date
          end_date = case_when(
            ongoing ~ as.Date("2200-01-01"),
            TRUE ~ end_date),
          # mark ongoing recession in char field
          end_char = case_when(
            ongoing ~ "Ongoing",
            TRUE ~ end_char)
        ) %>%
        # clean up
        select(start_char, end_char, start_date, end_date, ongoing)

      if (!quietly) {message("Successfully fetched from NBER")}

      # Return recessions
      return(recessions)
    },
    error = function(cond){
      if (!quietly) message("WARNING: Fetch or processing failed. `NULL` returned.")
      return(NULL)
    }
    )
}
