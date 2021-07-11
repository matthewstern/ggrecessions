# need to document and export this.

#' @import ggplot2
StatRecessions <- ggproto("StatRecessions", Stat,
  compute_panel = function(data,
                           scales,
                           xlimits,
                           ylimits,
                           xscale,
                           show_ongoing){

    # determine limits
    xlimits <- process_limits(xlimits, "x", scales, data)
    ylimits <- process_limits(ylimits, "y", scales, data)

    # determine scale
    if (xscale == "detect"){xscale <- scales$x$scale_name}

    # transform the data
    data <- filter_recessions(min = xlimits[1],
                              max = xlimits[2],
                              date_scale = (xscale == "date"),
                              show_ongoing = show_ongoing)

    data["bottom"] <- ylimits[1]
    data["top"] <- ylimits[2]

    return(data)
  }
)



#' Helper function to determine limits
#'
#' @param limits From where should the limits for recessions be determined?
#'   Accepts vectors of length 1 or 2. Length 1 vectors will be doubled. Values
#'   are one of "limits", "breaks", "data", or a number. The special case,
#'   "all", converts to c(-Inf, +Inf).
#' @param axis Text value "x" or "y".
#' @param scales,data the scales environment and data table, passed on from the
#'   stat. limits are determined from within these values.
#'
#' @importFrom utils type.convert
#'
#' @noRd
process_limits <- function(limits, axis, scales, data){

  axis <- match.arg(axis, c("x", "y"))

  # Confirm `limits` is 2-length vector
  if (length(limits) == 1){
    if (limits == "all"){
      limits <- c(-Inf, +Inf)
    } else {
      limits <- c(limits, limits)
    }
  } else if(length(limits) > 2){
    stop("Supply only 1 or 2 limits.")
  }

  # convert custom limits
  lims <- c()
  for (pos in seq_along(limits)){
    val <- type.convert(limits[pos], as.is = TRUE)
    if(class(val) %in% c("numeric", "integer")){
      lims[pos] <- val
      limits[pos] <- "custom"
    }
    limits[pos] <- match.arg(limits[pos], c("limits", "breaks", "data", "custom"))
  }

  # calculate min and max
  min <- switch(limits[1],
                "data" = min(data[[axis]]),
                "limits" = scales[[axis]]$get_limits()[1],
                "breaks" = min(scales[[axis]]$get_breaks(), na.rm = TRUE),
                "custom" = lims[1])

  max <- switch(limits[2],
                "data" = max(data[[axis]]),
                "limits" = scales[[axis]]$get_limits()[2],
                "breaks" = max(scales[[axis]]$get_breaks(), na.rm = TRUE),
                "custom" = lims[2])

  return(c(min, max))
}

