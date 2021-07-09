#' @import ggplot2
StatRecessions <- ggproto("StatRecessions", Stat,
  compute_panel = function(data,
                           scales,
                           from_data,
                           show_ongoing){

    # fetch a min and max from either the plot's data or scale
    min <- ifelse(from_data, min(data$x), scales$x$range$range[1])
    max <- ifelse(from_data, max(data$x), scales$x$range$range[2])

    # determine if x axis has a date scale
    ## add a fn arg for date_scale that defaults to NULL and set this up to only run if it's NULL
    date_scale <- scales$x$scale_name == "date"

    # transform the data
    data <- filter_recessions(min = min,
                              max = max,
                              date_scale = date_scale,
                              show_ongoing = show_ongoing) # HAVEN'T TESTED THIS YET

    # add a fn arg for y min and max that defaults to "limits" but can also be
    # "breaks" or a vector of two numbers. If limits or breaks, determine ymin and max
    # from scale.

    return(data)
  }


  # think about adding required aes? would need to be x, or x or y if function can be inverted.

  # what other stat functions? default_aes? others?

)








