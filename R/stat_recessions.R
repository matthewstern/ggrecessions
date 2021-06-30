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
    date_scale <- scales$x$scale_name == "date"

    # transform the data
    data <- filter_recessions(min = min,
                              max = max,
                              date_scale = date_scale,
                              show_ongoing = show_ongoing) # HAVEN'T TESTED THIS YET

    return(data)
  }

  # compute y axis heights from scale, too -- maybe allow "limits", "breaks", or custom?

  # think about adding required aes? would need to be x, or x or y if function can be inverted.
  # think about adding ability to do y axis too

)








