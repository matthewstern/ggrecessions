
# add show_legend
# add ... to pass on other things to geom_rect
# look at inherit params from geom_rect when documenting?

geom_recessions <- function(xlimits = "limits",
                            ylimits = "all",
                            xscale = "detect",
                            fill = "gray90",
                            show_ongoing = TRUE){

  start <- end <- bottom <- top <- NULL

  geom_rect(aes(xmin = after_stat(start),
                xmax = after_stat(end),
                ymin = after_stat(bottom),
                ymax = after_stat(top)),
            fill = fill,
            xlimits = xlimits,
            ylimits = ylimits,
            xscale = xscale,
            show_ongoing = show_ongoing,
            stat = "recessions")
}
