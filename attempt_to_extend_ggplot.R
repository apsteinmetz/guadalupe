```r
library(ggplot2)
library(ggmap)

GeomScaleBar <- ggproto("GeomScaleBar", Geom,
                        required_aes = c(),
                        default_aes = aes(color = "black", linewidth = 1.5, size = 3),
                        
                        draw_panel = function(data, panel_params, coord, scale_miles = 5) {
                          # Get panel bounds
                          ranges <- coord$backtransform_range(panel_params)
                          
                          # Calculate positions for scale bar
                          x_start <- ranges$x[1] + (ranges$x[2] - ranges$x[1]) * 0.4
                          y_pos <- ranges$y[1] + (ranges$y[2] - ranges$y[1]) * 0.1
                          
                          # Convert miles to degrees (approximate)
                          miles_to_deg <- scale_miles / 69
                          x_end <- x_start + miles_to_deg
                          
                          # Create line segment
                          line_grob <- grid::segmentsGrob(
                            x0 = x_start, y0 = y_pos,
                            x1 = x_end, y1 = y_pos,
                            default.units = "native",
                            gp = grid::gpar(col = data$color[1], lwd = data$linewidth[1])
                          )
                          
                          # Create text label
                          text_grob <- grid::textGrob(
                            label = paste(scale_miles, "miles"),
                            x = (x_start + x_end) / 2,
                            y = y_pos,
                            vjust = -0.5,
                            hjust = 0.5,
                            default.units = "native",
                            gp = grid::gpar(col = data$color[1], cex = data$size[1] / 3.5)
                          )
                          
                          grid::gTree(children = grid::gList(line_grob, text_grob))
                        }
)

geom_scale_bar <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", scale_miles = 5, ...,
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomScaleBar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scale_miles = scale_miles,
      na.rm = na.rm,
      ...
    )
  )
}

map_data <- get_map(location = "New York", zoom = 10)
ggmap(map_data) +
  geom_scale_bar(scale_miles = 5, color = "black", linewidth = 2, size = 4) +
  theme_minimal() +
  labs(title = "Scale Bar Example", x = "Longitude", y = "Latitude")

