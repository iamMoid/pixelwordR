library("ggplot2")
library("ggeasy")

pixelplot <- function(matrix_data, type = "") {
  plot <- ggplot2::ggplot(
    data = matrix_data,
    mapping = aes(
      x = x_axis,
      y = reorder(y_axis, desc(y_axis)),
      color = values,
      fill = values
    )
  ) +
    ggplot2::scale_color_viridis_c(option = "viridis", direction = -1) +
    ggplot2::scale_fill_viridis_c(option = "viridis", direction = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggeasy::easy_remove_axes()

  if (type == "") {
    plot <- plot + ggplot2::geom_raster()
  } else {
    plot <- plot + ggplot2::geom_point(size = 0.1)
  }

return(plot)
}
