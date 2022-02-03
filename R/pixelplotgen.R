library("ggplot2")
library("ggeasy")

pixelplot <- function(matrix_data) {
  plot <- ggplot2::ggplot(
    data = matrix_data,
    mapping = aes(
      x = x_axis,
      y = y_axis,
      fill = values
    )
  ) +
    ggplot2::scale_fill_viridis_c(option="magma", direction = -1) +
    ggplot2::geom_raster() +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none") +
    ggeasy::easy_remove_axes()

return(plot)
}
