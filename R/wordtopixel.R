library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggeasy")

#' Create a colorful pixelated gif for the input word
#'
#' @param word Word to create the gif for.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' > pixelword("Moid")

# initialize 7 by 7 matrix with random numbers and set column names
mx <- as.data.frame(matrix(data = rnorm(49), nrow = 7, ncol = 7))
colnames(mx) <- c("x01", "x02", "x03", "x04", "x05", "x06", "x07")

# assign row names to a new column
mxy <- cbind(y_axis = case_when(
  stringr::str_length(rownames(mx)) == 2 ~ paste0("y", rownames(mx)),
  stringr::str_length(rownames(mx)) == 1 ~ paste0("y0", rownames(mx)),
  TRUE                                   ~ "error"
), mx)

# convert 2 row level data using pivot longer
mxyl <- mxy |>
  pivot_longer(cols = !y_axis, names_to = "x_axis", values_to = "values")

# pixelword function to plot gif
pixelword <- function(word) {
  plot <- ggplot2::ggplot(
    data = matrix_gen(word),
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
