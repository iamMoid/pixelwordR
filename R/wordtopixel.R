library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggeasy")
library("cowplot")

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
  letters <- stringr::str_split(word, "")[[1]]

  vpad <- matrix(data = 0L, nrow = 11, ncol = 2)

  pixel_banner <- vpad

  for (letter in letters) {
    print(letter)
    pixel_matrix <- matrix_gen(letter)
    pixel_banner <- cbind(pixel_banner, pixel_matrix, vpad)
  }

  hpad <- matrix(data = 0L, nrow = 2, ncol = ncol(pixel_banner))
  pixel_banner <- rbind(hpad, pixel_banner, hpad)

  colnames(pixel_banner) <- paste0("x", 1:ncol(pixel_banner))
  colnames(pixel_banner)[1:9] <- paste0("x0", 1:9)

  rownames(pixel_banner) <- paste0("y", 1:nrow(pixel_banner))
  rownames(pixel_banner)[1:9] <- paste0("y0", 1:9)

  pixel_banner_l <- pixel_banner |>
    as.data.frame() |>
    rownames_to_column("y_axis") |>
    pivot_longer(-c(y_axis), names_to = "x_axis", values_to = "values")

  banner <- pixelplot(pixel_banner_l)

return(banner)
}
