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
