library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggeasy")

matrix_gen <- function(l) {
  r = 11
  c = 7
  p = 2
  mxr <- matrix(data = runif((r + p * 2) * (c + p), 1, 5), nrow = r + p * 2, ncol = c + p)
  if (l == "A") {
    mx <- matrix(data = 1L, nrow = r + p * 2, ncol = c + p)
    mx[1 + p, c(1 + p/2, 7 + p/2)] = 0
    mx[4 + p, 4 + p/2] = 0
    mx[(8 + p):(11 + p), 4 + p/2] = 0
    mx[1:p,] = 0
    mx[(r + p + 1):(r + p * 2),] = 0
    mx[,1] = 0
    mx[,c + p] = 0
  }

  mxf <- mx * mxr

  colnames(mxf) <- c("x01", "x02", "x03", "x04", "x05", "x06", "x07", "x08", "x09")
  rownames(mxf) <- c("y15", "y14", "y13", "y12", "y11", "y10", "y09", "y08", "y07", "y06", "y05", "y04", "y03", "y02", "y01")

  mxl <- mxf |>
    as.data.frame() |>
    rownames_to_column("y_axis") |>
    pivot_longer(-c(y_axis), names_to = "x_axis", values_to = "values")

return(mxl)
}
