library("tidyverse")
library("dplyr")
library("stringr")
library("ggplot2")
library("ggeasy")

matrix_gen <- function(letter) {
  r = 11
  c = 7
  p = 2
  mxr <- matrix(data = runif(r * c, 1, 5), nrow = r, ncol = c)
  mx <- matrix(data = 1L, nrow = r, ncol = c)
  if (letter == "A") {
    mx[1, c(1, 7)] = 0
    mx[c(4, 8:11), 4] = 0
  } else if (letter == "B") {
    mx[c(1, 6, 11), 7] = 0
    mx[c(4, 8), 4] = 0
  } else if (letter == "C") {
    mx[c(1, 11), c(1, 7)] = 0
    mx[c(4:8), 4] = 0
    mx[6, c(5:7)] = 0
  } else if (letter == "D") {
    mx[c(1, 11), 7] = 0
    mx[c(4:8), 4] = 0
  } else if (letter == "E") {
    mx[c(4, 8), c(4:7)] = 0
  } else if (letter == "F") {
    mx[c(4, 8:11), c(4:7)] = 0
  } else if (letter == "G") {
    mx[c(1, 11), 1] = 0
    mx[1, 7] = 0
    mx[c(4, 5, 8), 4] = 0
    mx[5, c(5:7)] = 0
  } else if (letter == "H") {
    mx[c(1:4, 8:11), 4] = 0
  } else if (letter == "I") {
    mx[c(4:8), c(1:2, 6:7)] = 0
  } else if (letter == "J") {
    mx[c(1:7), c(1:4)] = 0
    mx[11, c(1, 7)] = 0
    mx[8, 4] = 0
  } else if (letter == "K") {
    mx[c(1:4, 8:11), 4] = 0
    mx[6, 7] = 0
  } else if (letter == "L") {
    mx[c(1:8), c(4:7)] = 0
  } else if (letter == "N") {
    mx[c(4:11), 4] = 0
    mx[1, 7] = 0
  } else if (letter == "O") {
    mx[c(1, 11), c(1, 7)] = 0
    mx[c(4:8), 4] = 0
  } else if (letter == "P") {
    mx[c(1, 7), 7] = 0
    mx[4, 4] = 0
    mx[c(8:11), c(4:7)] = 0
  } else if (letter == "R") {
    mx[c(1, 6), 7] = 0
    mx[c(4, 8:11), 4] = 0
  } else if (letter == "S") {
    mx[4, c(4:7)] = 0
    mx[8, c(1:4)] = 0
    mx[c(1, 7), 1] = 0
    mx[c(5, 11), 7] = 0
  } else if (letter == "T") {
    mx[c(4:11), c(1:2, 6:7)] = 0
  }

  mxf <- mx * mxr

  colnames(mxf) <- c("x01", "x02", "x03", "x04", "x05", "x06", "x07")
  rownames(mxf) <- c("y11", "y10", "y09", "y08", "y07", "y06", "y05", "y04", "y03", "y02", "y01")

  mxl <- mxf |>
    as.data.frame() |>
    rownames_to_column("y_axis") |>
    pivot_longer(-c(y_axis), names_to = "x_axis", values_to = "values")

return(mxl)
}
