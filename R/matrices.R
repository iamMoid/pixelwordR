library("tidyverse")
library("dplyr")
library("stringr")

matrix_gen <- function(letter) {
  if (!letter %in% c("M", "N", "Q", "W")) {
    r = 11
    c = 7

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
    } else if (letter == "U") {
      mx[11, c(1, 7)] = 0
      mx[c(1:8), 4] = 0
    } else if (letter == "V") {
      mx[10, c(1, 7)] = 0
      mx[11, c(1, 2, 6, 7)] = 0
      mx[c(1:8), 4] = 0
    } else if (letter == "X") {
      mx[6, c(1, 7)] = 0
      mx[c(1:4, 8:11), 4] = 0
    } else if (letter == "Y") {
      mx[7, c(1, 7)] = 0
      mx[8:11, c(1, 2, 6, 7)] = 0
      mx[c(1:4), 4] = 0
    } else if (letter == "Z") {
      mx[4, c(1:3)] = 0
      mx[5, c(1:2)] = 0
      mx[6, c(1, 7)] = 0
      mx[7, c(6:7)] = 0
      mx[8, c(5:7)] = 0
    }

    mxf <- mx * mxr

  } else if (letter %in% c("Q")) {

    r = 11
    c = 9

    mxr <- matrix(data = runif(r * c, 1, 5), nrow = r, ncol = c)
    mx <- matrix(data = 1L, nrow = r, ncol = c)
    if (letter == "Q") {
      mx[c(1, 11), c(1, 7)] = 0
      mx[c(4, 5, 8), 4] = 0
      mx[c(1:8), c(8:9)] = 0
      mx[9, 9] = 0
    }

    mxf <- mx * mxr

  } else {

    r = 11
    c = 11

    mxr <- matrix(data = runif(r * c, 1, 5), nrow = r, ncol = c)
    mx <- matrix(data = 1L, nrow = r, ncol = c)
    if (letter == "M") {
      mx[c(1, 8:11), c(4:8)] = 0
      mx[c(6, 7), c(4, 8)] = 0
      mx[c(2, 7), c(5, 7)] = 0
      mx[c(2, 3), 6] = 0
    } else if (letter == "N") {
      mx[c(1:2, 11), c(4:8)] = 0
      mx[c(7:10), 4] = 0
      mx[c(3, 8:10), 5] = 0
      mx[c(3:4, 9:10), 6] = 0
      mx[c(3:5, 10), 7] = 0
      mx[c(3:6), 8] = 0
    } else if (letter == "W") {
      mx[c(1:4, 11), c(4:8)] = 0
      mx[c(5, 6), c(4, 8)] = 0
      mx[c(5, 10), c(5, 7)] = 0
      mx[c(9, 10), 6] = 0
    }

    mxf <- mx * mxr

  }

return(mxf)
}
