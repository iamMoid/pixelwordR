library("tidyverse")
library("dplyr")
library("stringr")

matrix_gen_l <- function(letter, type = "") {

  if (letter %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                    "K", "L", "O", "P", "Q", "R", "S", "T", "U", "V")) {

    r = 12
    c = 5
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "A") {
      mx[c(2:9), c(1, 5)] = 1
      mx[c(1, 5), c(2:4)] = 1
    } else if (letter == "B") {
      mx[c(1:9), 1] = 1
      mx[c(1, 5, 9), c(2:4)] = 1
      mx[c(2:4, 6:8), 5] = 1
    } else if (letter == "C") {
      mx[c(1, 9), c(2:5)] = 1
      mx[c(2:8), 1] = 1
    } else if (letter == "D") {
      mx[c(1:9), 1] = 1
      mx[c(1, 9), c(2:3)] = 1
      mx[c(2, 8), 4] = 1
      mx[c(3:7), 5] = 1
    } else if (letter == "E") {
      mx[c(1:9), 1] = 1
      mx[c(1, 5, 9), c(2:4)] = 1
      mx[c(1, 9), 5] = 1
    } else if (letter == "F") {
      mx[c(1:9), 1] = 1
      mx[c(1, 5), c(2:4)] = 1
      mx[c(1), 5] = 1
    } else if (letter == "G") {
      mx[c(2:8), 1] = 1
      mx[c(1, 9), c(2:4)] = 1
      mx[c(2, 5:8), 5] = 1
      mx[5, c(3:4)] = 1
    } else if (letter == "H") {
      mx[c(1:9), c(1, 5)] = 1
      mx[5, c(2:4)] = 1
    } else if (letter == "I") {
      mx[c(2:8), 3] = 1
      mx[c(1, 9), c(1:5)] = 1
    } else if (letter == "J") {
      mx[c(1:6), 5] = 1
      mx[c(7:8), c(1, 5)] = 1
      mx[9, c(2:4)] = 1
    } else if (letter == "K") {
      mx[c(1:9), 1] = 1
      mx[5, 2] = 1
      mx[c(4, 6), 3] = 1
      mx[c(3, 7), 4] = 1
      mx[c(1:2, 8:9), 5] = 1
    } else if (letter == "L") {
      mx[c(1:9), 1] = 1
      mx[9, c(2:5)] = 1
    } else if (letter == "O") {
      mx[c(2:8), c(1, 5)] = 1
      mx[c(1, 9), c(2:4)] = 1
    } else if (letter == "P") {
      mx[c(1:9), 1] = 1
      mx[c(1, 5), c(2:4)] = 1
      mx[c(2:4), 5] = 1
    } else if (letter == "Q") {
      mx[c(2:8), 1] = 1
      mx[c(2:7), 5] = 1
      mx[1, c(2:4)] = 1
      mx[9, c(2:3)] = 1
      mx[6, 2] = 1
      mx[7, 3] = 1
      mx[8, 4] = 1
      mx[9, 5] = 1
    } else if (letter == "R") {
      mx[c(1:9), 1] = 1
      mx[c(1, 5), c(2:4)] = 1
      mx[c(2:4), 5] = 1
      mx[6, 2] = 1
      mx[7, 3] = 1
      mx[8, 4] = 1
      mx[9, 5] = 1
    } else if (letter == "S") {
      mx[c(1, 5, 9), c(2:4)] = 1
      mx[c(2:4, 8), 1] = 1
      mx[c(2, 6:8), 5] = 1
    } else if (letter == "T") {
      mx[c(2:9), 3] = 1
      mx[1, c(1:5)] = 1
    } else if (letter == "U") {
      mx[c(1:8), c(1, 5)] = 1
      mx[9, c(2:4)] = 1
    } else if (letter == "V") {
      mx[c(1:7), c(1, 5)] = 1
      mx[8, c(2, 4)] = 1
      mx[9, 3] = 1
    } else if (letter == "M") {
      mx[c(1:9), c(1, 5)] = 1
      mx[2, c(2, 4)] = 1
      mx[c(3:4), 3] = 1
    } else if (letter == "N") {
      mx[c(1:9), c(1, 5)] = 1
      mx[3, 2] = 1
      mx[4, 3] = 1
      mx[5, 4] = 1
    } else if (letter == "W") {
      mx[c(1:9), c(1, 5)] = 1
      mx[8, c(2, 4)] = 1
      mx[c(6:7), 3] = 1
    } else if (letter == "X") {
      mx[c(1:3, 7:9), c(1, 5)] = 1
      mx[c(4, 6), c(2, 4)] = 1
      mx[5, 3] = 1
    } else if (letter == "Y") {
      mx[c(1:4), c(1, 5)] = 1
      mx[5, c(2, 4)] = 1
      mx[c(6:9), 3] = 1
    } else if (letter == "Z") {
      mx[c(1, 9), c(1:5)] = 1
      mx[c(2:3), 5] = 1
      mx[4, 4] = 1
      mx[5, 3] = 1
      mx[6, 2] = 1
      mx[c(7:8), 1] = 1
    }

  } else if (letter %in% c("M", "N", "W", "X", "Y", "Z")) {

    r = 12
    c = 7
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "M") {
      mx[c(1:9), c(1, 7)] = 1
      mx[2, c(2, 6)] = 1
      mx[3, c(3, 5)] = 1
      mx[4, 4] = 1
    } else if (letter == "N") {
      mx[c(1:9), c(1, 7)] = 1
      mx[2, 2] = 1
      mx[3, 3] = 1
      mx[4, 4] = 1
      mx[5, 5] = 1
      mx[6, 6] = 1
    } else if (letter == "W") {
      mx[c(1:9), c(1, 7)] = 1
      mx[8, c(2, 6)] = 1
      mx[7, c(3, 5)] = 1
      mx[6, 4] = 1
    } else if (letter == "X") {
      mx[c(1:2, 8:9), c(1, 7)] = 1
      mx[c(3, 7), c(2, 6)] = 1
      mx[c(4, 6), c(3, 5)] = 1
      mx[5, 4] = 1
    } else if (letter == "Y") {
      mx[c(1:3), c(1, 7)] = 1
      mx[c(4), c(2, 6)] = 1
      mx[c(5), c(3, 5)] = 1
      mx[c(6:9), 3] = 1
    } else if (letter == "Z") {
      mx[c(1, 9), c(1:7)] = 1
      mx[2, 7] = 1
      mx[3, 6] = 1
      mx[4, 5] = 1
      mx[5, 4] = 1
      mx[6, 3] = 1
      mx[7, 2] = 1
      mx[8, 1] = 1
    }
  }
    
return(mx)
}
