library("tidyverse")
library("dplyr")
library("stringr")

matrix_gen_s <- function(letter, type = "") {

  if (letter %in% c("a", "b", "c", "d", "e", "g", "h", "k", "n", "o", "p",
                    "q", "r", "s", "u", "v", "x", "y", "z")) {

    r = 11
    c = 5
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "a") {
      mx[c(4:8), 5] = 1
      mx[4, c(2:4)] = 1
      mx[c(5:7), 1] = 1
      mx[7, 4] = 1
      mx[8, c(2:3)] = 1
    } else if (letter == "b") {
      mx[c(1:8), 1] = 1
      mx[4, c(2:4)] = 1
      mx[c(5:7), 5] = 1
      mx[7, 2] = 1
      mx[8, c(3:4)] = 1
    } else if (letter == "c") {
      mx[c(4, 8), c(2:5)] = 1
      mx[c(5:7), 1] = 1
    } else if (letter == "d") {
      mx[c(1:8), 5] = 1
      mx[4, c(2:4)] = 1
      mx[c(5:7), 1] = 1
      mx[7, 4] = 1
      mx[8, c(2:3)] = 1
    } else if (letter == "e") {
      mx[c(5:7), 1] = 1
      mx[c(4, 6, 8), c(2:4)] = 1
      mx[c(5, 8), 5] = 1
    } else if (letter == "g") {
      mx[c(5:10), 5] = 1
      mx[c(4, 11), c(2:4)] = 1
      mx[c(5:7, 10), 1] = 1
      mx[7, 4] = 1
      mx[8, c(2:3)] = 1
    } else if (letter == "h") {
      mx[c(1:8), 1] = 1
      mx[4, c(2:4)] = 1
      mx[c(5:8), 5] = 1
    } else if (letter == "k") {
      mx[c(1:8), 1] = 1
      mx[6, c(2:3)] = 1
      mx[c(5, 7), 4] = 1
      mx[c(4, 8), 5] = 1
    } else if (letter == "n") {
      mx[c(5:8), c(1, 5)] = 1
      mx[4, c(2:4)] = 1
    } else if (letter == "o") {
      mx[c(5:7), c(1, 5)] = 1
      mx[c(4, 8), c(2:4)] = 1
    } else if (letter == "p") {
      mx[c(5:11), 1] = 1
      mx[c(4, 8), c(2:4)] = 1
      mx[c(5:7), 5] = 1
    } else if (letter == "q") {
      mx[c(5:11), 5] = 1
      mx[c(4, 8), c(2:4)] = 1
      mx[c(5:7), 1] = 1
    } else if (letter == "r") {
      mx[4, c(2:5)] = 1
      mx[c(5:8), 1] = 1
    } else if (letter == "s") {
      mx[c(4, 6, 8), c(2:4)] = 1
      mx[c(5, 8), 1] = 1
      mx[c(4, 7), 5] = 1
    } else if (letter == "u") {
      mx[c(4:7), c(1, 5)] = 1
      mx[8, c(2:4)] = 1
    } else if (letter == "v") {
      mx[c(4:6), c(1, 5)] = 1
      mx[7, c(2, 4)] = 1
      mx[8, 3] = 1
    } else if (letter == "x") {
      mx[c(4, 8), c(1, 5)] = 1
      mx[c(5, 7), c(2, 4)] = 1
      mx[6, 3] = 1
    } else if (letter == "y") {
      mx[c(4:10), 5] = 1
      mx[11, c(2:4)] = 1
      mx[c(4:7, 10), 1] = 1
      mx[7, 4] = 1
      mx[8, c(2:3)] = 1
    } else if (letter == "z") {
      mx[c(4, 8), c(1:5)] = 1
      mx[5, 4] = 1
      mx[6, 3] = 1
      mx[7, 2] = 1
    }

  } else if (letter %in% c("i", "l")) {

    r = 11
    c = 1
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "i") {
      mx[c(2, 4:8), 1] = 1
    } else if (letter == "l") {
      mx[c(1:8), 1] = 1
    }

  } else if (letter %in% c("f", "j", "t")) {

    r = 11
    c = 3
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "f") {
      mx[c(2:8), 1] = 1
      mx[c(1, 4), c(2:3)] = 1
    } else if (letter == "j") {
      mx[c(2, 4:9), 3] = 1
      mx[10, c(1:2)] = 1
    } else if (letter == "t") {
      mx[c(1:7), 1] = 1
      mx[c(4, 8), c(2:3)] = 1
    }

  } else if (letter %in% c("m", "w")) {

    r = 11
    c = 9
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == "m") {
      mx[c(5:8), c(1, 5, 9)] = 1
      mx[4, c(2:4, 6:8)] = 1
    } else if (letter == "w") {
      mx[c(4:7), c(1, 5, 9)] = 1
      mx[8, c(2:4, 6:8)] = 1
    }

  } else if (letter %in% c(" ", ".", "!")) {

    r = 11
    c = 1
    mx <- matrix(data = 0L, nrow = r, ncol = c)

    if (letter == " ") {
    
    } else if (letter == ".") {
      mx[8, 1] = 1
    } else if (letter == "!") {
      mx[c(1:6, 8), 1] = 1
    }

  }

return(mx)
}
