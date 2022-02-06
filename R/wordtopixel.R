library("tidyverse")
library("dplyr")
library("stringr")

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
pixelword <- function(word, type = "") {

  # extract letters of the word
  letters <- stringr::str_split(word, "")[[1]]

  # define vertical padding/spacing between letters
  vpad <- matrix(data = 0L, nrow = 11, ncol = 2)

  # initialize output banner
  pixel_banner <- vpad

  # consolidate letter matrices into a banner separated by padding
  for (letter in letters) {
    pixel_matrix <- matrix_gen(letter, type)
    pixel_banner <- cbind(pixel_banner, pixel_matrix, vpad)
  }

  # add top and bottom padding
  hpad <- matrix(data = 0L, nrow = 2, ncol = ncol(pixel_banner))
  pixel_banner <- rbind(hpad, pixel_banner, hpad)

  # assign column names as x01, x02, ...
  if (ncol(pixel_banner) > 99) {
    colnames(pixel_banner) <- paste0("x", 1:ncol(pixel_banner))
    colnames(pixel_banner)[1:9] <- paste0("x00", 1:9)
    colnames(pixel_banner)[10:99] <- paste0("x0", 10:99)
    print(colnames(pixel_banner))
  } else {
    colnames(pixel_banner) <- paste0("x", 1:ncol(pixel_banner))
    colnames(pixel_banner)[1:9] <- paste0("x0", 1:9)
    print(colnames(pixel_banner))
  }

  # assign row names as y01, y02, ...
  rownames(pixel_banner) <- paste0("y", 1:nrow(pixel_banner))
  rownames(pixel_banner)[1:9] <- paste0("y0", 1:9)

  # pivot banner to long data frame
  pixel_banner_l <- pixel_banner |>
    as.data.frame() |>
    rownames_to_column("y_axis") |>
    pivot_longer(-c(y_axis), names_to = "x_axis", values_to = "values")

  # call plot function on pixel banner elongated data
  banner <- pixelplot(pixel_banner_l, type)

  # save banner as png image
  ggsave("docs/banner.png",
         plot = banner,
         height = 20 * nrow(pixel_banner),
         width = 15 * ncol(pixel_banner),
         units = c("px"),
         dpi = 600)

return(banner)
}
