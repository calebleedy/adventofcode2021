#' ---
#' title: Day9.R
#' author: Caleb Leedy
#' date: 12/09/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)

# *************
# * Read Data *
# *************

data <- read_csv("day9_exdata.txt", col_names = FALSE)
data <- read_csv("day9_data.txt", col_names = FALSE)

data <- as.matrix(data)

# *************
# * Problem 1 *
# *************

get_low_points <- function(mat) {

  pad_mat <- matrix(10, nrow = nrow(mat) + 2, ncol = ncol(mat) + 2)
  pad_mat[2:(nrow(mat) + 1), 2:(ncol(mat) + 1)] <- mat
  ret <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))

  for (i in 2:(nrow(mat) + 1)) {
    for (j in 2:(ncol(mat) + 1)) {
      is_lower <- pad_mat[i, j] < min(c(pad_mat[i - 1, j], pad_mat[i, j - 1],
                                        pad_mat[i + 1, j], pad_mat[i, j + 1]))
      if (is_lower) {
        ret[i - 1, j - 1] <- TRUE
      }

    }
  }

  return(ret)

}

sum(as.vector(data)[as.vector(get_low_points(data))] + 1)

# *************
# * Problem 2 *
# *************

low_mat <- get_low_points(data)

get_basin_points <- function(vec_ind, mat) {

  # These are padded points
  x <- ((vec_ind - 1) %% nrow(mat) + 1) + 1
  y <- ((vec_ind - 1) %/% nrow(mat) + 1) + 1

  pad_mat <- matrix(9, nrow = nrow(mat) + 2, ncol = ncol(mat) + 2)
  pad_mat[2:(nrow(mat) + 1), 2:(ncol(mat) + 1)] <- mat

  points_in_basin <- tibble(x = numeric(0), y = numeric(0))
  points_tocheck <- tibble(x = c(x), y = c(y))
  points_checked <- tibble(x = numeric(0), y = numeric(0))

  while(nrow(points_tocheck) != 0) {
    test_x <- points_tocheck$x[1]
    test_y <- points_tocheck$y[1]

    if (pad_mat[test_x, test_y] != 9) {
      points_in_basin <- add_row(points_in_basin, x = test_x - 1, y = test_y - 1)
      points_checked <- add_row(points_checked, x = test_x, y = test_y)
      points_tocheck <- points_tocheck[-1, ]
      points_to_add <- tibble(x = c(test_x - 1, test_x, test_x, test_x + 1),
                              y = c(test_y, test_y - 1, test_y + 1, test_y))
      points_tocheck <- 
        anti_join(distinct(bind_rows(points_tocheck, points_to_add)), points_checked)
    } else {
      points_tocheck <- points_tocheck[-1, ]
      points_checked <- add_row(points_checked, x = test_x, y = test_y)
    }
  }

  return(points_in_basin)

}

vec_ind <- as.vector(which(low_mat))
get_basin_points(vec_ind[1], data)

basin_sizes <- purrr::map(vec_ind, function(vec) {get_basin_points(vec, data)}) %>%
  purrr::map(nrow) %>%
  unlist()

prod(basin_sizes[base::order(basin_sizes, decreasing = TRUE)][1:3])
