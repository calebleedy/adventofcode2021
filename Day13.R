#' ---
#' title: Day13.R
#' author: Caleb Leedy
#' date: 12/14/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(rlang)

# *************
# * Read Data *
# *************

data <- read_csv("day13_exdata.txt", col_names = FALSE)
data <- read_csv("day13_data.txt", col_names = FALSE)

folds_df <- filter(data, is.na(X2)) %>%
  dplyr::select(-X2) %>%
  mutate(X1 = str_replace(X1, "fold along ", "")) %>%
  tidyr::separate(X1, into = c("dim", "value")) %>%
  mutate(value = as.numeric(value))

points_df <- filter(data, !is.na(X2)) %>%
  mutate(x = as.numeric(X1),
         y = as.numeric(X2)) %>%
  dplyr::select(x, y)

# *************
# * Problem 1 *
# *************

apply_fold <- function(df, dim, value) {

  df[[dim]] <- ifelse(df[[dim]] > value, value - (df[[dim]] - value), df[[dim]])

  distinct(df)
}

apply_fold(points_df, folds_df$dim[1], folds_df$value[1])

# *************
# * Problem 3 *
# *************

ret_df <- points_df
for (i in 1:nrow(folds_df)) {
  ret_df <- apply_fold(ret_df, folds_df$dim[i], folds_df$value[i])
}

ret_df

grid_mat <- matrix(FALSE, nrow = max(ret_df$y) + 1, ncol = max(ret_df$x) + 1)

for (i in 1:nrow(ret_df)) {

  grid_mat[ret_df$y[i] + 1, ret_df$x[i] + 1] <- TRUE

}

image(grid_mat)
