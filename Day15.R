#' ---
#' title: Day15.R
#' author: Caleb Leedy
#' date: 12/16/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(Rcpp)
library(RcppArmadillo)

# *************
# * Read Data *
# *************

data <- read_csv("day15_exdata.txt", col_names = FALSE)
data <- read_csv("day15_data.txt", col_names = FALSE)

data_mat <- as.matrix(data)

# *************
# * Problem 1 *
# *************

sourceCpp("day15.cpp")

old_cost_mat <- matrix(9999, nrow = nrow(data_mat), ncol = ncol(data_mat))
cost_mat <- get_path_weight(data_mat, old_cost_mat)

while (!identical(cost_mat, old_cost_mat)) {

  old_cost_mat <- cost_mat
  cost_mat <- get_path_weight(data_mat, old_cost_mat)

}

cost_mat[nrow(cost_mat), ncol(cost_mat)]

# *************
# * Problem 2 *
# *************

new_data_mat  <- rbind(cbind(data_mat, ((data_mat - 1 + 1) %% 9) + 1,
      ((data_mat - 1 + 2) %% 9) + 1, ((data_mat - 1 + 3) %% 9) + 1,
      ((data_mat - 1 + 4) %% 9) + 1),
cbind(((data_mat - 1 + 1) %% 9) + 1, ((data_mat - 1 + 2) %% 9) + 1,
      ((data_mat - 1 + 3) %% 9) + 1, ((data_mat - 1 + 4) %% 9) + 1,
      ((data_mat - 1 + 5) %% 9) + 1),
cbind(((data_mat - 1 + 2) %% 9) + 1, ((data_mat - 1 + 3) %% 9) + 1,
      ((data_mat - 1 + 4) %% 9) + 1, ((data_mat - 1 + 5) %% 9) + 1,
      ((data_mat - 1 + 6) %% 9) + 1),
cbind(((data_mat - 1 + 3) %% 9) + 1, ((data_mat - 1 + 4) %% 9) + 1,
      ((data_mat - 1 + 5) %% 9) + 1, ((data_mat - 1 + 6) %% 9) + 1,
      ((data_mat - 1 + 7) %% 9) + 1),
cbind(((data_mat - 1 + 4) %% 9) + 1, ((data_mat - 1 + 5) %% 9) + 1,
      ((data_mat - 1 + 6) %% 9) + 1, ((data_mat - 1 + 7) %% 9) + 1,
      ((data_mat - 1 + 8) %% 9) + 1))


old_cost_mat <- matrix(999999, nrow = nrow(new_data_mat), ncol = ncol(new_data_mat))
cost_mat <- get_path_weight(new_data_mat, old_cost_mat)

for (i in 1:250000) {

  old_cost_mat <- cost_mat
  cost_mat <- get_path_weight(data_mat, old_cost_mat)

}

# 2830 too high, 2800 too low, 2825 too high
cost_mat[nrow(cost_mat), ncol(cost_mat)]





