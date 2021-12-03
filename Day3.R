#' ---
#' title: Day3.R
#' author: Caleb Leedy
#' date: 12/03/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(rlang)

# ********
# * Data *
# ********

data <- read_delim("day3_exdata.txt", delim = ",", col_names = FALSE)
data <- read_delim("day3_data.txt", delim = ",", col_names = FALSE)

# *************
# * Problem 1 *
# *************

# Gamma Rate
gam_bin_rate <- (sign(colSums(data) - (nrow(data) - colSums(data))) + 1) * 1 / 2

# Epsilon Rate
eps_bin_rate <- (sign((nrow(data) - colSums(data)) - colSums(data)) + 1) * 1 / 2

# Convert to Decimal
convert_to_decimal <- function(vec) {

  reversed_vec <- rev(vec)
  ret <- 0
  for (i in 1:length(vec)) {
    ret = ret + reversed_vec[i] * (2^(i-1))
  }

  return(ret)
}

convert_to_decimal(gam_bin_rate) * convert_to_decimal(eps_bin_rate)

# *************
# * Problem 2 *
# *************

tmp_data <- data
for (i in 1:ncol(data)) {

  bit_to_keep <- 
  round((sign(sum(tmp_data[, i]) - (nrow(tmp_data) - sum(tmp_data[, i]))) + 1) * 1 / 2 + 0.001)

  tmp_data <- filter(tmp_data, .data[[paste0("X", i)]] == bit_to_keep)

}

oxygen_rating <- convert_to_decimal(tmp_data[1, ])

tmp_data <- data
for (i in 1:ncol(data)) {

  pop_bit <- 
  round((sign(sum(tmp_data[, i]) - (nrow(tmp_data) - sum(tmp_data[, i]))) + 1) * 1 / 2 + 0.001)

  fewer_bit <- 1 - pop_bit

  tmp_data <- filter(tmp_data, .data[[paste0("X", i)]] == fewer_bit)

  if (nrow(tmp_data) == 1) {
    break()
  }
  
}

co2_rating <- convert_to_decimal(tmp_data[1, ])

oxygen_rating * co2_rating
