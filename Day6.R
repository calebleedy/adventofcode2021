#' ---
#' title: Day6.R
#' author: Caleb Leedy
#' date: 12/06/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(Rmpfr)

# *************
# * Read Data *
# *************

data <- read_csv("day6_exdata.txt", col_names = FALSE)
data <- read_csv("day6_data.txt", col_names = FALSE)

data <- as.numeric(data[1, ])

get_counts <- function(vec) {
  c(length(which(vec == 0)),
    length(which(vec == 1)),
    length(which(vec == 2)),
    length(which(vec == 3)),
    length(which(vec == 4)),
    length(which(vec == 5)),
    length(which(vec == 6)),
    length(which(vec == 7)),
    length(which(vec == 8)))
}

data_df <- tibble(type = 0:8,
                  counts = get_counts(data))

# *************
# * Problem 1 *
# *************

sim_lanternfish <- function(counts, days = 80) {

  for (i in 1:days) {
    old_counts <- counts
    new_counts <- c(old_counts[-1], old_counts[1])
    new_counts[7] <- new_counts[7] + old_counts[1]
    counts <- new_counts
  }

  return(counts)

}

res <- sim_lanternfish(mpfr(data_df$counts, 20)) 
sum(res)

# *************
# * Problem 2 *
# *************

res <- sim_lanternfish(mpfr(data_df$counts, 60), days = 256) 
sum(res)


