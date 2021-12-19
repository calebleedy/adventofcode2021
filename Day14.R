#' ---
#' title: Day14.R
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
library(Rcpp)

# *************
# * Read Data *
# *************

data <- read_delim("day14_exdata.txt", delim = ",", col_names = FALSE)
data <- read_delim("day14_data.txt", delim = ",", col_names = FALSE)

polymer <- data$X1[1]

conversion_df <- data[-1, ] %>%
  tidyr::separate(X1, into = c("from", "to"))

# *************
# * Problem 1 *
# *************

get_next_polymer <- function(start_pol, con_df) {
  end_pol <- str_sub(start_pol, 1, 1)
  for (i in 2:str_length(start_pol)) {
    end_pol <- 
      c(end_pol,
        filter(con_df, from == str_sub(start_pol, i - 1, i)) %>% pull(to),
        str_sub(start_pol, i , i))
  }
  str_flatten(end_pol)
}

num_days <- 10

for (i in 1:num_days) {
  polymer <- get_next_polymer(polymer, conversion_df)
}

str_length(polymer)

ret_df <- tibble(str = unlist(str_split(polymer, ""))) %>%
  group_by(str) %>%
  summarize(count = n()) %>%
  ungroup()

max(ret_df$count) - min(ret_df$count)

# *************
# * Problem 2 *
# *************

data <- read_delim("day14_exdata.txt", delim = ",", col_names = FALSE)
data <- read_delim("day14_data.txt", delim = ",", col_names = FALSE)

polymer <- data$X1[1]

conversion_df <- data[-1, ] %>%
  tidyr::separate(X1, into = c("from", "to"))

sourceCpp("Day14.cpp")

poly_vec <- unlist(str_split(polymer, ""))
conversion_df
poly_vec
num_days <- 40

grow_poly(polymer, conversion_df$from, conversion_df$to)

ret_poly <- grow_polymer_days(polymer, 40, conversion_df$from, conversion_df$to)

ret_df <- tibble(str = ret_poly) %>%
  group_by(str) %>%
  summarize(count = n()) %>%
  ungroup()

max(ret_df$count) - min(ret_df$count)










for (i in 1:num_days) {
  print(paste0("Day ", i, " of ", num_days))
  polymer <- get_next_polymer(polymer, conversion_df)
  saveRDS(polymer, paste0("polymer_day", i, ".RDS"))
}

str_length(polymer)

ret_df <- tibble(str = unlist(str_split(polymer, ""))) %>%
  group_by(str) %>%
  summarize(count = n()) %>%
  ungroup()

max(ret_df$count) - min(ret_df$count)


