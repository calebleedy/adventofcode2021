# Day8.R
# Author: Caleb Leedy
# Date Created: 12/8/2021
# Purpose:
# This document contains code for solving the puzzles for Day 8 of the Advent of
# Code challenge.

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ************
# * Get Data *
# ************

data <- read_delim("day8_exdata.txt", delim = "|", col_names = FALSE)
data <- read_delim("day8_ex2data.txt", delim = "|", col_names = FALSE)
data <- read_delim("day8_data.txt", delim = "|", col_names = FALSE)

names(data) <- c("input", "output")

clean_data <- data %>%
  mutate(output = str_trim(output)) %>%
  mutate(input = str_trim(input)) %>%
  separate(output, paste0("out", 1:4)) %>%
  separate(input, paste0("in", 1:10))

# *************
# * Problem 1 *
# *************

clean_data %>%
  dplyr::select(starts_with("out")) %>%
  pivot_longer(out1:out4, names_to = "type", values_to = "output") %>%
  mutate(out_len = str_length(output)) %>%
  filter(out_len %in% c(2, 4, 3, 7)) %>%
  nrow()

# *************
# * Problem 2 *
# *************

order_str <- function(str) {
  unlist(str_split(str, "")) %>% str_sort() %>% str_c(sep = "", collapse = "")
}

get_abcdefg <- function(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10) {

  signals <- c(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10)
  num_1 <- signals[which(str_length(signals) == 2)]
  num_4 <- signals[which(str_length(signals) == 4)]
  num_7 <- signals[which(str_length(signals) == 3)]
  num_8 <- signals[which(str_length(signals) == 7)]
  num_069 <- signals[which(str_length(signals) == 6)]
  num_235 <- signals[which(str_length(signals) == 5)]

  tochar <- function(str) {unlist(str_split(str, ""))}

  all_069 <- purrr::reduce(purrr::map(num_069, tochar), base::intersect)

  a <- setdiff(tochar(num_7), tochar(num_1))
  b <- setdiff(intersect(all_069, tochar(num_4)), tochar(num_1))
  c <- setdiff(tochar(num_1), all_069)
  d <- setdiff(tochar(num_4), union(tochar(num_1), all_069))
  e <- setdiff(tochar(num_8), union(union(tochar(num_4), tochar(num_7)), all_069))
  f <- intersect(tochar(num_1), all_069)
  g <- setdiff(all_069, union(tochar(num_4), tochar(num_7)))

  return(list(a = a, b = b, c = c, d = d, e = e, f = f, g = g))
}

get_out_val <- function(out_str, letter_vals) {
  str <- order_str(out_str)
  lv <- letter_vals
  case_when(str == order_str(paste0(lv$a, lv$b, lv$c, lv$e, lv$f, lv$g)) ~ 0,
            str == order_str(paste0(lv$c, lv$f)) ~ 1,
            str == order_str(paste0(lv$a, lv$c, lv$d, lv$e, lv$g)) ~ 2,
            str == order_str(paste0(lv$a, lv$c, lv$d, lv$f, lv$g)) ~ 3,
            str == order_str(paste0(lv$b, lv$c, lv$d, lv$f)) ~ 4,
            str == order_str(paste0(lv$a, lv$b, lv$d, lv$f, lv$g)) ~ 5,
            str == order_str(paste0(lv$a, lv$b, lv$d, lv$e, lv$f, lv$g)) ~ 6,
            str == order_str(paste0(lv$a, lv$c, lv$f)) ~ 7,
            str == order_str(paste0(lv$a, lv$b, lv$c, lv$d, lv$e, lv$f, lv$g)) ~ 8,
            str == order_str(paste0(lv$a, lv$b, lv$c, lv$d, lv$f, lv$g)) ~ 9)
}


get_digits <- function(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10,
                       out1, out2, out3, out4) {

  letter_vals <- get_abcdefg(in1, in2, in3, in4, in5, in6, in7, in8, in9, in10)

  out1_val <- get_out_val(out1, letter_vals)
  out2_val <- get_out_val(out2, letter_vals)
  out3_val <- get_out_val(out3, letter_vals)
  out4_val <- get_out_val(out4, letter_vals)

  return(as.numeric(paste0(out1_val, out2_val, out3_val, out4_val)))

}


# Test
purrr::pmap(clean_data, get_digits) %>% unlist() %>% sum()
