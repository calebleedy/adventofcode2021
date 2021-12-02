#' ---
#' title: Day1.R
#' author: Caleb Leedy
#' date: 12/01/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(readr)
library(dplyr)

# ********
# * Data *
# ********
# Test Data
puzz1 <- tibble(X1 = c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263))

puzz1 <- read_csv("day1_data.csv", col_names = FALSE)

# *******************
# * Get Differences *
# *******************

puzz1 %>%
  mutate(oneless = lag(X1)) %>%
  mutate(diff = X1 - oneless) %>%
  mutate(signdiff = sign(diff)) %>%
  filter(signdiff > 0) %>%
  nrow()

# ******************
# * Sliding Window *
# ******************

puzz1 %>%
  mutate(onemore = lead(X1),
         twomore = lead(onemore)) %>%
  mutate(movavg = X1 + onemore + twomore) %>%
  mutate(X1 = movavg) %>%
  mutate(oneless = lag(X1)) %>%
  mutate(diff = X1 - oneless) %>%
  mutate(signdiff = sign(diff)) %>%
  filter(signdiff > 0) %>%
  nrow()

