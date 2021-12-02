#' ---
#' title: Day2.R
#' author: Caleb Leedy
#' date: 12/02/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)

# *************
# * Load Data *
# *************

day2 <- tibble(X1 = c("forward", "down", "forward", "up", "down", "forward"),
       X2 = c(5, 5, 8, 3, 8, 2))
day2 <- read_delim("day2_data.txt", delim = " ", col_names = FALSE)

# *************
# * Problem 1 *
# *************
# Multiply the final depth and position

tot_forward <- day2 %>%
  filter(X1 == "forward") %>%
  summarize(tot_forward = sum(X2)) %>%
  pull(tot_forward)

tot_down <- day2 %>%
  filter(X1 == "down") %>%
  summarize(tot_forward = sum(X2)) %>%
  pull(tot_forward)

tot_up <- day2 %>%
  filter(X1 == "up") %>%
  summarize(tot_forward = sum(X2)) %>%
  pull(tot_forward)

(tot_down - tot_up) * tot_forward

# *************
# * Problem 2 *
# *************

tot_depth <- day2 %>%
  mutate(ups = ifelse(X1 == "up", X2, 0),
         downs = ifelse(X1 == "down", X2, 0)) %>%
  mutate(cum_up = cumsum(ups),
         cum_down = cumsum(downs)) %>%
  mutate(cum_aim = cum_down - cum_up) %>%
  filter(X1 == "forward") %>%
  mutate(add_depth = X2 * cum_aim) %>%
  summarize(tot_depth = sum(add_depth)) %>%
  pull(tot_depth)

tot_depth * tot_forward
