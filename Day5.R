#' ---
#' title: Day5.R
#' author: Caleb Leedy
#' date: 12/06/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# *************
# * Read Data *
# *************

data <- read_delim("day5_data.txt", delim = "->", col_names = FALSE, show_col_types = FALSE) %>%
  mutate(X1 = str_trim(X1)) %>%
  mutate(X2 = str_trim(X2)) %>%
  tidyr::separate(X1, c("x1", "y1"), sep = ",") %>%
  tidyr::separate(X2, c("x2", "y2"), sep = ",") %>%
  mutate(across(everything(), .fns = as.numeric))

# *************
# * Problem 1 *
# *************

get_active_points <- function(x1, y1, x2, y2) {

  if ((x2 - x1) == 0) {
    ret <- tibble(x = x1, y = y1:y2)
  } else {
    slope <- (y2 - y1) / (x2 - x1)

    y_vec <- unlist(purrr::map(x1:x2, function(x) {slope * (x - x1) + y1}))
    ret <- tibble(x = x1:x2, y = y_vec)
  }

  ret
}

purrr::pmap_dfr(filter(data, x1 == x2 | y1 == y2), get_active_points) %>%
  group_by(x, y) %>%
  filter(n() > 1) %>%
  summarize() %>%
  nrow()

# *************
# * Problem 2 *
# *************

purrr::pmap_dfr(data, get_active_points) %>%
  group_by(x, y) %>%
  filter(n() > 1) %>%
  summarize() %>%
  nrow()
