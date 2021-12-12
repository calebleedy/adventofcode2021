#' ---
#' title: Day10.R
#' author: Caleb Leedy
#' date: 12/10/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(stringr)

# *************
# * Load Data *
# *************

data <- read_csv("day10_exdata.txt", col_names = FALSE)
data <- read_csv("day10_data.txt", col_names = FALSE)

# *************
# * Problem 1 *
# *************

get_right_delim <- function(delim_char) {
  case_when(
    delim_char == "(" ~ ")",
    delim_char == "[" ~ "]",
    delim_char == "{" ~ "}",
    delim_char == "<" ~ ">"
  )
}

get_cost <- function(delim_char) {
  case_when(
    delim_char == ")" ~ 3,
    delim_char == "]" ~ 57,
    delim_char == "}" ~ 1197,
    delim_char == ">" ~ 25137
  )
}

get_problem <- function(str) {

  str_vec <- str_split(str, "")[[1]]

  char_stack <- character(0)
  left_parens <- c("(", "{", "[", "<")
  right_parens <- c(")", "}", "]", "<")
  is_corrupt <- FALSE
  error_char <- ""

  for (i in 1:length(str_vec)) {
    if (str_vec[i] %in% left_parens) {
      char_stack <- c(str_vec[i], char_stack)
    } else { # right_parens
      if (str_vec[i] == get_right_delim(char_stack[1])) { # Correct match
        char_stack <- char_stack[-1]
      } else { # Error
        is_corrupt <- TRUE
        error_char <- str_vec[i]
        break
      }
    }
  }

  return(list(is_corrupt = is_corrupt,
              error_char = error_char
              stack_lst = list(char_stack)))
}

purrr::map_dfr(data$X1, get_problem) %>%
  filter(is_corrupt) %>%
  mutate(cost = get_cost(error_char)) %>%
  pull(cost) %>%
  sum()

# *************
# * Problem 2 *
# *************

value_to_add <- function(char) {
  case_when(
    char == "(" ~ 1,
    char == "[" ~ 2,
    char == "{" ~ 3,
    char == "<" ~ 4,
  )
}

get_points <- function(char_vec) {

  score <- 0

  for (i in 1:length(char_vec)) {
    score <- score * 5
    score <- score + value_to_add(char_vec[i])
  }

  return(list(score = score))

}

res_df <- purrr::map_dfr(data$X1, get_problem) %>%
  filter(!is_corrupt) 

score_df <- purrr::map_dfr(res_df$stack_lst, get_points) %>%
  arrange(score)

score_df$score[(nrow(score_df) + 1) / 2]
