#' ---
#' title: Day12.R
#' author: Caleb Leedy
#' date: 12/12/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(readr)
library(dplyr)
library(stringr)

# *************
# * Read Data *
# *************

data <- read_delim("day12_exdata.txt", delim = "-", col_names = FALSE)
data <- read_delim("day12_ex2data.txt", delim = "-", col_names = FALSE)
data <- read_delim("data12_ex3data.txt", delim = "-", col_names = FALSE)
data <- read_delim("day12_data.txt", delim = "-", col_names = FALSE)

data_df <- bind_rows(data, tibble(X1 = data$X2, X2 = data$X1))

# *************
# * Problem 1 *
# *************

paths_to_end <- list()

#' @param nodes_visited A vector that cannot be empty
go_to_next_node <- function(nodes_visited, node_df = data_df) {

  # Stopping Conditions:
  # 1. At end node
  # 2. At a small letter twice

  # At end node
  if (nodes_visited[1] == "end") {
    paths_to_end[[length(paths_to_end) + 1]] <<- rev(nodes_visited)
    return(NULL)
  }

  # At small letter twice
  if (nodes_visited[1] %in% nodes_visited[-1]) {
    if (str_detect(nodes_visited[1], "[:lower:]")) {
      return(NULL)
    }
  }

  # Otherwise, go to the next node
  next_df <- data_df %>% filter(X1 == nodes_visited[1])

  for (i in 1:nrow(next_df)) {
    go_to_next_node(c(next_df$X2[i], nodes_visited), data_df)
  }

}

paths_to_end <- list()
go_to_next_node(c("start"), data_df)

paths_to_end

# *************
# * Problem 2 *
# *************

paths_to_end <- list()

#' @param nodes_visited A vector that cannot be empty
go_to_next_node <- function(nodes_visited, one_double_cave, node_df = data_df) {

  # Stopping Conditions:
  # 1. At end node
  # 2. Two small letter twice
  # 3. At start node

  # At end node
  if (nodes_visited[1] == "end") {
    paths_to_end[[length(paths_to_end) + 1]] <<- rev(nodes_visited)
    return(NULL)
  }

  # At start node
  if (length(nodes_visited) > 1 & nodes_visited[1] == "start") {
    return(NULL)
  }

  # Two small letters twice 
  if (nodes_visited[1] %in% nodes_visited[-1]) {
    if (str_detect(nodes_visited[1], "[:lower:]")) {
      if (one_double_cave) {
        return(NULL)
      } else {
        next_df <- data_df %>% filter(X1 == nodes_visited[1])
        for (i in 1:nrow(next_df)) {
          go_to_next_node(c(next_df$X2[i], nodes_visited), TRUE, data_df)
        }
        return(NULL)
      }
    }
  }

  # Otherwise, go to the next node
  next_df <- data_df %>% filter(X1 == nodes_visited[1])
  for (i in 1:nrow(next_df)) {
    go_to_next_node(c(next_df$X2[i], nodes_visited), one_double_cave, data_df)
  }

}

paths_to_end <- list()
go_to_next_node(c("start"), FALSE, data_df)

paths_to_end %>% length()

