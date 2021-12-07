#' ---
#' title: Day4.R
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

# *************
# * Read Data *
# *************

data <- readChar("day4_exdata.txt", file.info("day4_exdata.txt")$size)
data <- readChar("day4_data.txt", file.info("day4_data.txt")$size)

# **************
# * Problems 1 *
# **************

# Numbers Drawn
nums_drawn <- 
  as.numeric(unlist(str_extract_all(str_extract(data, "^.*\\n"), "[0-9]+")))

# Bingo Games
bingo_vecs <- purrr::map(str_split(data, "\\n\\n")[[1]][-1], function(vec) {
             as.numeric(unlist(str_extract_all(vec, "[0-9]+")))
})

# Simulate Game
for (i in 1:length(nums_drawn)) {
  # Draw Number
  number_drawn <- nums_drawn[i]

  # Mark vecs
  for (j in 1:length(bingo_vecs)) {
    if (number_drawn %in% bingo_vecs[[j]]) {
      bingo_vecs[[j]][which(bingo_vecs[[j]] == number_drawn)] <- NA 
    }
  }

  # Check for winner
  winning_board <- 0
  for (j in 1:length(bingo_vecs)) {
    bing_mat <- matrix(bingo_vecs[[j]], nrow = 5)

    # Check rows and cols
    for (x in 1:5) {
      if (all(is.na(bing_mat[x, ])) | all(is.na(bing_mat[, x]))) {
      # Assign winner
        winning_board <- j
      }
    }
  }

  # Find solution
  if (winning_board != 0) {
    ans <- sum(bingo_vecs[[winning_board]], na.rm = TRUE) * number_drawn
    print(paste0("The answer is ", ans))
    break()
  }

}

# *************
# * Problem 2 *
# *************

# Numbers Drawn
nums_drawn <- 
  as.numeric(unlist(str_extract_all(str_extract(data, "^.*\\n"), "[0-9]+")))

# Bingo Games
bingo_vecs <- purrr::map(str_split(data, "\\n\\n")[[1]][-1], function(vec) {
             as.numeric(unlist(str_extract_all(vec, "[0-9]+")))
})

winning_boards <- c()

# Simulate Game
for (i in 1:length(nums_drawn)) {
  # Draw Number
  number_drawn <- nums_drawn[i]

  # Mark vecs
  for (j in 1:length(bingo_vecs)) {
    if (number_drawn %in% bingo_vecs[[j]]) {
      bingo_vecs[[j]][which(bingo_vecs[[j]] == number_drawn)] <- NA 
    }
  }

  # Check for winner
  for (j in 1:length(bingo_vecs)) {
    bing_mat <- matrix(bingo_vecs[[j]], nrow = 5)

    # Check rows and cols
    for (x in 1:5) {
      if (all(is.na(bing_mat[x, ])) | all(is.na(bing_mat[, x]))) {
        # Add winner
        if (!(j %in% winning_boards)) {
          winning_boards <- c(j, winning_boards)
        }
      }
    }
  }

  # Find solution
  if (length(bingo_vecs) - length(winning_boards) == 0) {
    board <- winning_boards[1]
    ans <- sum(bingo_vecs[[board]], na.rm = TRUE) * number_drawn
    print(paste0("The answer is ", ans))
    break()
  }

}
