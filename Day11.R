#' ---
#' title: Day11.R
#' author: Caleb Leedy
#' date: 12/11/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)
library(purrr)

# *************
# * Read Data *
# *************

data_df <- read_csv("day11_exdata.txt", col_names = FALSE)
data_df <- read_csv("day11_exdata2.txt", col_names = FALSE)
data_df <- read_csv("day11_data.txt", col_names = FALSE)

data_mat <- as.matrix(data_df)

# *************
# * Problem 1 *
# *************

#' @param day This is an integer from 1 to n and refers to the number of flashes
#' after the day listed
get_flashes <- function(day, mat = data_mat) {

  power_mat <- mat
  tot_flashes <- 0

  for (d in seq_len(day)) {
    # print(paste0("Day ", d, " of ", day))

    # Initialize each day
    points_to_flash <- tibble(x = numeric(0), y = numeric(0), ind = numeric(0))
    points_flashed <- tibble(x = numeric(0), y = numeric(0), ind = numeric(0))
    points_to_check <- tibble(x = numeric(0), y = numeric(0), ind = numeric(0))

    # Step 1 of each day
    power_mat <- power_mat + 1

    # Step 2 check to see what flashes
    ind_to_flash <- which(as.vector(power_mat) > 9)

    # add to points_to_flash
    while (length(ind_to_flash) > 0) {
      points_to_flash <- 
        add_row(points_to_flash,
                 x = (ind_to_flash[1] - 1) %% nrow(power_mat) + 1,
                 y = (ind_to_flash[1] - 1) %/% nrow(power_mat) + 1,
                 ind = ind_to_flash[1])
      ind_to_flash <- ind_to_flash[-1]
    }

    while (nrow(points_to_flash) > 0) {
      # Flash points (check with points_flashed); update power_mat; move points
      # to points_flashed
      # Flash points
      if (points_to_flash$ind[1] %in% points_flashed$ind) {
        points_to_flash <- points_to_flash[-1, ]
      } else {
        # Flash Points
        x <- points_to_flash$x[1]
        y <- points_to_flash$y[1]
        power_to_add <- 
        tibble(x = c(x - 1, x - 1, x - 1, x, x, x + 1, x + 1, x + 1),
               y = c(y - 1, y, y + 1, y - 1, y + 1, y - 1, y, y + 1))

        power_to_add <- 
          filter(power_to_add, x >= 1, y >= 1,
                 x <= nrow(power_mat), y <= ncol(power_mat))

        power_mat[power_to_add$x, power_to_add$y] <-
          power_mat[power_to_add$x, power_to_add$y] + 1

        # Move Points to points_flashed
        points_flashed <- 
          add_row(points_flashed, x = x, y = y, ind = (y - 1) * nrow(power_mat) + x)
        points_to_flash <- points_to_flash[-1, ]

        # Check to see if more points need to flash (check with points_flashed)
        points_to_check <- power_to_add %>%
          mutate(power_lev = unlist(purrr::map2(x, y, function(x, y) {power_mat[x, y]}))) %>%
          filter(power_lev > 9) %>%
          anti_join(points_flashed, by = c("x", "y")) %>%
          mutate(ind = (y - 1) * nrow(power_mat) + x) %>%
          dplyr::select(x, y, ind)

        points_to_flash <- distinct(bind_rows(points_to_flash, points_to_check))
      }
    }

    # Zero power_mat points that have flashed
    for (i in 1:nrow(points_flashed)) {
      power_mat[points_flashed$x[i], points_flashed$y[i]] <- 0
    }

    tot_flashes <- tot_flashes + nrow(points_flashed)

  }

  return(list(day = day, power_mat = power_mat, tot_flashes = tot_flashes))

}

get_flashes(100, data_mat)

# *************
# * Problem 2 *
# *************

power_mat <- data_mat 
day <- 1000

for (d in 1:day) {
  print(paste0("Day ", d, " of ", day))
  power_mat <- get_flashes(1, power_mat)$power_mat

  if (all(as.numeric(power_mat) == 0)) {
    print(paste0("All flash on day ", d))
    break()
  }
}
