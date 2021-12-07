#' ---
#' title: Day7.R
#' author: Caleb Leedy
#' date: 12/07/2021
#' output: pdf_document
#' ---

# *************
# * Libraries *
# *************

library(dplyr)
library(readr)

# *************
# * Read Data *
# *************

data <- read_csv("day7_exdata.txt", col_names = FALSE)
data <- read_csv("day7_data.txt", col_names = FALSE)

data <- as.numeric(data[1, ])

cur <- round(mean(data))
l_cur <- cur - 1
r_cur <- cur + 1

fuel_cost <- function(guess) {
  sum(abs(data - guess))
}

for (iter in 1:1000) {
  if ((fuel_cost(cur) < fuel_cost(l_cur)) & (fuel_cost(cur) < fuel_cost(r_cur))) {
    break
  } else {
    if (fuel_cost(cur) > fuel_cost(l_cur)) {
      cur <- l_cur
      l_cur <- cur - 1
      r_cur <- cur + 1
    } else # fuel_cost(cur) > fuel_cost(r_cur) {
      cur <- r_cur
      l_cur <- cur - 1
      r_cur <- cur + 1
  }
}

cur
fuel_cost(cur)
fuel_cost(l_cur)
fuel_cost(r_cur)


# *************
# * Problem 2 *
# *************

data <- read_csv("day7_exdata.txt", col_names = FALSE)
data <- read_csv("day7_data.txt", col_names = FALSE)

data <- as.numeric(data[1, ])

fuel_cost <- function(guess) {
  sum(unlist(purrr::map(abs(data - guess), function(num) {sum(0:num)})))
}

cur <- round(mean(data))
l_cur <- cur - 1
r_cur <- cur + 1


for (iter in 1:1000) {
  if ((fuel_cost(cur) < fuel_cost(l_cur)) & (fuel_cost(cur) < fuel_cost(r_cur))) {
    break
  } else {
    if (fuel_cost(cur) > fuel_cost(l_cur)) {
      cur <- l_cur
      l_cur <- cur - 1
      r_cur <- cur + 1
    } else # fuel_cost(cur) > fuel_cost(r_cur) {
      cur <- r_cur
      l_cur <- cur - 1
      r_cur <- cur + 1
  }
}

cur
fuel_cost(cur)
fuel_cost(l_cur)
fuel_cost(r_cur)

plot(450:500, unlist(purrr::map(450:500, fuel_cost)))
