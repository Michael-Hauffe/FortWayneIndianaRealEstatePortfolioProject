library(rvest)
library(tibble)
library(httr)
library(purrr)
library(dplyr)
library(readr)
library(lubridate)

data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)
class(data$list_price)
temp <- data %>% filter(year(last_sold_date) == 2024, !is.na(list_price)) %>% summarize(mean(list_price))
temp


