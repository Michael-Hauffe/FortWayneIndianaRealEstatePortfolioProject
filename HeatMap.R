library(ggcorrplot)
library(readr)
library(dplyr)
library(lubridate)

options(width = 120)

# Read the data
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)
unfiltereddata <- data
# Filter and clean data
data <- data %>%
  mutate(list_date = mdy(list_date)) %>%
  filter(
    style == 'SINGLE_FAMILY',
    year(list_date) %in% c(2022:2024),
    list_price < 300000,
    !is.na(sqft),
    !is.na(latitude),
    !is.na(longitude),
    !is.na(stories),
    !is.na(lot_sqft),
    !is.na(full_baths),
    !is.na(half_baths),
    !is.na(parking_garage),
    !is.na(sold_price)
  ) %>%
  select(
    sqft, zip_code, beds, full_baths, half_baths, year_built, sold_price,
    lot_sqft, price_per_sqft, latitude, longitude, stories, hoa_fee, parking_garage
  )

# Replace hoa_fee null values with 0
data <- data %>%
  mutate(hoa_fee = ifelse(is.na(hoa_fee), 0, hoa_fee))

# Calculate the correlation matrix
correlation_matrix <- cor(data)

# Plot correlation heatmap
ggcorrplot(correlation_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           title = "Correlation Heatmap of Variables with Sold Price",
           ggtheme = ggplot2::theme_minimal(),
           lab_size = 3)  # Adjust the font size as needed

