library(readr)
library(dplyr)
library(ggplot2)

# Read the data
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)

# Filter and clean data
data_clean <- data %>%
  filter(
    style == 'SINGLE_FAMILY',
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
data_clean <- data_clean %>%
  mutate(hoa_fee = ifelse(is.na(hoa_fee), 0, hoa_fee))

# Calculate correlations with sold_price
correlation_with_sold_price <- cor(data_clean)[, "sold_price"]

# Create a dataframe for plotting
correlation_df <- tibble(variable = names(correlation_with_sold_price), correlation = correlation_with_sold_price)

max_freq <- min(table(correlation_df$correlation))
print(correlation_df$correlation)

# Create bar chart
ggplot(correlation_df, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(correlation, 2)), vjust = -0.5) +
  labs(title = "Correlation of Variables with Sold Price",
       x = "Variable", y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

