library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Read the data
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)

# Filter and clean data
clean_data <- data %>%
  mutate(list_date = mdy(list_date)) %>% #correct list_date format
  mutate(hoa_fee = ifelse(is.na(hoa_fee), 0, hoa_fee)) # Replace hoa_fee null values with 0

# Select only numeric columns
numeric_data <- clean_data %>% select_if(is.numeric)

# Calculate correlations with sold_price using only non-null values
correlation_with_sold_price <- cor(numeric_data, use = "pairwise.complete.obs")[, "sold_price"]

# Create a dataframe for plotting
correlation_df <- tibble(variable = names(correlation_with_sold_price), correlation = correlation_with_sold_price)

# Create bar chart
ggplot(correlation_df, aes(x = variable, y = correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(correlation, 2)), vjust = -0.5) +
  labs(title = "Correlation of Variables with Sold Price",
       x = "Variable", y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
