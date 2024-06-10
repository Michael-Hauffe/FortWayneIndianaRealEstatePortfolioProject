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


# Calculate percent discount
data <- data %>%
  mutate(percent_discount = 100 * (sold_price - list_price) / list_price)

# Mean percent discount
mean_percent_discount <- mean(data$percent_discount, na.rm = TRUE)

# Standard deviation
sd_percent_discount <- sd(data$percent_discount, na.rm = TRUE)

# Sample size
n <- sum(!is.na(data$percent_discount))

# Critical t value
t_value <- qt(0.975, df = n - 1)

# Margin of error
margin_of_error <- t_value * (sd_percent_discount / sqrt(n))

# Confidence interval
lower_bound <- mean_percent_discount - margin_of_error
upper_bound <- mean_percent_discount + margin_of_error

# Output the confidence interval
ci <- tibble(
  lower_bound = lower_bound,
  mean = mean_percent_discount,
  upper_bound = upper_bound
)
print(ci)
