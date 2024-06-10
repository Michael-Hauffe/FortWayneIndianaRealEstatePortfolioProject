library(tibble)
library(dplyr)
library(readr)
library(lubridate)
library(tidymodels)
library(ggplot2)
library(broom)

# Read the data
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)

# Filter and clean data
data <- data %>%
  mutate(list_date = mdy(list_date)) %>%
  filter(
    style == 'SINGLE_FAMILY',
    year(list_date) %in% c(2022:2024),
    !is.na(sold_price), 
    !is.na(list_price)
  )

# Split the data into training and testing sets
split <- initial_split(data, prop = 0.8)

train_data <- training(split)
test_data <- testing(split)

# Define the recipe
house_recipe <- recipe(sold_price ~ list_price, data = train_data)

# Define the model specification
house_spec <- linear_reg()

# Train the model
house_workflow <- workflow() %>%
  add_recipe(house_recipe) %>%
  add_model(house_spec) %>%
  fit(data = train_data)

# Prepare the recipe
prepared_recipe <- house_recipe %>%
  prep()

# Apply the prepared recipe to the testing data
test_data_processed <- prepared_recipe %>%
  bake(new_data = test_data)

# Make predictions on the processed testing data
predictions <- predict(house_workflow, new_data = test_data_processed)

# Calculate evaluation metrics
mae <- mean(abs(predictions$.pred - test_data$sold_price))
mse <- mean((predictions$.pred - test_data$sold_price)^2)
rsquared <- cor(predictions$.pred, test_data$sold_price)^2

print(paste("Mean Absolute Error:", mae))
print(paste("Mean Squared Error:", mse))
print(paste("R-squared:", rsquared))

# Extract the slope from the model using broom
coefficients <- tidy(house_workflow$fit$fit)
slope <- coefficients$estimate[coefficients$term == "list_price"]

# Print the slope
print(paste("Slope:", slope))

# Visualization
ggplot(test_data, aes(x = list_price, y = sold_price)) +
  geom_point() +
  labs(x = "List Price", y = "Sold Price") +
  ggtitle("Relationship between List Price and Sold Price") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_text(x = max(test_data$list_price), y = max(test_data$sold_price), 
            label = paste("Slope =", round(slope, 2)), hjust = 1, vjust = -1, color = "red")

# Extract the coefficient estimate for list_price
coefficient_estimate <- coefficients$estimate[coefficients$term == "list_price"]

# Extract the standard error for list_price
standard_error <- coefficients$std.error[coefficients$term == "list_price"]

# Calculate the degrees of freedom
n <- nrow(test_data_processed)
p <- 1  # number of predictor variables (list_price)
df <- n - p - 1  # degrees of freedom

# Calculate the t-value for a 95% confidence interval
t_value <- qt(0.975, df)

# Calculate the margin of error
margin_of_error <- t_value * standard_error

# Calculate the lower and upper bounds of the confidence interval
lower_bound <- coefficient_estimate - margin_of_error
upper_bound <- coefficient_estimate + margin_of_error

# Print the confidence interval
print(paste("95% Confidence Interval for the effect of list_price on sold_price: [", 
            round(lower_bound, 2), ",", round(upper_bound, 2), "]"))
