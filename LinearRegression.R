library(rvest)
library(tibble)
library(httr)
library(purrr)
library(dplyr)
library(readr)
library(lubridate)
library(tidymodels)
library(ggplot2)

# Read the data
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)

# Remove rows with missing sold_price
data <- data %>% filter(!is.na(sold_price), !is.na(list_price))

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

# Check the predictions
print(head(predictions))

# Check the test data 
print(head(test_data_processed))

# Visualization
ggplot(test_data, aes(x = list_price, y = sold_price)) +
  geom_point() +
  labs(x = "List Price", y = "Sold Price") +
  ggtitle("Relationship between List Price and Sold Price") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
