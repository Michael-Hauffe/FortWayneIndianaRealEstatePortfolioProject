library(tibble)
library(dplyr)
library(readr)
library(lubridate)
library(tidymodels)
library(randomForest)  # Load the randomForest library
library(ggplot2)
library(ranger)

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


# Split the data into training and testing sets
split <- initial_split(data, prop = 0.8)

train_data <- training(split)
test_data <- testing(split)

# Define the recipe
house_recipe <- recipe(sold_price ~ ., data = train_data)

# Define the model specification for Random Forest
house_spec <- rand_forest() %>%
  set_mode("regression")  # Set the mode to "regression"

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

# Combine predictions with test data
results <- test_data %>%
  bind_cols(predictions)

# Calculate residuals and differences
results <- results %>%
  mutate(residual = abs(sold_price - .pred), difference = sold_price - .pred)

# Find the biggest outlier and add back some original data for identification
biggest_outliers <- results %>%
  arrange(desc(residual)) %>%
  slice_head(n = 10) %>% left_join(unfiltereddata)

#Make a table of the biggest outliers for manual review
view(biggest_outliers)

# Calculate evaluation metrics
mae <- mean(abs(results$.pred - results$sold_price))
mse <- mean((results$.pred - results$sold_price)^2)
rsquared <- cor(results$.pred, results$sold_price)^2

print(paste("Mean Absolute Error:", mae))
print(paste("Mean Squared Error:", mse))
print(paste("R-squared:", rsquared))

# Check the predictions
print(head(predictions))

# Check the test data 
print(head(test_data_processed))

# Visualization of prediction vs sold_price
ggplot(results, aes(x = .pred, y = sold_price)) +
  geom_point(color = "blue") +
  labs(x = "Predicted Price", y = "Sold Price") +
  ggtitle("Predicted vs Sold Price")
