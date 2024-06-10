library(tibble)
library(dplyr)
library(readr)
library(lubridate)
library(tidymodels)
library(randomForest)
library(ggplot2)
library(ranger)

options(width = 120)

# Read the data
#I would like to add data about crime rate, school rankings, subjective niceness of the property, and more
data <- read_csv("FortWayneRealEstate.csv", col_names = TRUE)
data$zip_code <- as.factor(data$zip_code) #Changing zip_code to a factor
unfiltereddata <- data
# Filter and clean data
data <- data %>%
  mutate(list_date = mdy(list_date)) %>%
  filter(
    style == 'SINGLE_FAMILY',
    year(list_date) %in% c(2020:2024),
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
    lot_sqft, latitude, longitude, stories, hoa_fee, parking_garage
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
  slice_head(n = 10) %>% left_join(unfiltereddata, by = c("latitude","longitude"))

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





#Making a prediction

# Define the new house parameters as a data frame
new_house <- tibble(
  sqft = 1248,
  zip_code = 46819,
  beds = 4,
  full_baths = 1,
  half_baths = 0,
  year_built = 1949,
  lot_sqft = 12196,
  latitude = 41.0185,
  longitude = -85.1678,
  stories = 2,
  hoa_fee = 0,
  parking_garage = 1
)

# Preprocess the new house data using the prepared recipe
new_house_processed <- prepared_recipe %>%
  bake(new_data = new_house)

# Make prediction
new_house_prediction <- predict(house_workflow, new_data = new_house_processed)

# Display the prediction
print(new_house_prediction)


