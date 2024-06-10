library(rvest)
library(tibble)
library(dplyr)
library(readr)
library(stringr)

main <- function(url, zipcode, initial_delay = 60*5) {
  retry_delay <- initial_delay
  tryCatch({
    # Attempt to read the HTML content
    h <- read_html(url)
    
    # Extract relevant data
    nodes <- h %>% html_elements("table") %>% html_text()
    
    if (length(nodes) >= 4) {
      violentcrime <- nodes[2]
      propertycrime <- nodes[3]
      othercrime <- nodes[4]
      
      # Function to find the crime rate
      find_crime_rate <- function(crime, crime_type) {
        #searches for a positive number of digits or periods that appear immediately after a newline character. It captures these digits or periods as a group.
        pattern <- paste0(crime, "\\n([0-9\\.]+)") 
        match <- str_match(crime_type, pattern)
        
        #Checks if the data is as expected
        if (is.na(match[2])) {
          return(NA)
        } else {
          return(as.numeric(match[2]))
        }
      }
      
      # Extract crime rates
      Assault <- find_crime_rate("Assault", violentcrime)
      Robbery <- find_crime_rate("Robbery", violentcrime)
      Rape <- find_crime_rate("Rape", violentcrime)
      Murder <- find_crime_rate("Murder", violentcrime)
      TotalViolentCrime <- find_crime_rate("Total Violent Crime", violentcrime)
      Theft <- find_crime_rate("Theft", propertycrime)
      VehicleTheft <- find_crime_rate("Vehicle Theft", propertycrime)
      Burglary <- find_crime_rate("Burglary", propertycrime)
      Arson <- find_crime_rate("Arson", propertycrime)
      TotalPropertyCrime <- find_crime_rate("Total Property Crime", propertycrime)
      Kidnapping <- find_crime_rate("Kidnapping", othercrime)
      DrugCrimes <- find_crime_rate("Drug Crimes", othercrime)
      Vandalism <- find_crime_rate("Vandalism", othercrime)
      IdentityTheft <- find_crime_rate("Identity Theft", othercrime)
      AnimalCruelty <- find_crime_rate("Animal Cruelty", othercrime)
      TotalOtherRate <- find_crime_rate('Total “Other” Rate', othercrime)
      
      # Create a data frame with the extracted data
      data <- tibble(
        zipcode = zipcode,
        Assault = Assault,
        Robbery = Robbery,
        Rape = Rape,
        Murder = Murder,
        TotalViolentCrime = TotalViolentCrime,
        Theft = Theft,
        VehicleTheft = VehicleTheft,
        Burglary = Burglary,
        Arson = Arson,
        TotalPropertyCrime = TotalPropertyCrime,
        Kidnapping = Kidnapping,
        DrugCrimes = DrugCrimes,
        Vandalism = Vandalism,
        IdentityTheft = IdentityTheft,
        AnimalCruelty = AnimalCruelty,
        TotalOtherRate = TotalOtherRate
      )
      
      # Define the file path
      data_file <- "C:\\Users\\cfmon\\OneDrive\\Documents\\IndianaCrimeStats.csv"
      
      # Check if the file exists and if it has content
      if (file.exists(data_file)) {
        if (file.size(data_file) > 0) {
          write_headers <- FALSE
        } else {
          write_headers <- TRUE
        }
      } else {
        write_headers <- TRUE
      }
      
      # Write the data to the CSV file
      write_csv(data, data_file, append = TRUE, col_names = write_headers)
    } else {
      cat("No relevant tables found for URL:", url, "\n")
    }
  }, error = function(e) {
    if (grepl("HTTP error 429", e$message)) {
      cat("Rate limit exceeded. Waiting", retry_delay, "seconds before retrying...\n")
      Sys.sleep(retry_delay) # Wait before retrying
      # Increase delay exponentially for each retry to avoid further limits
      retry_delay <- min(retry_delay * 2, 3600) # Cap the wait time at 1 hour
      main(url, zipcode, retry_delay) # Retry the function
    } else {
      cat("Error: ", e$message, "\n")
    }
  })
}

# Read the CSV file containing zip codes
zipcode_data <- read_csv("C:\\Users\\cfmon\\OneDrive\\Documents\\FortWayneIndianaRealEstatePortfolioProject\\FortWayneRealEstate.csv")

# Extract unique zip codes
zipcodelist <- unique(zipcode_data$zip_code)

# Loop through each unique zip code and process
for (zipcode in zipcodelist) {
  url <- paste0("https://crimegrade.org/safest-places-in-", zipcode, "/")
  cat("Processing URL:", url, "\n")
  main(url, zipcode)
  
  # Introduce a delay between requests to prevent rate limiting
  Sys.sleep(20) # Increase delay between requests
} 



