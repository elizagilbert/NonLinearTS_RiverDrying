#Libraries ####
library(tidyverse)
library(infotheo)
library(lubridate)

#plan(multisession)

#data  ####
Isleta_Div <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d"), Year = year(dates)) %>% 
  filter(year(dates) >= 2010) %>%
  filter(between(month(dates), 4, 10)) %>%
  select(Extent, Discharge_cfs, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm, Year) 

SanAcacia_Div <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R2") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d"), Year = year(dates)) %>% 
  filter(year(dates) >= 2010) %>%
  filter(between(month(dates), 4, 10)) %>%
  select(Extent, Discharge_cfs, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm, Year) 

#average annual entropy ####
# Function to calculate entropy for each year
calculate_entropy_by_year <- function(data) {
  # Split the data by year
  data %>% 
    group_by(Year) %>% 
    summarise(across(everything(), 
                     ~ entropy(discretize(.x)), 
                     .names = "entropy_{.col}")) %>%
    ungroup()
}

# Apply the function
entropy_by_year <- calculate_entropy_by_year(SanAcacia_Div)

# Calculate average and standard deviation of entropy across years for each variable
entropy_summary <- entropy_by_year %>%
  summarise(across(starts_with("entropy_"), 
                   list(mean = mean, sd = sd), 
                   .names = "{.col}_{.fn}"))

#conditional entropy####
# Function to calculate conditional entropy and percent reduction for each pair
calculate_conditional_entropy <- function(data) {
  # List of variables (excluding Year)
  variables <- colnames(data)[-7]
  n <- length(variables)
  
  # Initialize result list for percent reduction
  percent_reduction_list <- list()
  
  # Iterate through each year
  for (year in unique(data$Year)) {
    year_data <- data %>% filter(Year == year) %>% select(-Year)
    year_disc <- discretize(year_data)  # Discretize the data
    
    percent_reduction_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(variables, variables))
    
    # Nested loops for each variable pair
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          # Entropy of variable X
          H_X <- entropy(year_disc[, i], method = "mm")
          
          # Check for zero entropy to prevent division errors
          if (H_X > 0) {
            # Conditional entropy of X given Y
            H_X_given_Y <- condentropy(year_disc[, i], year_disc[, j], method = "mm")
            # Percent reduction in uncertainty
            percent_reduction_matrix[i, j] <- (H_X - H_X_given_Y) / H_X * 100
          } else {
            percent_reduction_matrix[i, j] <- 0  # Set to 0 if H_X is zero
          }
        }
      }
    }
    # Save matrix for the current year
    percent_reduction_list[[as.character(year)]] <- percent_reduction_matrix
  }
  
  return(percent_reduction_list)
}

# Apply the function
percent_reduction_by_year <- calculate_conditional_entropy(SanAcacia_Div)

# Average the percent reduction matrices across all years
average_percent_reduction <- Reduce("+", percent_reduction_by_year) / length(percent_reduction_by_year)

# Round results
average_percent_reduction <- round(average_percent_reduction, 0)

#best lag ####
# Function to calculate optimal lag for highest reduction in conditional entropy and SE
calculate_optimal_lag_with_se <- function(data, max_lag = 5) {
  # List of variables (excluding Year)
  variables <- colnames(data)[-7]
  n <- length(variables)
  
  # Initialize list to store results for each year
  lag_results_list <- list()
  
  # Iterate through each year
  for (year in unique(data$Year)) {
    year_data <- data %>% filter(Year == year) %>% select(-Year)
    year_disc <- discretize(year_data)  # Discretize the data
    
    # Matrix to store the optimal lag for each pair
    optimal_lag_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(variables, variables))
    
    # Nested loops for each variable pair
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {
          H_X <- entropy(year_disc[, i], method = "mm")
          
          # Skip if H(X) is zero
          if (H_X > 0) {
            best_reduction <- -Inf  # Initialize best reduction as negative infinity
            best_lag <- NA  # Initialize best lag
            
            # Iterate over lags
            for (lag in -max_lag:max_lag) {
              shifted_Y <- dplyr::lag(year_disc[, j], n = abs(lag))
              
              # Handle negative or positive lags properly
              if (lag < 0) shifted_Y <- dplyr::lag(year_disc[, j], n = abs(lag))
              if (lag > 0) shifted_Y <- dplyr::lead(year_disc[, j], n = abs(lag))
              
              # Adjust for leading/trailing NA due to lagging
              valid_indices <- complete.cases(year_disc[, i], shifted_Y)
              
              if (sum(valid_indices) > 0) {
                # Conditional entropy with lagged Y
                H_X_given_Y <- condentropy(year_disc[valid_indices, i], shifted_Y[valid_indices], method = "mm")
                percent_reduction <- (H_X - H_X_given_Y) / H_X * 100
                
                # Update best lag if this lag gives a higher reduction
                if (percent_reduction > best_reduction) {
                  best_reduction <- percent_reduction
                  best_lag <- lag
                }
              }
            }
            # Store the best lag for this pair
            optimal_lag_matrix[i, j] <- best_lag
          } else {
            optimal_lag_matrix[i, j] <- 0  # Set to 0 if H(X) is zero
          }
        }
      }
    }
    # Store optimal lag matrix for this year
    lag_results_list[[as.character(year)]] <- optimal_lag_matrix
  }
  
  # Stack all matrices into an array for year-by-year analysis
  lag_array <- simplify2array(lag_results_list)
  
  # Calculate the average lag across years
  average_optimal_lag <- apply(lag_array, c(1, 2), mean, na.rm = TRUE)
  
  # Calculate the standard deviation of lags across years
  lag_sd <- apply(lag_array, c(1, 2), sd, na.rm = TRUE)
  
  # Number of years (for SE calculation)
  n_years <- length(lag_results_list)
  
  # Calculate the standard error
  lag_se <- lag_sd / sqrt(n_years)
  
  # Replace NaN values in results with 0
  average_optimal_lag[is.na(average_optimal_lag)] <- 0
  lag_se[is.na(lag_se)] <- 0
  
  # Round results
  average_optimal_lag <- round(average_optimal_lag, 2)
  lag_se <- round(lag_se, 2)
  
  return(list(average_lag = average_optimal_lag, standard_error = lag_se))
}

# Apply the function
lag_results <- calculate_optimal_lag_with_se(SanAcacia_Div, max_lag = 7)

# Extract results
average_optimal_lag <- lag_results$average_lag
standard_error <- lag_results$standard_error

# Display results
cat("Average Optimal Lag for Each Variable Pair (Averaged Across Years):\n")
print(average_optimal_lag)

cat("\nStandard Error of Lag for Each Variable Pair:\n")
print(standard_error)




