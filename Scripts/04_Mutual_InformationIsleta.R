library(tidyverse)
library(infotheo)

#data Isleta ####
Bosque_Farms <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "BosqueFarms") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Discharge_cfs)
Isleta_Div <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R1") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Extent, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm) 


mydat <- cbind(Bosque_Farms, Isleta_Div) %>% 
  select(Extent, Discharge_cfs, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm)
mydat_disc <- discretize(mydat)

#definitions####

#Conditional entropy represents the average uncertainty or randomness remaining in X after knowing Y
#lower values suggest knowing Y greatly reduces uncertainty in X
#higher values indicate that even when knowing Y, there's still significant uncertainty in X
#entropy is directional

#mutual information is the amount of information between random variables
  #amount of information one variable provides about the other-quantifies
  #how much knowing the value of one variable reduces the uncertainty about the other
  #overall dependence between two variables

#conditional mutual information measures the amount of information one variable provides about the other
  #a measure of the reduction in uncertainty of one variable due to know the other
  #use CMI when confounding variables are present



#entropy ####
H_ex <- entropy(mydat_disc$Extent, method = "mm")
  #0.62 entropy in drying itself and is highest possible entropy is the entropy of this variable
H_variables <- sapply(mydat_disc, entropy, method = "mm")

#conditional entropy
H_ex_dis <- condentropy(mydat_disc$Extent, mydat_disc$Discharge_cfs, method = "mm") 
H_ex_div <- condentropy(mydat_disc$Extent, mydat_disc$Diversion_cfs, method = "mm")
H_ex_ret <- condentropy(mydat_disc$Extent, mydat_disc$Returns_cfs, method = "mm")
H_ex_tem <- condentropy(mydat_disc$Extent, mydat_disc$Temp_C, method = "mm")
H_ex_pre <- condentropy(mydat_disc$Extent, mydat_disc$Precip_mm, method = "mm")

#bootstrap for CIs ####
data_frame <- mydat_disc

# Number of bootstrap samples
n_bootstrap <- 1000

# Get list of variables
variables <- names(data_frame)

# Initialize a list to store results
results <- list()
observed_entropies <- numeric(0)
pairs <- character(0)

# Loop through all combinations (both directions)
for (var1 in variables) {
  for (var2 in variables) {
    if (var1 != var2) {
      # Extract the variables
      X <- data_frame[[var1]]
      Y <- data_frame[[var2]]
      
      # Calculate the observed conditional entropy
      observed_entropy <- condentropy(X, Y)
      observed_entropies <- c(observed_entropies, observed_entropy)
      pairs <- c(pairs, paste(var1, "->", var2))
      
      # Perform bootstrap for conditional entropy
      bootstrap_entropies <- numeric(n_bootstrap)
      
      for (j in 1:n_bootstrap) {
        # Resample with replacement
        indices <- sample(1:length(X), replace = TRUE)
        X_boot <- X[indices]
        Y_boot <- Y[indices]
        
        # Calculate conditional entropy for the bootstrap sample
        bootstrap_entropies[j] <- condentropy(X_boot, Y_boot)
      }
      
      # Calculate confidence interval (95%)
      ci <- quantile(bootstrap_entropies, probs = c(0.025, 0.975))
      
      # Store the result
      results[[paste(var1, var2, sep = "_")]] <- list(
        "observed_entropy" = observed_entropy,
        "bootstrap_entropies" = bootstrap_entropies,
        "confidence_interval" = ci
      )
      
      # Print confidence interval and observed entropy for each pair
      cat("Confidence Interval for", var1, "->", var2, ": [", ci[1], ",", ci[2], "], Observed:", observed_entropy, "\n")
    }
  }
}

# Create a summary table to determine if observed entropy is within confidence intervals
summary_table <- data.frame(
  Pair = pairs,
  Observed_Entropy = observed_entropies,
  CI_Lower = sapply(results, function(x) x$confidence_interval[1]),
  CI_Upper = sapply(results, function(x) x$confidence_interval[2]),
  Is_Significant = sapply(1:length(pairs), function(i) {
    obs <- observed_entropies[i]
    ci <- results[[gsub(" -> ", "_", pairs[i])]]$confidence_interval
    # Check if observed value falls outside of the confidence interval (significant reduction in uncertainty)
    return(!(obs >= ci[1] && obs <= ci[2]))
  })
)

# Print the summary table
print(summary_table)


# reduction in uncertainty for all variables####
# Get the names of the columns (variables)
variable_names <- colnames(mydat_disc)

n <- length(variable_names)
percent_reduction_matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(variable_names, variable_names))

# Calculate entropy and conditional entropy for each pair of variables
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      # Calculate entropy of X (i.e., variable i)
      H_X <- entropy(mydat_disc[, i], method = "mm")
      
      # Calculate conditional entropy of X given Y (i.e., variable i given variable j)
      H_X_given_Y <- condentropy(mydat_disc[, i], mydat_disc[, j], method = "mm")
      
      # Calculate the percent reduction in uncertainty
      percent_reduction_matrix[i, j] <- (H_X - H_X_given_Y) / H_X * 100
    }
  }
}
precent_reduction_matrix <- round(percent_reduction_matrix, 2)

#mutual information ####
I_AcrossVariables <- mutinformation(mydat_disc, method = "emp") #providing the matrix does a pairwise computation
  #value of 0.07 is a low level of shared information between two variables.
  #knowing the value of one provides only slight reduction in uncertainty about the other

#conditional mutual information ####
I_dry_dis_c_div <- condinformation(mydat_disc$Extent, mydat_disc$Discharge_cfs, mydat_disc$Diversion_cfs, method = "emp") 
    #The CMI value of 0.50284 nats suggests a moderate amount of shared information 
    #between the two variables given the third. It isn't a particularly high value, 
    #indicating that while there is some dependency between the variables conditioned on the third, 
    #it is not exceptionally strong.

I_dry_ret_c_div <- condinformation(mydat_disc$Extent, mydat_disc$Returns_cfs, mydat_disc$Diversion_cfs, method = "emp")  
  #The CMI value of 0.257 nats (0.371 bits) is suggests there drying and returns share a modest amount of 
  #information when conditioned on diversion rates. Knowing returns given diversions, reduces the uncertainty in
  #drying by 0.257 nats. Indicates there is some shared information between drying and returns but it is not
  #particularly strong. Means the relationship between returns and drying when knowing diversions is noticeable
  #but not highly informative.

#interaction information ####
  #measure of the "higher-order" interaction among variables. Accounts for how the information shared between any
  #two of the variables is influenced by the presence of the third. A positive value suggest the variables together
  #share more information than what is suggested by pairwise interactions along. Might indicate redundancy in the 
  #information among the variables. A negative number suggests that knowing all three variables together provides
  #less information than the sum of pairwise interactions would suggest. This indicates synergistic interaction,
  #where the joint information of all variables is less than expected from the pairwise combinations alone. 
  #essentially, some of the information is "canceled" out when all variables are considered together. This
  #is important in complex systems where the whole is not just the sum of its parts. A 0 implies
  #the variables do not interact in a higher-order manner; their interactions are fully explained by pairwise relationships

ii <- interinformation(mydat_disc, method = "sg")
  #an interaction computation of -0.06 suggest a mild synergistic relationship among the variables,
  #meaning their combined information is slightly less than what would be expected if you only considered their pairwise
  #interactions.

#multiinformation ####
  #also known as total correlation is a measure that quantifies amount of shared information among all variables
  #generalizes concept of mutual information to more than two variables
  #positive values indicates redundancy among variables, meaning they share some information
  #the variables are not completely independent; knowing some reduces uncertainty about others
  #a 0, means variables completely independent and no shared information
  #large the value the mroe information is shared and the less independent they are
  #the maximum value would be the sum of individual entropies or if each were perfectly correlated then the number
  #of variables you have

my_dat_red <- mydat_disc %>% 
  select(!Extent)
M <- multiinformation(mydat_disc)

#lag mutual information ####
# Parameters
max_lag <- 214  # Define the maximum number of lags you want to test

# Initialize a vector to store mutual information for each lag
mutual_info <- numeric(max_lag + 1)

# Calculate mutual information for each lag
for (lag in 0:max_lag) {
  # Lag Discharge by 'lag' time steps
  lagged_discharge <- c(rep(NA, lag), mydat_disc$Discharge_cfs[1:(length(mydat_disc$Discharge_cfs) - lag)])
  
  # Calculate mutual information using non-lagged Extent and lagged Discharge
  # Omit NA values which occur due to lagging
  valid_indices <- which(!is.na(lagged_discharge))
  mutual_info[lag + 1] <- mutinformation(mydat_disc$Extent[valid_indices], lagged_discharge[valid_indices], method = "emp")
}

# Find the lag with the maximum mutual information
best_lag <- which.max(mutual_info) - 1  # Subtract 1 because R indexing starts at 1

# Output the best lag
cat("The lag with the highest mutual information is:", best_lag, "with an MI of", mutual_info[best_lag + 1], "\n")

# Plotting the mutual information against lags
plot(0:max_lag, mutual_info, type = 'b', xlab = "Lag (days)", ylab = "Cond Entrop",
     main = "Mutual Information vs. Lag", col = "blue", pch = 19)
abline(v = best_lag, col = "red", lwd = 2, lty = 2)  # Adds a line at the best lag

#lag conditional entropy ####
# Parameters
max_lag <- 214 # Define the maximum number of lags you want to test

# Initialize a vector to store mutual information for each lag
mutual_info <- numeric(max_lag + 1)

# Calculate mutual information for each lag
for (lag in 0:max_lag) {
  # Lag Discharge by 'lag' time steps
  lagged_discharge <- c(rep(NA, lag), mydat_disc$Discharge_cfs[1:(length(mydat_disc$Discharge_cfs) - lag)])
  
  # Calculate mutual information using non-lagged Extent and lagged Discharge
  # Omit NA values which occur due to lagging
  valid_indices <- which(!is.na(lagged_discharge))
  mutual_info[lag + 1] <- condentropy(mydat_disc$Extent[valid_indices], lagged_discharge[valid_indices], method = "emp")
}

# Find the lag with the maximum mutual information
best_lag <- which.max(mutual_info) - 1  # Subtract 1 because R indexing starts at 1

# Output the best lag
cat("The lag with the highest mutual information is:", best_lag, "with an MI of", mutual_info[best_lag + 1], "\n")

# Plotting the mutual information against lags
plot(0:max_lag, mutual_info, type = 'b', xlab = "Lag (days)", ylab = "Cond Entrop",
     main = "Cond Entrop vs. Lag", col = "blue", pch = 19)
abline(v = best_lag, col = "red", lwd = 2, lty = 2)  # Adds a line at the best lag
