library(tidyverse)
library(infotheo)

#data Isleta ###
Escondida <- read.csv("Data/Processed/USGS_discharge.csv") %>% 
  filter(site_name == "Escondida") %>% 
  mutate(dates = as.Date(dateTime, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Discharge_cfs)
SanAcacia <- read.csv("Data/Processed/DiversionSubreachData.csv") %>% 
  filter(Reach == "R2") %>% 
  mutate(dates = as.Date(Date, format = "%Y-%m-%d")) %>% 
  filter(year(dates) >= 2010) %>% 
  select(Extent, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm) 


mydat <- cbind(Escondida, SanAcacia) %>% 
  select(Extent, Discharge_cfs, Diversion_cfs, Returns_cfs, Temp_C, Precip_mm)
mydat_disc <- discretize(mydat)

#conditional entropy measures the remaining uncertainty in X after knowing Y
#mutual information is the amount of information between random variables
  #amount of information one variable provides about the other-quantifies
  #how much knowing the value of one variable reduces the uncertainty about the other
  #overall dependence between two variables
#conditional mutual information measures the amount of information one variable provides about the other
  #a measure of the reduction in uncertainty of one variable due to know the other
  #use CMI when confounding variables are present

#conditional entropy ####
  #Conditional entropy represents the average uncertainty or randomness remaining in X after knowing Y
  #lower values suggest knowing Y greatly reduces uncertainty in X
  #higher values indicate that even when knowing Y, there's still significant uncertainty in X
  #entropy is directional

#entropy
H_ex <- entropy(mydat_disc$Extent, method = "mm")
  #0.62 entropy in drying itself and is highest possible entropy is the entropy of this variable
H_variables <- sapply(mydat_disc, entropy, method = "mm")

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

#mutual information ###
I_AcrossVariables <- mutinformation(mydat_disc, method = "emp") #providing the matrix does a pairwise computation
  #value of 0.07 is a low level of shared information between two variables.
  #knowing the value of one provides only slight reduction in uncertainty about the other

  #mutual information is not directional
I_test_direction <- mutinformation(mydat_disc$ExtentDry, mydat_disc$Discharge_cfs, method = "emp")
I_test_otherdirection <- mutinformation(mydat_disc$Discharge_cfs, mydat_disc$ExtentDry, method = "emp")


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
