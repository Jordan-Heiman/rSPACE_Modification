### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-27

## Function purpose: Creates a vector of lengths of time between visits 

#################################### Intro #####################################

# Name: time_int
# Description:  Creates a vector of lengths of time between visits in an 
#               encounter history, uses zero for time periods within the same 
#               season (secondary occasions) and one for time periods that are 
#               one year long (primary occasions)

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-27

################################# Arguments ####################################

# n_visit:
#     Number of visits to each site each season/year
# n_yrs: 
#     Number of seasons/years sampled

################################# Output #######################################

# tmp[-n_yrs * n_visit]:
#     A vector of lengths of time between visits in an encounter history

################################################################################
## Function

time_int <- function(n_visit, n_yrs){  
  
  # Create a vector of zeros as long as the number of visits
  tmp <- rep(0, n_visit) 
  
  # Set the last value in that vector to one
  tmp[n_visit] <- 1  
  
  # Extend this vector to repeat as many times are there are years in the 
  # simulation
  tmp <- rep(tmp, n_yrs) 
  
  # Return this vector with the last value taken off
  return(tmp[-n_yrs * n_visit])
} 
