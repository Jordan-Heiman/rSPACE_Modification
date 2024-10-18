### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-29

## Function purpose: Summarize the percent of simulations that detected a known 
#                    trend

#################################### Intro #####################################

# Name: sum_data
# Description:  Summarize the results of the analysis for the various 
#               simulations and how many scenarios were able to detect a known 
#               trend in the simulated population

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-29

################################# Arguments ####################################

# dta:
#     A data frame based on the simulation results text file created by the 
#     test_replicates function and as formatted by the get_data function 

################################# Output #######################################

# dta:
#     A data frame summarizing the results of the simulations and how many were
#     able to detect a known trend in a simulated population

################################################################################
## Function

sum_data <- function(dta){
  
  # If there is no count column in the simulation results, error and warn user
  if (!('count' %in% names(dta))) {
    stop("No 'count' variable")
  }
  
  # Set up a vector of the variables that will get processed
  variables <- c("n_grid", "n_visits", "detP", "alt_model", "loc_per", 
                 "grid_min", "grid_max")
  
  # Set up a placeholder for count
  count <- NULL
  
  # Determine the number of simulation replicates were conducted based on the 
  # simulation results data
  n_runs <- length(unique(dta$rn))
  
  # Subset the simulation results just down to the columns that will get
  # processed for summary and the count column
  dta <- dta[, names(dta) %in% c(variables, 'count')]
  
  # Summarize the count of how many simulations were possible for each 
  # combination of total grid cells, visits, detection probability, and sampling
  # model and how many of those were able to detect the known trend in the 
  # simulated population
  dta <- ddply(dta, 
               names(dta)[!grepl("count", names(dta))], 
               summarise,
               total = sum(!is.na(count)),
               count = sum(count, na.rm = T))
  
  # Set up a column for the total number of simulations that were conducted
  dta$n_runs <- n_runs
  
  # Make sure that all the variables that were specified are in the summary data 
  # frame, and if they aren't fill the missing columns with zero
  variables <- variables[!(variables %in% names(dta))]
  if (length(variables) > 0) {
    eval(parse(text = paste0('dta$', variables, ' = 0')))
  }
  
  return(dta)
}
