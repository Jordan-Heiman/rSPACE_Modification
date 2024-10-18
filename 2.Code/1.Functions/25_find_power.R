### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-30

## Function purpose: 

#################################### Intro #####################################

# Name: 
# Description:  

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-30

################################# Arguments ####################################

# x:
#     [description of x] 

################################# Output #######################################

# y:
#     [description of y]  

################################################################################
## Function

# Uses loess smoother to interpolate number of cells required for power
find_power <- function(folder,
                       data,
                       CI = 0.95, 
                       pwr = .8, 
                       n_grid = NULL){
  
  # Set up a couple error handling functions
  tryWARN <- function(xx){
    suppressWarnings(
      tryCatch(
        withCallingHandlers(xx, 
                            warning = function(w){
                              message('Smoothed estimates may not be reliable.\nTry increasing number of data points or number of simulations\n')
                              }), 
                            error = function(x){
                              return(NA)
                            }))}
  tryNA <- function(xx){
    tryCatch(xx, 
             error = function(x){
               return(NA)
             })}
  
  # If both the `folder` and `data` arguments are not provided, error and warn
  # the user
  if(missing(folder) & missing(data)){
    stop("Must supply either a folder or data")
  }
  
  # If the `data` argument is not provided, use the get_data and sum_data 
  # functions to get the necessary data frames for determining power
  if(missing(data)){
    dta <- get_data(folder, CI)
    dtaS <- sum_data(dta)
    
    # If the `data` argument is provided, set the object `dtaS` as the data
    # frame that was provided
  } else {
    dtaS <- data
  }
  
  # Set a column in the summarized data frame that hold the percent of 
  # simulations in each scenario that were able to detect the known trend in the 
  # simulated population
  dtaS$percent <- dtaS$count/dtaS$total
  
  # Determine which columns in the summary data frame were altered in different 
  # scenarios and will need to be assessed for their effect on power
  extra.var <- which(!(names(dtaS) %in% c('n_grid', 'total', 'n_runs', 'count', 
                                          'percent')) &
                     unname(apply(dtaS, 2, function(x) length(unique(x)) > 1)))
  
  # If the argument `n_grid` is set to NULL...
  if(is.null(n_grid)){
    
    # Set up a function for finding power for the scenarios using the Loess 
    # smoother
    fit_loess_pwr <- function(DF, 
                              percent){
      
      # Wrapped in error handling, run the loess function for the data
      fit <- tryWARN(loess(n_grid ~ percent, data = DF))
      
      # Then use the predict function to get a power value for each scenario
      fit <- tryNA(as.numeric(predict(fit, data.frame(percent = percent))))
      
      return(data.frame(n_grid = fit))
    }
    
    # Run through the different variables that were adjusted to create a data 
    # frame of the power results
    fit <- ddply(dtaS, names(dtaS)[extra.var], fit_loess_pwr, percent = pwr)
  
    # If the argument `n_grid` is not set to NULL...
  } else {
    
    # Set up a function for finding the number of grid cells required by each 
    # scenario to reach desired power using the Loess smoother
    fit_loess_grid <- function(DF,
                               n_grid){
      
      # Wrapped in error handling, run the loess function for the data
      fit <- tryWARN(loess(percent ~ n_grid, data = DF))
      
      # Then use the predict function to get a power value for each scenario 
      # depending on the number of grid cells
      fit <- tryNA(as.numeric(predict(fit, data.frame(n_grid = n_grid))))
      
      return(data.frame(pwr = fit))
    }
    
    # Run through the different variables that were adjusted to create a data 
    # frame of the power results
    fit <- ddply(dtaS, names(dtaS)[extra.var], fit_loess_grid, n_grid = n_grid)    
  }
  
  # If fitting failed for all scenarios, error and warn the user
  if(all(is.na(fit[, ncol(fit)]))){
    stop('Smoothing function failed')
    
    # If fitting failed for any scenarios, warn the user
  } else if(any(is.na(fit[, ncol(fit)]))){
    message('Tested values out of range, NAs returned\n')
  }
  
  return(fit)
}
