### Power
### Jordan Heiman
## Date: 2023-03-29

## Function purpose: Read in simulation data and prepare it for plotting

#################################### Intro #####################################

# Name: get_data
# Description:  Reads in simulation results file and parameters data and adds
#               a count column for plotting by parent function

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-29

################################# Arguments ####################################

# folder:
#       Folder path for folder that contains both the simulation results text 
#       file and the parameters data file, these files can be nested within 
#       other folders in the folder provided
# CI:
#       Default: 0.95; The desired confidence interval to use for the trend 
#       analysis

################################# Output #######################################

# dta:
#       Data frame of simulation results for plotting

################################################################################
## Function

get_data <- function(folder, 
                     sing_max = 5,
                     CI = 0.95){
  
  # If the `folder` argument ends with txt...
  if (grepl('txt$', folder)) {
    
    # Set `filename` as a character string with the file path provided in the 
    # `folder` argument
    filename <- folder
    
    # Pull out the folder name provided in the file path for the `folder` 
    # argument
    folder <- dirname(folder)
    
    # If the `folder` argument does not end with txt...
  } else {
    
    # Find the file path for the simulation results text file and store it as a
    # character string called `filename`
    filename <- dir(paste0(folder, "/output"), 
                    pattern = 'sim_results.*txt',
                    recursive = F,
                    full.names = T)
    
    # Remove any file names that have 'NOTUSE` in the file names
    filename <- filename[!grepl('NOTUSE', filename)]
  }
  
  # Find the file in the provided folder that contains the parameters that were 
  # used in the scenario
  key <- dir(paste0(folder, "/output"), 
             pattern = 'Parameters.Rdata',
             recursive = F, 
             full.names = T)
  
  # If the simulation results file or the parameter data was not found, error 
  # and let the user know
  if (length(filename) == 0) {
    stop("Can't find results file")
  }
  if (length(key) == 0) {
    stop("Can't find the parameters key")
  }
  
  # If more than one simulation results file or parameter data file was found, 
  # warn the user that only one file will be used 
  if (length(filename) > 1) {
    message('More than 1 file found.')
    filename <- select.list(choices = filename,
                            title = "Please choose which results to use",
                            graphics = TRUE)
  }
  if (length(key) > 1) {
    message("More than 1 parameter set found")
    key <- select.list(choices = key,
                       title = "Please choose which parameter set to use",
                       graphics = TRUE)
  }
  
  # Pull the true lambda parameter that was used to create the simulated
  # population from the parameters data file
  lmda <- local({
    load(key[1])
    get(ls())$lmda 
  })
  
  # Read in the simulation data from the simulation results file and remove runs
  # with a singularity value > 5
  dta <- read.table(filename[1], header = T) %>% 
    mutate(trend = ifelse(singular <= sing_max, 
                          trend, 
                          NA), 
           trendSE = ifelse(singular <= sing_max, 
                            trendSE, 
                            NA))
  
  # If a count column is provided in the simulation results data...
  if ("count" %in% names(dta)) {
    
    # Warn the user that the count column will be used for the confidence 
    # interval in place of the `CI` argument provided
    message("Using provided values for 'count'.  CI argument ignored")
    
    # Otherwise, if a trend and trend SE column are included in the simulation 
    # results data
  } else if (all(c("trend","trendSE") %in% names(dta))) {
    
    # Find the z-score for the confidence interval desired based on a standard 
    # normal distribution
    CI <- qnorm(CI) 
    
    # For each simulation that was analyzed, determine if the confidence 
    # interval for the trend that was estimated for the simulation includes the
    # true lambda value, this is done by first determining if the true lambda 
    # represents a growing or shrinking population by re-centering around zero 
    # and returning the sign associated with the value (- or + returned as -1 or 
    # 1, respectively), then using that to ensure that the trend estimate is a 
    # positive trend so that the upper confidence interval can be used, and 
    # evaluate whether the estimate falls within the confidence interval that is
    # set
    dta$count <- as.numeric(sign(lmda - 1) * dta$trend > CI * dta$trendSE)
    
    # If the simulation results data does not have a count column or both a 
    # trend and trend SE column, error and warn the user
  } else stop("Either 'count' or 'trend' + 'trendSE' must be provided") 
  
  return(dta)
}
