### Power
### Jordan Heiman
## Date: 2023-03-28

## Function purpose: Calculates the standard error for an occupancy trend

#################################### Intro #####################################

# Name: fpc_trend_se
# Description:  Calculates the standard error for an occupancy trend as modeled 
#               by RMark and corrected for finite populations

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-28

################################# Arguments ####################################

# Random.effects.model
#       The estimated effects, standard errors and variance from a robust design
#       occupancy model created by RMark
# k: 
#       The number of years that model represents
# FPC:
#       The finite population correction factor as calculated by the 
#       finite_pop_corr function

################################# Output #######################################

# trendSE: 
#       The standard error for the occupancy trend

################################################################################
## Function

fpc_trend_se <- function(Random.effects.model, 
                         k, 
                         FPC){
  
  # Pull out the standard error for the beta estimate from the occupancy model
  trendSE <- Random.effects.model$beta[2, 2] 
  
  # Pull out the sigma squared value for the occupancy model
  process.variance <- Random.effects.model$sigma^2 
  
  # Calculate the sampling variance for the trend
  sampling.variance <- k * trendSE^2 - process.variance    
  
  # Calculated the standard error for the trend in occupancy
  trendSE <- sqrt((process.variance + FPC * sampling.variance)/k)       
  
  return(trendSE)    
} 
