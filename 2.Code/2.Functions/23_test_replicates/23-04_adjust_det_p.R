### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-24

## Function purpose: Adjusts detection probabilities to be analyzed

#################################### Intro #####################################

# Name: adjust_det_p
# Description:  Adjusts detection probabilities to be analyzed

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# detP_test:
#     A vector of detection probabilities 
# detP1:
#     Default: 1; The maximum detection probability?, this should always be 1?

################################# Output #######################################

# detP_test:
#     A vector of adjusted detection probabilities 

################################################################################
## Function

adjust_det_p <- function(detP_test,
                         detP1 = 1){
  
  # Determine how many different detection probability values there are to test
  n <- length(detP_test)
  
  # Reorder the detection probabilities as highest to lowest in case they are 
  # not organized already
  detP_test <- sort(detP_test, decreasing = T)
  
  # Adds the `detP1` argument to the beginning of the list of detection 
  # probabilities and drops the lowest detection probability, then takes the 
  # full list of detection probabilities provided, divided by the shifted list 
  # of detection probabilities, so that each detection probability that is to be 
  # tested is divided by probability above it (with the highest being divided by 
  # `detP1`)
  detP_test <- detP_test/c(detP1, detP_test)[-(n + 1)] 
  
  return(detP_test) 
}   
