### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-27

## Function purpose: Calculate finite population correction factor

#################################### Intro #####################################

# Name: finite_pop_corr
# Description:  Calculate the finite population correction factor based on the
#               number of cells sampled and the total available

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-27

################################# Arguments ####################################

# n:
#     Number of grid cells whose encounter histories are being analyzed
# N: 
#     Total number of grid cells available for sampling
# use: 
#     Default: TRUE; A TRUE/FALSE value indicating whether or not to use the 
#     finite population correction

################################# Output #######################################

# fpc:
#     The finite population correction factor to be used  

################################################################################
## Function

finite_pop_corr <- function(n, 
                            N, 
                            use = T) {
  
  # Calculate the finite population correction factor
  if(use == T){
    return((N - n)/N) 
  } else {
    return(1)
  }
}  
