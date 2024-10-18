### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-23

## Function purpose: Runs the calc_prob_cpp function

#################################### Intro #####################################

# Name: prob_pres
# Description:  Basically just a shell for the calc_prob_cpp function

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-23

################################# Arguments ####################################

# surface:
#     A vector with a length that is equal to the number of pixels in the
#     species probability of use raster that was provided, each number in this 
#     vector represents the probability of use of at least one individual in the 
#     simulated population, this is the result of the build_use_layer function
# gridvec:
#     A vector that is the length of all the pixels from the probability of use
#     raster for the species which holds the grid cell number that the
#     respective pixel is assigned to, with zeros representing pixels that 
#     should not be assigned to any grid cell
# detP:
#     The detection probability of the species

################################# Output #######################################

# USE:
#     A vector with a length equal to the number of grid cells, including grid 
#     cell zero, with each value representing the probability that an individual 
#     is present and detected in the grid cell 

################################################################################
## Function

prob_pres <- function(surface, 
                      gridvec,
                      detP = 1){
  
  # Using the calc_prob_cpp function, calculate the probability that each grid 
  # cell is occupied and the individual is detected within that cell
  USE <- .C("calc_prob_c", 
            as.double(surface), # Use  =  use surface with prob of at least one individual
            as.integer(gridvec), # Grid = vector of grid indices for each pixel
            use = as.integer(rep(0, max(gridvec) + 1)), #detection
            as.double(detP), #detection probability
            as.integer(length(surface)), #pixels
            as.integer(max(gridvec)), #max_grid
            occP = as.double(runif(n = (max(gridvec) + 1))) #test values, output detection probabilities
            )$occP[(unique(gridvec) + 1)[-1]]
  
  # Because a probability cannot be less than zero or greater than one, adjust
  # any values that somehow came out that way
  USE[USE < 0] <- 0
  USE[USE > 1] <- 1
  
  return(USE)
}
