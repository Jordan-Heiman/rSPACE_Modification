### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-22

## Function purpose: Build a probability of use layer for simulated population

#################################### Intro #####################################

# Name: build_use_layer
# Description:  Using the use_surface function builds a raster layer that 
#               represents the probability of use of at least one individual in 
#               the simulated population 

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-22

################################# Arguments ####################################

# map:
#     Raster of probability of use for species of interest
# individuals:
#     List of vectors with a length equal to the number of types of individuals 
#     in the simulated population. Each vector contains the pixel numbers that 
#     represent the activity centers of one type of individual
# Parameters:
#     Parameters for simulation as entered through enter_parameters functions or
#     as manually created in a list 

################################# Output #######################################

# useLayer:
#     A vector with a length that is equal to the number of pixels in the
#     species probability of use raster that was provided, each number in this 
#     vector represents the probability of use of at least one individual in the 
#     simulated population

################################################################################
## Function

build_use_layer <- function(map, 
                            individuals,
                            Parameters, 
                            Example = FALSE){
  
  # Get the number of individual types from the parameters list
  nTypes <- length(Parameters$MFratio)
  
  # Use use_surface function to create a layer that represents the probability 
  # of not being used for each pixel for individual type 1
  NotUsed <- 1 - use_surface(Individuals = individuals[[1]],
                             howmuch = Parameters$moveDistQ[1],
                             howfar = Parameters$moveDist[1],
                             map = map,
                             trunk = Parameters$maxDistQ[1])
  
  # Creates probability of not used for each pixel for subsequent types of 
  # individuals and multiplies that by the previous total probability of not 
  # used for each pixel, if argument prevents trying to do this if only one 
  # individual is provided
  if (!Example & nTypes > 1) {
    for (ii in 2:nTypes) {
      NotUsed <- NotUsed * (1 - use_surface(Individuals = individuals[[ii]],
                                            howmuch = Parameters$moveDistQ[ii],
                                            howfar = Parameters$moveDist[ii],
                                            map = map,
                                            trunk = Parameters$maxDistQ[ii]))
    }
  }
  
  if (!is.null(Parameters$repeat.groups)) {
    if (Parameters$repeat.groups == T) {
      NotUsed <- NotUsed^2
    }
  }
  
  # Change the probability of no use back to the probability of use by at least 
  # one individual
  useLayer <- 1 - NotUsed
  return(useLayer)
}
