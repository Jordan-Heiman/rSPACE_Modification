### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-22

## Function purpose: Build a layer that represents the probability of use

#################################### Intro #####################################

# Name: use_surface
# Description:  Uses the use_surface_cpp function to build a raster that 
#               represents the probability of use for individuals in a simulated
#               population

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-22

################################# Arguments ####################################

# Individuals:
#     Single vector containing the pixel numbers that are occupied by an 
#     individual of one type  
# howmuch:
#     Proportion of movements in movement radius for one individual type
# howfar:
#     Movement radius in kilometers of one individual type 
# map:
#     Raster of probability of use for species of interest
# trunk:
#     Maximum proportion of movements to allow for one individual type

################################# Output #######################################

# USE:
#     A vector with a length that is equal to the number of pixels in the
#     species probability of use raster that was provided, each number in this 
#     vector represents the probability of use of at least one individual in the 
#     simulated population 

################################################################################
## Function

use_surface <- function(Individuals, 
                        howmuch,
                        howfar, 
                        map,
                        trunk = trunk){
  
  # If there are not any used pixels then the probability of use is 0 everywhere
  # added the length requirement because errors were being thrown during 
  # removing individuals from population if one type had no individuals removed
  if (is.null(Individuals) | length(Individuals) == 0) {
    return(rep(0, 
               ifelse(class(map) == "RasterLayer",
                      length(getValues(map)),
                      global(map, "notNA")[1, 1])))
  }
  
  # Calculate the variance of movement distribution in map units (lat/long = 
  # degrees, UTM = meters) using the solve_sd function 
  sdXY <- solve_sd(howmuch, howfar, map)
  
  # Error if the `trunk` argument is greater than one or less than zero
  if (trunk > 1 | trunk < 0) {
    stop('Invalid value for trunction')
  }
  
  # If the `trunk` argument is equal to 1, change it to 0, because 1 indicates 
  # all movements are allowed for the individual type otherwise use a standard
  # normal distribution to calculate the cutoff value for the distribution 
  trunk <- ifelse(trunk == 1, 0, qnorm((1 + trunk) / 2))
  
  # Use the use_surface_c function to build the raster that represents the 
  # probability of use for individuals in a simulated population
  USE <- .C("use_surface_c", 
            x_ind = as.double(xFromCell(map, Individuals)),
            y_ind = as.double(yFromCell(map, Individuals)),
            N_ind = as.integer(length(Individuals)),
            x = as.double(xFromCell(map, 1:ncell(map))),
            y = as.double(yFromCell(map, 1:ncell(map))),
            use_spp = as.double(terra::values(map)),
            pixels = as.integer(ifelse(class(map) == "RasterLayer", 
                                       length(getValues(map)), 
                                       global(map, "notNA")[1, 1])),
            sd_x = as.double(c(sdXY[, 1])),
            sd_y = as.double(c(sdXY[, 2])),
            trunc_cutoff = as.double(c(trunk)))$use_spp
  
  return(USE)
}
