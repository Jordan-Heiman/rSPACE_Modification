### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-21

## Function purpose: Determine locations to place simulated individuals

#################################### Intro #####################################

# Name: place_individuals
# Description:  Determines the locations for new individuals in a simulated 
#               population based on limitations provided in the parameters list

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-21

################################# Arguments ####################################

# map:
#     Raster of probability of use for species of interest
# use:
#     Vector of pixel numbers that have a suitable probability of use based on 
#     habitat cutoff provided
# N:
#     Number of individuals of one type in a population that need to be placed
#     on the simulated landscape
# buffer:
#     The buffer value provided for one type of individual in the simulated 
#     population, this is provided in the parameters and indicates how close 
#     together individuals of that type can be on a landscape
# wght:
#     A TRUE/FALSE value indicating whether pixels should be weighted for 
#     selection based on their value in the probability of use layer

################################# Output #######################################

# USE:
#     Vector with a length equal to the number of usable pixels, vector 
#     consists of ones and zeros indicating whether or not each usable pixel is 
#     occupied by an individual 

################################################################################
## Function

place_individuals <- function(map, 
                              use,
                              N,
                              buffer, 
                              wght = F){
  
  # Calculated the number of 'usable' pixels based on the habitat cutoff
  n <- length(use)
  
  # If `wght` argument is set to TRUE, sample all the pixels based on their
  # probability of use in the provided raster, otherwise, sample the pixel 
  # evenly. This sampling creates a list of usable pixels in a random order (if
  # weighted it is less random). This is the order that the pixels will get used
  # by individuals
  if (wght == T) {
    smp <- sample(n, n, prob = terra::values(map)[use])
  } else {
    smp <- sample(n, n)
  }
  
  # Create vectors for all the x and y coordinates of the raster pixels that are 
  # useable
  x <- xFromCell(map, use)
  y <- yFromCell(map, use)
  
  # This returns a one if the map is in latitude and longitude
  isLongLat <- as.numeric(grepl('+proj=longlat', crs(map, proj = TRUE)))
  
  # Create a vector of integers that are all zero with the same length as the 
  # number of usable pixels then assign a 1 to the zero that corresponds to the 
  # first sampled pixel as was randomly determined by the sample function at 
  # lines 58-62. 
  use10 <- rep(0, n)
  use10[smp[1]] <- isLongLat
  
  # Use the sample_ind function to determine the used pixels in the simulation
  USE <- .C('sample_ind_c',
            as.double(x),               #Longitude
            as.double(y),               #Latitude
            as.integer(N),              #Desired number of wolverines to place
            as.double(buffer),          #Minimum distance between points
            use = as.integer(use10),    #Indicator of whether to include or not
            as.integer(n),              #Number of snow_points to check (maxid)
            as.integer(smp - 1))$use    #Random order to use (shift for C indexing)
  
  return((1:n)[USE == 1])
  
}