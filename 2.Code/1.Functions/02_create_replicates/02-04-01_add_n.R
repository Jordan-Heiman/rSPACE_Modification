### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-21

## Function purpose: Add individuals to a simulated population

#################################### Intro #####################################

# Name: add_n
# Description:  Using a provided probability of use raster for the species of 
#               interest, add individuals to a simulated population

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-21

################################# Arguments ####################################

# dN:
#     Starting size of the simulated population
# map:
#     Raster of probability of use for species of interest
# Parameters:
#     Parameters for simulation as entered through enter_parameters functions or
#     as manually created in a list 
# pop.df:
#     A data frame with information about individuals currently in the simulated
#     population, including the types of individuals

################################# Output #######################################

# new.ind:
#     List of vectors with a length equal to the number of types of individuals 
#     in the simulated population. Each vector contains the pixel numbers that 
#     are occupied by an individual of one type  

################################################################################
## Function

add_n <- function(dN, 
                  map,
                  Parameters, 
                  pop.df = NULL){
  
  # Create a vector with a TRUE or FALSE value for each pixel in the provided 
  # probability of use raster indicating whether or not the pixel is suitable
  # based on the habitat cutoff provided in the parameters
  habitatOK <- terra::values(map) >= Parameters$habitat.cutoff
  
  # From the parameters provided, get the number of different individual types
  nTypes <- length(Parameters$MFratio)
  
  # If the `pop.df` argument is NULL, assume there are not already individuals 
  # in the simulated population...
  if (is.null(pop.df)) { 
    
    # Create a vector of all pixel numbers that are suitable for individuals
    useLocations <- which(habitatOK)
    
    # Using the place_individuals function, create a vector of pixels that will 
    # be the locations of the individuals of each type in the population 
    new.ind <- lapply(1:nTypes, function(x){
      newInd <- place_individuals(map = map,
                                  use = useLocations,
                                  N = dN * Parameters$MFratio[x],
                                  buffer = Parameters$buffer[x],
                                  wght = Parameters$wghts)
      return(useLocations[newInd])
    })
    
    # If the `pop.df` argument is not NULL, there are already individuals in the 
    # simulated population...
  } else {
    
    # For each type of individual...
    new.ind <- lapply(1:nTypes, function(x){
      
      # Get the current pixel numbers that are occupied by the same type of 
      # individual
      currentLocations <- pop.df[pop.df$type == x, ]$locID
      
      # And the coordinates of those pixels
      currentLocations <- xyFromCell(map, currentLocations) 
      
      # Calculate the distance to an occupied pixel from all pixels
      if (class(map) == "RasterLayer") {
        distances <- distanceFromPoints(map, currentLocations) 
      } else if (class(map) == "SpatRaster") {
        distances <- distance(map, vect(currentLocations, crs = crs(map)))
      }
      
      # Create a vector of TRUE/FALSE values for each pixel indicating whether 
      # or not it is far enough from the closest occupied pixel to be a usable
      # point, i.e. it is beyond the buffer distance
      territoryOK <- terra::values(distances)/1000 > Parameters$buffer[x] 
      
      # Now get the pixel numbers for all pixels that pass the habitat cutoff 
      # and are far enough away from other individuals
      useLocations <- which(habitatOK & territoryOK) 
      
      # As long as there is at least one pixel that is available for use based  
      # on these cutoffs, use the place_individuals function to determine the 
      # activity center of the new individuals
      if (length(useLocations) > 0) {
        newInd <- place_individuals(map = map,
                                    use = useLocations,
                                    N = round(dN * Parameters$MFratio[x]),
                                    buffer = Parameters$buffer[x],
                                    wght = Parameters$wghts)
        
        # This gets the actual pixel number that was selected as used
        return(useLocations[newInd])
        
        # If there are no pixels available, then return a NULL value
      } else return(NULL) })
  }
  return(new.ind) 
}
