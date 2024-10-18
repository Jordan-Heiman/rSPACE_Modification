### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-21

## Function purpose: Create a data frame with details of a simulated population

#################################### Intro #####################################

# Name: pop_dataframe
# Description:  Create a data frame with information about the number of types 
#               and individuals in the population as well as where those 
#               individuals are on the landscape 

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-21

################################# Arguments ####################################

# pop.list
#     List of vectors with a length equal to the number of types of individuals 
#     in the simulated population. Each vector contains the pixel numbers that 
#     are occupied by an individual of one type  
# map:
#     Raster of probability of use for species of interest

################################# Output #######################################

# pop.df:
#     Data frame that has the used pixels and individual type in that pixel for 
#     a simulated population. If a map was provided, the coordinates for each 
#     pixel will be included

################################################################################
## Function

pop_dataframe <- function(pop.list,
                          map = NULL){
  
  # Get the number of individual types within the population
  nTypes <- length(pop.list)
  
  # Get the number of individuals per type
  nPerType <- sapply(pop.list, length)
  
  # Create a data frame that has the used pixels and individual type in that pixel
  pop.df <- data.frame(type = rep(1:nTypes, nPerType),
                       locID = unlist(pop.list))
  if (!is.null(map)) {
    xy <- xyFromCell(map, pop.df$locID)
    pop.df$x <- xy[, 1]
    pop.df$y <- xy[, 2]
  }
  
  return(pop.df)
}