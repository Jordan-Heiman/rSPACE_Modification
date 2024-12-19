### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-23

## Function purpose: Determines individuals to drop from a simulated population

#################################### Intro #####################################

# Name: drop_n
# Description:  Given a number of individuals to drop and a starting simulated 
#               population, returns a list of individuals to drop from the 
#               population

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-23

################################# Arguments ####################################

# dN:
#     An absolute value integer of the number of individuals that must be 
#     dropped from a population in order to achieve a desired lambda 
# map:
#     Raster of probability of use for species of interest
# Parameters:
#     Parameters for simulation as entered through enter_parameters functions or
#     as manually created in a list 
# pop.df:
#     Data frame that has the used pixels and individual type in that pixel for a 
#     simulated population

################################# Output #######################################

# list(drop.rows, lost.ind):
#     A list containing the rows that will be dropped from the `pop.df` object 
#     and a list of the pixel number of each of those individual's activity 
#     centers organized by type 

################################################################################
## Function

drop_n <- function(dN, 
                   map, 
                   Parameters, 
                   pop.df){
  
  # Retrieve how many types of individuals there are from the parameter list
  nTypes <- length(Parameters$MFratio)
  
  # If the `wght` parameter is true, this represents that not all individuals 
  # are removed from a landscape evenly, therefore...
  if (Parameters$wghts == T) {
    
    # Select individuals to remove with the probability of removal for each 
    # individual equal to the habitat quality for the location of the individual 
    # based on the pixels value in the raster map 
    drop.rows <- sample(nrow(pop.df), 
                        dN, 
                        prob = terra::values(map)[pop.df$locID])
    
    # If the `wghts` parameter is set to FALSE, all individuals are equally 
    # likely to be lost from the landscape...
  } else {
    
    # Thus, randomly select individuals to remove
    drop.rows <- sample(nrow(pop.df), dN)
  }
  
  # Retrieve the pixel and type of each individual to be lost
  lost.ind <- pop.df[drop.rows, ]
  
  # Create a list for the pixels where individuals were lost organized by type
  lost.ind <- lapply(1:nTypes, function(x){
    lost.ind$locID[lost.ind$type == x]
  })
  
  return(list(drop.rows, lost.ind))
}
