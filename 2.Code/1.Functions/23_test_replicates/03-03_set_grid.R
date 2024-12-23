### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-24

## Function purpose: Determines the grid cell numbers in a simulation

#################################### Intro #####################################

# Name: set_grid
# Description:  Determines the grid cell numbers in a simulation based on a 
#               filtered sub-region raster or an output file from the simulation 
#               being analyzed

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# filetest:
#     An output file from the simulations that are being analyzed 
# SubPop:
#     A raster with the sampling grid layer filtered to a sub-region

################################# Output #######################################

# GRDuse:
#     A vector of grid cell numbers that were in the simulation that is being 
#     analyzed 

################################################################################
## Function

set_grid <- function(filetest, 
                     SubPop = NULL){   
  
  # If `SubPop` is not NULL, use it to filter the grid layer to only include a 
  # sub-region of the sampling area                                                
  if (!is.null(SubPop)) {              
    
    # Set up `SupPop` as a raster using the second layer from the file
    map <- raster(SubPop, band = 2)   
    
    # Get the grid cell numbers that are left in the grid after filtering
    GRDuse <- unique(getValues(map))  
    
    # Remove any NA or zero values from the list of cell numbers
    GRDuse <- GRDuse[which(GRDuse > 0)] 
    
    # If no `SubPop` filter was provided...
  } else {                    
    
    # Use the read_input function to create a data frame from the simulation
    # output file
    test <- read_input(filename = filetest) 
    
    # Pull out just the grid cell numbers to return to the parent function
    GRDuse <- test$GridID                                                       
  }          
  
  return(GRDuse)                                                               
}    