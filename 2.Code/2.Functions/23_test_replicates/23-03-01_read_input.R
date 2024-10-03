### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-24

## Function purpose: Reads a simulation output file and returns a data frame

#################################### Intro #####################################

# Name: read_input
# Description:  Given a simulation output file of encounter histories, creates a
#               data frame containing the grid cell numbers and their respective 
#               encounter histories

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# filename:
#     An output file from the simulations that are being analyzed 

################################# Output #######################################

# DF:
#     A data frame of the grid cells and encounter histories for those cells 
#     from the sampling simulation that is being analyzed 

################################################################################
## Function

read_input <- function(filename){ 
  
  # Read in the output file from a simulation, keeping only the second and
  # fourth column, these contain the grid cell number and the encounter history 
  # for that cell
  DF <- read.delim(filename, header = F, sep = c(' ', '*'), 
                   colClasses = c("character"))[, c(2, 4)]
  
  # Assign the correct column names to those columns that were kept
  names(DF) <- c('GridID', 'ch') 
  
  # Set the `GridID` column to a numeric type
  DF$GridID <- as.numeric(DF$GridID)
  
  return(DF)  
}   