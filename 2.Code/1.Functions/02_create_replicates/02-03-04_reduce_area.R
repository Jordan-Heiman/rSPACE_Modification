### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Adjusts grid to account for effective sampling area

#################################### Intro #####################################

# Name: reduce_area
# Description:  Based on the effective sampling area provided, reduces the size 
#               of each cell and leaves and unsampled area of each cell that is 
#               outside the effective sampling area

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# gridLayer:
#     Vector that is the length of all the pixel which holds the grid cell 
#     number that the respective pixel is assigned to, with zeros representing 
#     pixels that should not be assigned to any grid cell
# gridsize:
#     Size of the grid cells in kilometers squared as provided through the 
#     enter_parameters function or manually entered
# sample.area:
#     The effective sampling area of the survey method being used as provided 
#     through the enter_parameters function or manually entered

################################# Output #######################################

# gridLayer:
#     An updated vector that is the length of all the pixel which holds the grid 
#     cell number that the respective pixel is assigned to, with zeros
#     representing pixels that should not be assigned to any grid cell. This 
#     updated vector will have replace the grid cell number for pixels outside 
#     the effective sampling area of each cell with a zero

################################################################################
## Function

reduce_area <- function(gridLayer, 
                        grid_size,
                        sample.area){
  
  # Checks that the effective sample area provided is smaller than the grid size 
  if (sample.area > grid_size) {
    stop('Effective sample area is larger than grid_size')
  }
  
  # As long as the sample area is smaller than the grid cell size...
  if (sample.area < grid_size) {
    
    # Cycle through all the grid cells that are viable after filtering by 
    # habitat and by map...
    for (x in unique(gridLayer[gridLayer > 0])) {
      
      # Get the pixel numbers that belong to the cell
      IDs <- which(gridLayer == x)
      
      # Get the pixel dimensions of the cell
      big.nr <- min(which(diff(IDs) > 1))
      big.nc <- round(length(IDs)/big.nr)
      
      # Create a matrix of the pixel number and where they belong in the cell, 
      # NOTE: This is making the cell turned 90 degrees counter-clockwise then 
      # flipped vertically, not sure that this matters in the end but it might 
      # at some point
      Cell <- matrix(IDs[1:c(big.nr * big.nc)], nrow = big.nr)
      
      # Calculate the portion of the dimensions of the full cell size  
      keep <- sqrt(sample.area/grid_size)
      
      # Determine the dimensions in pixels for the smaller cell size
      little.nr <- max(c(1, round(keep * big.nr)))
      little.nc <- max(c(1, round(keep * big.nc)))
       
      # Pick a random pixel to start at within the cell, accounting for the 
      # smaller area that will need to be covered, this is randomly deciding how 
      # many pixels will be between the edge of the effective area and the edge
      # of the full cell 
      Start <- sample((big.nr - little.nr) * (big.nc - little.nc), 1)
      start.nr <- Start %% (big.nr - little.nr)
      
      # If it was decided that the effective area will be up against the full 
      # cell edge then figure out the starting row number and column number
      start.nr <- ifelse(start.nr == 0, big.nr - little.nr, start.nr)
      start.nc <- (Start - 1) %/% (big.nr - little.nr) + 1
      
      # Get the pixel numbers within the larger cell that will be kept
      KeepID <- c(Cell[seq(start.nr, length.out = little.nr), 
                       seq(start.nc, length.out = little.nc)])
      
      # Get the pixel numbers that will be dropped from the cell
      DropIDs <- IDs[!(IDs %in% KeepID)]
      
      # Set the grid number for those pixels to zero
      gridLayer[DropIDs] <- 0
      
    }
  }
  
  return(gridLayer)
}
