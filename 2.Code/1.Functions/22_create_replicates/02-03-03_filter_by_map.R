### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Uses a provided map to filter the sampling grid

#################################### Intro #####################################

# Name: filter_by_map
# Description:  Based on a provided raster of ones and zeros, filter the created 
#               sampling grid to only contain cells that have meet the cutoff 
#               requirement of percentage of pixels that should be included

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# gridLayer:
#     Vector that is the length of all the pixel which holds the grid cell 
#     number that the respective pixel is assigned to, with zeros representing 
#     pixels that should not be assigned to any grid cell
# filter.map:
#     A raster of ones and zeros for each pixel indicating whether the pixel 
#     should be kept or not
# cutoff:
#     Minimum percentage of pixels within a cell that must marked as 1 in the 
#     filter.map for the cell to be considered for sampling, expressed as a
#     decimal 

################################# Output #######################################

# gridLayer:
#     An updated vector that is the length of all the pixel which holds the grid 
#     cell number that the respective pixel is assigned to, with zeros
#     representing pixels that should not be assigned to any grid cell. This 
#     updated vector dropped any grid cells that were not viable for sampling 
#     based on the threshold provided and replaces them with a zero for the grid
#     cell number of the respective pixels 

################################################################################
## Function

filter_by_map <- function(gridLayer, 
                          filter.map,
                          cutoff = 0.95){
  
  # If the cutoff is set to zero just return the full grid as all the cells will 
  # be kept
  if (cutoff == 0) { 
    return(gridLayer)
  }
  
  # Get a TRUE/FALSE value for each pixel of the map for whether or not the
  # filter.map has it marked to be kept
  mapOK <- terra::values(filter.map) == 1
  
  # If there is a grid cell numbered zero (this contains pixels that could not 
  # be evenly put into a grid cell and may contain pixels on all edges of the
  # raster), remove the first cell number so that the process starts with cell 
  # number one
  Cells <- sort(unique(gridLayer))
  if (Cells[1] == 0) {
    Cells <- Cells[-1]
  }
  
  # For each grid cell number, return how many pixels in that cell met the 
  # were marked to be kept
  V1 <- tabulate(gridLayer[mapOK],
                 nbins = max(Cells))[Cells]
  
  # For each grid cell number, return the total number of pixels in that cell 
  V2 <- tabulate(gridLayer, 
                 nbins = max(Cells))[Cells]
  
  # Return the grid cell numbers that contained a valid percentage of pixels 
  # above that were marked to be kept based on the cutoff value. For example, 
  # if the cutoff is 0.5, at least 50% of the pixels in a grid cell must be 
  # marked to be kept on the filter.map
  keep <- Cells[V1 >= cutoff * V2]
  
  # Remove the grid cell numbers from the grid that do not meet these 
  # requirements
  gridLayer[!(gridLayer %in% keep)] <- 0
  
  return(gridLayer)
}
