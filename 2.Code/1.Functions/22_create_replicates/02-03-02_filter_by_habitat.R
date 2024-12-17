### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Filter the grid cell based on habitat thresholds

#################################### Intro #####################################

# Name: filter_by_habitat
# Description:  Uses provided cutoffs and the probability of use raster to 
#               filter out grid cells that do not meet the minimum threshold for
#               sampling

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# gridLayer:
#     Vector that is the length of all the pixel which holds the grid cell 
#     number that the respective pixel is assigned to, with zeros representing 
#     pixels that should not be assigned to any grid cell
# map:
#     Probability of use raster for the species of interest
# sample.cutoff:
#     Minimum percentage of pixels within a cell that must be greater than or 
#     equal to the habitat cutoff for the cell to be considered for sampling, 
#     expressed as a decimal
# habitat.cutoff:
#     Minimum value for a pixel to still be considered viable for use by the 
#     species of interest

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

filter_by_habitat <- function(gridLayer,
                              map, 
                              sample.cutoff,
                              habitat.cutoff){
  
  # If no sample cutoff, then just return the full grid 
  if (sample.cutoff == 0) {
    return(gridLayer)
  }
  
  # Get a TRUE/FALSE value for each pixel of the map for whether or not the
  # pixel meets the habitat threshold value
  habitatOK <- terra::values(map) >= habitat.cutoff
  
  # If there is a grid cell numbered zero (this contains pixels that could not 
  # be evenly put into a grid cell and may contain pixels on all edges of the
  # raster), remove the first cell number so that the process starts with cell 
  # number one
  IDs <- sort(unique(gridLayer))
  if (IDs[1] == 0) {
    IDs <- IDs[-1]
  }
  
  # For each grid cell number, return how many pixels in that cell met the 
  # habitat threshold 
  V1 <- tabulate(gridLayer[habitatOK], 
                 nbins = max(IDs))[IDs]
  
  # For each grid cell number, return the total number of pixels in that cell 
  V2 <- tabulate(gridLayer, 
                 nbins = max(IDs))[IDs]
  
  # Return the grid cell numbers that contained a valid percentage of pixels 
  # above the habitat threshold based on the sample cutoff value. For example, 
  # if the sample cutoff is 0.5, at least 50% of the pixels in a grid cell must
  # be greater than or equal to the habitat cutoff
  keep <- IDs[V1 >= sample.cutoff * V2]
  
  # Remove the grid cell numbers from the grid that do not meet these 
  # requirements
  gridLayer[!(gridLayer %in% keep)] <- 0
  
  return(gridLayer)
  
}
