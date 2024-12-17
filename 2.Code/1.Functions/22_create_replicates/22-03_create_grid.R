### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Creates a sampling grid layer

#################################### Intro #####################################

# Name: create_grid
# Description:  Based on provided raster, parameters, and filter map creates a
#               grid cell layer to be used for sampling including filtering and
#               accounting for effective sampling area

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# map:
#     Probability of use raster for the species of interest
# pList:
#     List of parameters as created by the enter_parameters function or as
#     created manually and checked by the check_parameters function
# filter.map:
#     Map used to filter the raster provided
# reNumber:
#     Default: TRUE; A TRUE/FALSE logical operator indicating whether or not to
#     renumber the grid cells after they are created so that cells are numbered
#     sequentially, skipping any cells that are not considered as surveyed

################################# Output #######################################

# gridLayer:
#     A vector that is the length of all the pixels from the probability of use
#     raster for the species which holds the grid cell number that the
#     respective pixel is assigned to, with zeros representing pixels that
#     should not be assigned to any grid cell.

################################################################################
## Function

create_grid <- function(map,
                        pList,
                        filter.map = NULL,
                        reNumber = T) {
  # If there is no filter map provided, use make_grid function to make the grid
  # layer
  if (is.null(filter.map)) {
    gridLayer <- make_grid(map,
                           gridsize = pList$grid_size)

    # Then check that each grid cell is greater then the cutoff habitat value 
    # based on the values of all the pixels in the cell using the filter_by_habitat function
    gridLayer <- filter_by_habitat(gridLayer,
                                   map,
                                   sample.cutoff = pList$sample.cutoff,
                                   habitat.cutoff = pList$habitat.cutoff)

    # If a filter map is provided...
  } else {
    # double check there are no NaNs or NAs in the map
    if (global(filter.map, "isNA") > 0) {
      stop('NaNs in filter map')
    }
    
    # If all the values of the filter map are either 0 or 1...
    if (all(terra::values(filter.map) %in% c(0, 1))) {
      # Make the sampling grid just as above with the make_grid function
      gridLayer <- make_grid(map,
                             gridsize = pList$grid_size)

      # Use the filter_by_habitat function to first filter the sampling grid by
      # the habitat thresholds provided
      gridLayer <- filter_by_habitat(gridLayer,
                                     map,
                                     pList$sample.cutoff,
                                     pList$habitat.cutoff)

      # Then use the filter_by_map function to further filter the sampling grid
      gridLayer <- filter_by_map(gridLayer,
                                 filter.map,
                                 pList$filter.cutoff)

      # If all the values of the filter.map are not 0 or 1...
    } else {
      # Set the sampling grid as the values from pixels of the filter.map
      gridLayer <- terra::values(filter.map)
      reNumber <- F
    }
  }

  # Check that there is more than one grid cell created
  if (length(unique(gridLayer)) == 1) {
    stop("No grid cells in layer. Check filtering steps")
  }

  # If an effective sampling area is supplied use the reduce_area function to
  # adjust the grid to only cover an effective sampling area
  if (!is.null(pList$Effective.sample.area)) {
    gridLayer <- reduce_area(gridLayer,
                             pList$grid_size,
                             pList$Effective.sample.area)
  }

  # Renumber the grid cells so that they are sequential and start at 1 (keeping
  # grid cell number zero as pixels that don't belong to a grid cell)
  if (reNumber) {
    gridLayer <- (match(gridLayer,
                        unique(c(0, gridLayer))) - 1)
  }

  return(gridLayer)
}
