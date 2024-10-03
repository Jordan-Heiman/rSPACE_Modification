### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-20

## Function purpose: Creates sampling grid based on provided raster

#################################### Intro #####################################

# Name: make_grid
# Description:  Determines the projection of the provided probability of use 
#               raster for the species of interest. If the raster uses latitude
#               and longitude, a sampling grid is created using the 
#               make_grid_cpp function. If the raster uses UTMs a sampling grid 
#               is created within this function

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# map:
#     Probability of use raster for the species of interest 
# gridsize:
#     Size of the grid cells in kilometers squared as provided through the 
#     enter_parameters function or manually entered

################################# Output #######################################

# grid:
#     Vector that is the length of all the pixel which holds the grid cell 
#     number that the respective pixel is assigned to, with zeros representing 
#     pixels that should not be assigned to any grid cell

################################################################################
## Function

make_grid <- function(map,
                      gridsize){
  
  # Using the provided raster, pull out the number of pixels in the raster (n), 
  # the x coordinates of every pixel (x), and the y coordinates of every pixel 
  # (y)
  n <- ifelse(class(map) == "RasterLayer", length(getValues(map)), global(map, "notNA")[1, 1])
  x <- xFromCell(map, 1:n)
  y <- yFromCell(map, 1:n)
  
  # Determine whether the raster is projected in latitude and longitude or UTMs
  isLongLat <- grepl('+proj=longlat', crs(map, proj = TRUE))
  isUTM <- grepl('+proj=utm', crs(map, proj = TRUE))
  
  # If the raster is in lat/long:
  if (isLongLat) {
    
    # Use the make_grid_cpp function to create a sampling grid that covers the
    # raster, this just creates all possible cells to sample, without any 
    # filtering for use or habitat yet
    grid <- .C("make_grid_c",
               x = as.double(x),                     
               y =  as.double(y),                    
               grid_size = as.double(gridsize),              
               pixels = as.integer(n),                    
               grid = as.integer(rep(0,n)))$grid

    # If the raster is in UTMs: 
  } else if (isUTM) {
    
    # Get the dimensions of the map in pixels, flips the order so that it is the 
    # number of columns, then the number of rows
    map_xy <- dim(map)[2:1] 
    
    # Calculate the dimensions of the map in grid cells by taking the length of 
    # one side of a grid cell (sqrt(gridssize)), converting that to meters 
    # (*1000) and dividing it by the resolution of the map on each axis
    nxy <- ceiling(sqrt(gridsize)*1000/res(map))
    
    # using modulo, determine how many pixels will not fit into grid cells in 
    # each dimension
    skip_xy <- map_xy %% nxy
    
    # Dimensions of one grid cell in pixels
    n_cells <- (map_xy - skip_xy) %/% nxy
    
    # This creates a list of all possible pixel locations and sets their value
    # to 0 to start with
    gridDF <- expand.grid(x = 1:map_xy[1], y = 1:map_xy[2], value = 0)
    
    # This keeps only the pixels that can evenly fit into the grid while 
    # centering that grid on the map
    keeppixels <- (gridDF$x > floor(skip_xy[1]/2)) &
      (gridDF$x <= map_xy[1] - ceiling(skip_xy[1]/2)) &
      (gridDF$y > floor(skip_xy[2]/2)) &
      (gridDF$y <= map_xy[2] - ceiling(skip_xy[2]/2))
    
    # Set the value of each pixel location as the grid cell number it belongs to
    gridDF$value[keeppixels] <- rep(rep(rep(1:n_cells[1], 
                                            each = nxy[1]), 
                                        nxy[2]), 
                                    n_cells[2]) +
      rep(seq(from = 0, 
              by = n_cells[1], 
              length.out = n_cells[2]), 
          each = (n_cells[1] * nxy[1] * nxy[2]))
    
    # Store the grid as just the grid cell numbers for each pixel
    grid <- gridDF$value
  } else stop('Map must be either longlat or utm')
  
  return(grid)
}
