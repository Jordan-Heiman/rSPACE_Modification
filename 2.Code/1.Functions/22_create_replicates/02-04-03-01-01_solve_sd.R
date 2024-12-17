### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-22

## Function purpose: Calculate the variance of movement distribution

#################################### Intro #####################################

# Name: solve_sd
# Description:  Calculates the variance of a movement distribution given a 
#               movement radius and what proportion of movement is within that 
#               radius

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-22

################################# Arguments ####################################

# howmuch:
#     Proportion of movements in movement radius for one individual type
# howfar:
#     Movement radius in kilometers of one individual type 
# map:
#     Raster of probability of use for species of interest

################################# Output #######################################

# cbind(sd_x, sd_y):
#     A single row with two columns, one for the standard deviation of movement
#     in each axis direction for one individual type  

################################################################################
## Function

solve_sd <- function(howmuch,
                     howfar,
                     map){
  
  # Determine if the map is in latitude and longitude or UTMs
  isUTM <- grepl('+proj=utm', crs(map, proj = TRUE))
  isLongLat <- grepl('+proj=longlat', crs(map, proj = TRUE))
  
  # If the map is projected in UTMs...
  if (isUTM) {  
    
    # This is a calculation of how many kilometers are in one map unit for each 
    # axis (i.e. in lat long this is km per degree and in UTM this is just km
    # per meter)
    km_per_x <- 1/1000
    km_per_y <- 1/1000
    
    # If the map projection is in lat/long...
  } else if (isLongLat) {
   
    # Calculate how many kilometers are represented by each degree for each axis
    # First find the middle of each axis
    mid <- c(mean(c(slot(extent(ext(map)[1:4]), 'xmin'), 
                    slot(extent(ext(map)[1:4]), 'xmax'))),
             mean(c(slot(extent(ext(map)[1:4]), 'ymin'), 
                    slot(extent(ext(map)[1:4]), 'ymax'))))
    
    # Calculate the distance from the mid point to one degree in each direction,
    # this returns in meters and must be divided by 1000 to get kilometers
    km_per_x <- pointDistance(mid, mid + c(1, 0), lonlat = T) / 1000
    km_per_y <- pointDistance(mid, mid + c(0, 1), lonlat = T) / 1000
  }
  
  # Calculate the cutoff for a standard normal distribution using the proportion 
  # of movements that are inside the movement radius
  cutoff <- qnorm((1 + howmuch)/2)   
  
  # Convert the movement radius to map units for each axis
  howfar_x <- howfar / km_per_x      
  howfar_y <- howfar / km_per_y
  
  # Convert the distribution cutoff to map scale to get standard deviation in
  # map units
  sd_x <- howfar_x / cutoff          
  sd_y <- howfar_y / cutoff
  
  return(cbind(sd_x, sd_y))
}