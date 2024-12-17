### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Check map for NAs, NaNs, etc.

#################################### Intro #####################################

# Name: check_map
# Description:  Checks map for correct projection types and units. Checks for 
#               NAs and NaNs in map and prompts user to replace those. Also 
#               checks the filter map provided for matching project, extent and 
#               resolution as well as for NAs and NaNs.

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# map:
#     Raster of probability of use for species of interest
# filter.map:
#     Optional; map that will be used to filter the raster

################################# Output #######################################

# map:
#     Returns the raster as it was provided  

################################################################################
## Function

check_map <- function(map, 
                      filter.map){
  
  # Process for type Raster
  if (class(map) == "RasterLayer") {
    # checks that map uses either UTMs for lat/long
    if (!grepl("proj=utm|proj=longlat", proj4string(map))) {
      stop("Projection needs to be in utm or longlat")
    }
  
    # if the map uses UTMs make sure the units are in meters, and if that isn't 
    # in the proj4 string, add it and warn user
    if (grepl("+proj=utm.*", proj4string(map))) {
      if (!grepl("+units=m", proj4string(map))) {
        message("Assuming UTM +units=m")
      }
    }
    # Check for NA values and NaN values
    if (any(is.na(getValues(map)))) {
      stop("NAs in habitat map. Replace with 0s")
    }
    
    if (any(is.nan(getValues(map)))) {
      stop("NaNs in habitat map")
    }
  
    # if a filter map is provided, check for the same projection, extent, 
    # resolution and that there are not any NA values
    if (!is.null(filter.map) & class(filter.map) == "RasterLayer") {
      if (proj4string(filter.map) != proj4string(map)) {
        stop("map and filter.map must have the same projection")}
      if (any(is.nan(getValues(filter.map)))) {
        stop("NaNs in filter.map. Replace with 0s")}
      if (any(is.na(getValues(map)))) {
        stop("NAs in filter.map. Replace with 0s")}
      if (extent(filter.map) != extent(map)) {
        stop("map and filter.map must have the same extent")}
      if (any(res(filter.map) != res(map))) {
        stop("map and filter.map must have the same resolution")}
    } else if (!is.null(filter.map) & class(filter.map) != "RasterLayer") {
      stop(paste0("Filter map class is different than habitat map, please ",
                  "provide both maps as the same class"))
    }
  
    # Process for SpatRaster
  } else if (class(map) == "SpatRaster") {
    
    # checks that map uses either UTMs for lat/long
    if (!grepl("proj=utm|proj=longlat", crs(map, proj = TRUE))) {
      stop("Projection needs to be in utm or longlat")
    }
    
    # if the map uses UTMs make sure the units are in meters, and if that isn't 
    # in the proj4 string, add it and warn user
    if (grepl("+proj=utm.*", crs(map, proj = TRUE))) {
      if (!grepl("+units=m", crs(map, proj = TRUE))) {
        message("Assuming UTM +units=m")
      }}
    
    # Check for NA values and NaN values
    if (global(map, "isNA") > 0) {
      stop("NAs in habitat map. Replace with 0s")
    }
    
    # if a filter map is provided, check for the same projection, extent, resolution 
    # and that there are not any NA values
    if (!is.null(filter.map) & class(filter.map) == "SpatRaster") {
      if (crs(filter.map) != crs(map)) {
        stop("map and filter.map must have the same projection")}
      if (global(filter.map, "isNA") > 0) {
        stop("NaNs in filter.map. Replace with 0s")}
      if (global(map, "isNA") > 0) {
        stop("NAs in filter.map. Replace with 0s")}
      if (ext(filter.map) != ext(map)) {
        stop("map and filter.map must have the same extent")}
      if (any(res(filter.map) != res(map))) {
        stop("map and filter.map must have the same resolution")}
    } else if (!is.null(filter.map) & class(filter.map) != "SpatRaster") {
      stop(paste0("Filter map class is different than habitat map, please ",
                  "provide both maps as the same class"))
    }
    
  } else{
    stop(paste0("Habitat map is not a raster::RasterLayer or terra::SpatRaster,",
                " please provide habitat map in one of these two formats"))
  }
  
  return(map)
}