### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Checks parameter list for missing values

#################################### Intro #####################################

# Name: check_parameters
# Description:  Checks the list of parameters provided for any missing values, 
#               fills in defaults if there are null values for required 
#               parameters that are missing from the enter_parameters interface. 
#               Also updates names of some parameters if old names are being used.

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# pList:
#     Parameters for simulation as entered through enter_parameters functions or
#     as manually created in a list 
# argList:
#     Additional arguments that are provided to upper functions, used to check 
#     for a filter map in this function

################################# Output #######################################

# pList:
#     Parameters list with all required parameters under correct argument names  

################################################################################
## Function

check_parameters <- function(pList, 
                             argList){
  
  # Add some default values for parameters that are not on the interface for 
  # enter_parameters function
  if (is.null(pList$trendtype)) {
    pList$trendtype <- "abundance-exponential"
  }
  if (!(pList$trendtype %in% c("abundance-exponential", "abundance-linear")) {
    stop("Trend type must be either 'abundance-exponential' or 'abundance-linear'.")
  }
  if (is.null(pList$maxDistQ)) {
    pList$maxDistQ <- rep(1, length(pList$MFratio))
  }
  if (is.null(pList$wghts)) {
    pList$wghts <- T
  }
  
  # If there is no filter cutoff provided and a filter map is provided...
  if (is.null(pList$filter.cutoff)) {
    if (!is.null(argList$filter.map)) {
      # Check if the entire filter map has values of one or less associated with 
      # it...
      if (all(getValues(argList$filter.map) <= 1)) {
        # In which case, set the cutoff value as 0.95
        pList$filter.cutoff <- 0.95
      }
    }
  }
  
  # This can probably get scrapped, cleans up old names that would have come 
  # from previous versions of rSPACE
  if ('trunk' %in% names(pList)) {
    stop("Truncation parameter has been reworked using probabilities instead of SDs.
      Use 'maxDistQ' instead of 'trunk'")
  }
  
  oldnames <- c('howfar', 'howmuch', 'trunk', 'HRcenter.cutoff')
  newnames <- c('moveDist', 'moveDistQ', 'maxDistQ', 'habitat.cutoff')
  
  if (any(oldnames %in% names(pList))) {
    old <- match(oldnames, names(pList))
    names(pList)[old] <- newnames[!is.na(old)]
    warning(paste('The following parameter names are deprecated:',
                  oldnames[!is.na(old)],
                  '\n  Replace with: ', newnames[!is.na(old)]))
  }
  
  return(pList)
}
