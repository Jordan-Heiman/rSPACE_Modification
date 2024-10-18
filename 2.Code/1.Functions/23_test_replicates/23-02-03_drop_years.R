### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-28

## Function purpose: Adjust an encounter history for not sampling every year

#################################### Intro #####################################

# Name: drop_years
# Description:  Given an encounter history, adjust it for simulating sampling 
#               that is not done every year

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-28

################################# Arguments ####################################

# ch:
#     A vector of encounter histories where each value in the vector is a 
#     character string of the encounter history for one grid cell 
# n_visit: 
#     The number of visits that are being used in the analysis of the simulation
#     (the maximum number of visits that can be analyzed)
# dropvec:
#     A vector with a length equal to the number of years in the simulation, 
#     with each value in the vector being TRUE or FALSE, indicating years that 
#     should be included in the analysis (FALSE) or years that should be dropped
#     (TRUE)
# samples:
#     Default: NULL; For use with irregular sampling, a matrix of 0s and 1s with 
#     each column representing a year of sampling and each row representing a 
#     single grid cell. Values indicate whether (1) or not (0) the respective
#     grid cell was sampled in the respective year 

################################# Output #######################################

# ch:
#     A vector of adjusted encounter histories where each value in the vector is
#     a character string of the encounter history for one grid cell with "."
#     representing years where the grid cell was not sampled

################################################################################
## Function

drop_years <- function(ch, 
                       n_visit, 
                       dropvec = rep(c(F, T),
                                     length.out = nchar(ch[1])/n_visit), 
                       samples = NULL,
                       vari_samp = FALSE){  
  
  # Set up an empty matrix with a row for each grid cell and a column for each 
  # year in the simulation and TRUE/FALSE values indicating which encounter 
  # histories to keep (FALSE) or drop (TRUE). This starts as a pattern of 
  # F T F T... for each cell for use with alternate year sampling
  dropvec <- matrix(dropvec, 
                    ncol = length(dropvec),
                    nrow = length(ch), 
                    byrow = T)
  
  # If a sample matrix is provided in the arguments (sampling is irregular)...
  if (!is.null(samples) & !vari_samp) {
    
    # Randomly select the correct number of grid cells from the sample matrix
    # for the analysis being conducted
    smple <- sample(nrow(samples), 
                    length(ch), 
                    replace = F)
    
    # Set up the vector of years to drop to match the years that the grid cells
    # selected were sampled
    dropvec <- matrix(as.logical(as.matrix(samples[smple, ])),
                      nrow = nrow(dropvec))
    
    # If variable sampling was selected
  } else if (!is.null(samples) & vari_samp) {
    
    dropvec <- matrix(!as.logical(samples),
           nrow = nrow(dropvec)) 
    
  }
  
  # Split each encounter history into a vector of character strings that are a 
  # single value indicating a detection ("1") or a non-detection ("0")
  ch_split <- strsplit(ch, split = '')   
  
  # Within each encounter history, use the `dropvec` matrix to determine which 
  # years need to be dropped (marked as TRUE) and replace the visits for those 
  # years with ".", then return the encounter histories to the format they were 
  # brought in as (a vector where each value is a character string of the 
  # encounter history for one grid cell)
  ch <- unlist(sapply(1:length(ch), function(x) {
    ch_split[[x]][rep(dropvec[x, ], each = n_visit)] <- "." 
    paste(ch_split[[x]], collapse = "")
  }))   
  
  # If a sample matrix was not provided (sampling is every other year), drop the 
  # last year of sampling as it will be a dropped year anyway
  if (is.null(samples)) {
    ch <- substr(ch, 1, nchar(ch) - n_visit)
  }   
  
  return(ch)
  }
    