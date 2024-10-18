### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-27

## Function purpose: Adjust an encounter history to only represent a select 
#                    number of visits

#################################### Intro #####################################

# Name: drop_visits
# Description:  Adjust an encounter history to only represent a select 
#               number of visits

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-27

################################# Arguments ####################################

# ch:
#     A vector of encounter histories where each value in the vector is a 
#     character string of the encounter history for one grid cell 
# n_visits: 
#     The total number of visits that were used in the simulations (the maximum 
#     number of visits that can be analyzed)
# n_yrs: 
#     The number of years that were used in the simulations
# n_visit:
#     The number of visits that are being tested in the analysis and thus the 
#     number of visits the encounter history needs to be adjusted to represent

################################# Output #######################################

# ch:
#     An adjusted encounter history that is a vector of character strings that 
#     are single characters  

################################################################################
## Function

drop_visits <- function(ch,
                        n_visits,
                        n_yrs, 
                        n_visit){
  
  # Set up a vector of FALSE values the same length as the maximum number of 
  # visits represented in the encounter history
  tmp <- rep(F, n_visits)
  
  # Change the values to TRUE for the number of visits that the encounter 
  # histories are going to be adjusted to represent, for example if the original 
  # encounter history was for 6 visits and it needs to be adjusted to 2 visits 
  # the vector will be TRUE TRUE FALSE FALSE FALSE FALSE
  tmp[1:n_visit] <- T 
  
  # Now extend the vector to repeat for the number of years in the simulation
  tmp <- rep(tmp, n_yrs)
  
  # Split each encounter history into a vector of character strings that are a 
  # single value indicating a detection ("1") or a non-detection ("0")
  unlist(lapply(strsplit(ch, split = ""), 
                
                # Then based on if the corresponding value in `tmp` is TRUE, 
                # keep or drop the encounter history value
                function(x){ 
                  paste(x[tmp], collapse = "")
                }))
}     
