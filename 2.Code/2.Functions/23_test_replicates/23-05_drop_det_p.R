### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-27

## Function purpose: Adjusts an encounter history based on a provided detection 
#                    probability

#################################### Intro #####################################

# Name: drop_det_p
# Description:  Given an encounter history with a detection probability of one, 
#               will perform a Bernoulli trial for each value of one in the 
#               encounter history with a probability of success equal to the new 
#               detection probability provided in the arguments

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-27

################################# Arguments ####################################

# ch:
#     A vector of encounter histories where each value in the vector is a 
#     character string of the encounter history for one grid cell 
# detP:
#     The detection probability that the encounter histories need to be adjusted 
#     to represent

################################# Output #######################################

# ch:
#     An adjusted encounter history that is a vector of character strings that 
#     are single characters  

################################################################################
## Function

drop_det_p <- function(ch, 
                       detP) { 
  
  # Split each encounter history into a vector of character strings that are a 
  # single value indicating a detection ("1") or a non-detection ("0")
  unlist(lapply(strsplit(ch, split = ""), 
                
                # Use the switcheroo function to adjust the encouter histories 
                # to the new detection probability, then reformat the encouter
                # histories back to a vector of encouter histories where each 
                # value in the vector is a character string of the encounter 
                # history for one grid cell 
                function(x){
                  paste(switcheroo(x, detP), collapse = "")
                }))
}     
