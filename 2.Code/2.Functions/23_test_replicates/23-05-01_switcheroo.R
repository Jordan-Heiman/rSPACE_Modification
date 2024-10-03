### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-27

## Function purpose: Adjusts an encounter history based on a provided detection 
#                    probability

#################################### Intro #####################################

# Name: switcheroo
# Description:  Given an encounter history with a detection probability of one, 
#               will perform a Bernoulli trial for each value of one in the 
#               encounter history with a probability of success equal to the new 
#               detection probability provided in the arguments

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-27

################################# Arguments ####################################

# x:
#     An encounter history that is a vector of character strings that are single 
#     characters
# detP:
#     The detection probability that the encounter history needs to be adjusted 
#     to represent

################################# Output #######################################

# x:
#     An adjusted encounter history that is a vector of character strings that 
#     are single characters  

################################################################################
## Function

switcheroo <- function(x,
                       detP) {
  
  # For each value in the encounter history, 
  for(i in 1:length(x)){
    
    # If the value indicates a detection (x[i] == 1), 
    if(x[i] == "1"){
      
      # Perform a Bernoulli trial using the new detection probability, and if 
      # that trial results in a zero, 
      if(rbinom(1, 1, prob = detP) == 0){
        
        # Change the encounter history value to a zero
        x[i] = "0"
      }
    } 
  } 
  return(x) 
}  
