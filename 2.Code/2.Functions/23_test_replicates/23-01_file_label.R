### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-24

## Function purpose: Returns just the file name of a given ".txt" file

#################################### Intro #####################################

# Name: file_label
# Description:  Given a file path of a ".txt" file, returns the base name of the 
#               file with the ".txt" identification removed

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# filename:
#     Character string of the file path for a ".txt" file 

################################# Output #######################################

# filename:
#     Character string of the base name of the file with the ".txt" 
#     identification removed 

################################################################################
## Function

file_label <- function(filename){                                                
  filename <- basename(filename)                                                 
  filename <- gsub('.txt', '', filename)                                           
  return(filename)                                                             
}     