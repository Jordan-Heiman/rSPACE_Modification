### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-28

## Function purpose: Error handling for run_analysis function

#################################### Intro #####################################

# Name: error_handling
# Description:  A set of functions for error handling within the run_analysis 
#               function

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-28

################################################################################
## Function

try_n <- function(expr){
  tryCatch(expr, 
           error = function(e){ 
    return(NULL)
  })
}

try_w <- function(expr){
  suppressWarnings(try_n(expr))
}

try_mark <- function(expr){
  suppressMessages(tryCatch(expr,  
                            error = function(e){
                              if(grepl("mark.exe", e$message)){ 
                                stop(e$message, call. = F)
                              } else {
                                return(NULL)
                              }}))
}
