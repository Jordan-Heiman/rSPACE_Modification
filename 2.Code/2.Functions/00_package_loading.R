### rSPACE Modification                                                     ####
### Author: Jordan Heiman
## Date: 2023-05-31

## Source file purpose: Load packages (check if installed and if not install 
##                      them first)

#################################### Intro #####################################
# Name: package_loading
# Description:  This source file loads the packages needed for running the 
#               rSPACE modification scripts. 
# Associated publication:   Heiman, J.L., Tucker, J.M., Sells, S.N., Millspaugh, 
#                         J.J., Schwartz, M.K., [in review]

################################## Creation ####################################
# Author: Jordan Heiman
# Date Updated: 2024-10-17

################################# Arguments ####################################
# No arguments are provided to this function

################################# Output #######################################
# No outputs are given from this function

################################################################################
## Load required packages
# Script from:
# https://vbaliga.github.io/posts/2019-04-28-verify-that-r-packages-are-installed-and-loaded/index.html

## First specify the packages of interest
packages <- c("raster", "tcltk2", "ggplot2", "RMark", "magrittr", "terra",
              "progress", "plyr", "dplyr", "purrr", "data.table")

## Now check each package, if it needs to be installed, install it, then load it
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
    library(x, character.only = TRUE)
  }
)

