### Master's Thesis
### Jordan Heiman [others as needed]
## Date: 2023-05-31

## Function purpose: Load packages (check if installed and if not install them)

################################################################################
## Load required packages
# Function from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

## First specify the packages of interest
packages <- c("raster", 
              "tcltk2", 
              "ggplot2", 
              "RMark", 
              "magrittr",
              "terra",
              "progress",
              "dplyr",
              "purrr",
              "data.table",
              "plyr")

## Now check each package, if it needs to be installed, install it, then load it
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)

# Also need to install one package that is only on GitHub if downloading SWE data
# devtools::install_github('marinosr/SNODASR')
# library(SNODASR)
