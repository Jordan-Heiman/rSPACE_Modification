
# rSPACE Modification for Simulated Occupancy Sampling

## Example Workflow

[rSPACE original creator: Dr. Martha
Ellis](https://github.com/mmellis/rSPACE)

Script modifications by: Jordan Heiman

Last updated: 2024-10-18

### Introduction

These script files were originally designed to provide a framework to
analyze the statistical power of wildlife occupancy monitoring designs
for detecting trends in abundance of a population. These modified
scripts expand the capability of the framework to analyze a wider range
of sampling designs including those where sampling locations and effort
vary during every sampling cycle (i.e. season or year).

These scripts are not currently setup as a package format but this
repository can be cloned and setup as a local R project. This example
workflow follows the simulations done by Heiman et al. (in review).

### Library / Functions / Data

#### Library

Begin by installing and loading the packages that are needed for the
workflow.

``` r
source("2.Code/1.Functions/00_package_loading.R")
```

#### Functions

Source all the functions that are needed for running the modified rSPACE
analysis. As well as sourcing one file with C++ functions that are used
for efficiency.

``` r
list.files("2.Code/1.Functions/",
           pattern = ".R", 
           full.names = TRUE, 
           recursive = FALSE, 
           include.dirs = FALSE) %>% 
  lapply(., source) %>% 
  # Avoid lengthy print out
  invisible()

dyn.load("2.Code/2.C_Scripts/SPACE.dll")
```

#### Input Data

Import example raster of species distribution. In this case a portion of
predicted wolverine (*Gulo gulo*) in the Bitterroot National Forest in
Montana, USA. This distribution is represented by persistent spring snow
and is based on habitat selection of wolverines.

This section also establishes an output folder for saving results and
simulation details.

``` r
wolv_rast <- rast("1.Inputs/WolvHabitat_Bitterroot.tif")
output_folder <- paste0(getwd(), "/Outputs")
```

#### 
