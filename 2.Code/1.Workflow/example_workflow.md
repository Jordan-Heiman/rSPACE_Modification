
<h1 style="text-align: center;">
rSPACE Modification for Simulated Occupancy Sampling
</h1>
<h2 style="text-align: center;">
Example Workflow
</h2>

<div style="text-align: center;">

[rSPACE original creator: Dr. Martha
Ellis](https://github.com/mmellis/rSPACE)

</div>

<p style="text-align: center;">
Script modifications by: Jordan Heiman
</p>
<p style="text-align: center;">
Last updated: 2024-10-18
</p>

## Code purpose: Investigation of the possibility of detecting trends in montane

# red foxes using variable sampling techniques.

################################################################################ 

# Library / Functions / Data

# Library

# Load packages via sourcing a package function file. These are chapter specific

source(“./2.Code/2.Functions/20_package_loading.R”) library(readxl)

# Functions

# This sources all the function files for this chapter, ignoring files with key

# phrases in their file path. These phrases include ‘package’, and ‘Not_in_use’.

# Package files are only sourced above as they are chapter specific

list.files(“./2.Code/2.Functions”, pattern = “.R\$”, full.names = TRUE,
recursive = FALSE, include.dirs = FALSE) %\>%
grep(“package\|Not_in_use”, ., invert = TRUE, value = TRUE) %\>%
grep(“2\[\[:digit:\]\]\_.\*.R”, ., ignore.case = TRUE, value = TRUE)
%\>% lapply(., source) %\>% invisible()

dyn.load(“./2.Code/4.C_Scripts/SPACE.dll”)

# Data

# Establishes the location of the output folder for saving results to

output_folder \<- here::here(“3.Outputs”)

## Setup

Begin by installing and loading the packages that are needed for the
workflow.

``` r
source("../2.Functions/00_package_loading.R")
```