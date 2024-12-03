
# rSPACE Modification for Simulated Occupancy Sampling

## Example Workflow

[rSPACE original creator: Dr. Martha
Ellis](https://github.com/mmellis/rSPACE)

Script modifications by: Jordan Heiman

Last updated: 2024-10-18

*This workflow is still in development, however, functions within this
repo work as is and should be well commented*

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
map <- rast("1.Inputs/WolvHabitat_Bitterroot.tif")
output_folder <- paste0(getwd(), "/Outputs")
```

#### Create Replicates

To begin a simulated population must first be created using the create
replicates function. This function uses a series of parameters to create
a spatially explicit population and then simulate perfect sampling of
that population using an array of grid cells of a specified size.

The first argument is n_runs, which represents the number of population
replicates to create. for this brevity of this example we create 10
replicate but recommend using 100 or more for more precise results.

``` r
n_runs <- 10
```

The next argument is a raster representing the species distribution or
predicted habitat of the species of interest. In this case, our raster
of spring snow representing predicted wolverine habitat. This raster
must be read as a SpatRast class and was already read in above.

Next we need a list of parameters that describe the population. These
parameters were originally described by Ellis et al. (2015).

                      Parameters = params,
                      folder.dir = output_folder,
                      run.label = sim_name,
                      base.name = paste0(sim_name, "_"),
                      filter.map = NULL,
                      printN = 1,
                      saveParameters = 1,
                      saveGrid = 0,
                      skipConfirm = T, # Not the default selection
                      add = T,
                      overwrite = F, 
                      showSteps = ifelse(reps == 1, TRUE, FALSE)
                      
                      
                      

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-ellis_2015" class="csl-entry">

Ellis, Martha M., Jacob S. Ivan, Jody M. Tucker, and Michael K.
Schwartz. 2015. “<span class="nocase">rSPACE</span>: Spatially Based
Power Analysis for Conservation and Ecology.” Edited by Timothée Poisot.
*Methods in Ecology and Evolution* 6 (5): 621–25.
<https://doi.org/10.1111/2041-210X.12369>.

</div>

</div>
