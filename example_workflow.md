
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

All values within the raster must be $\ge$ 0 and not NA.

``` r
map <- rast("1.Inputs/WolvHabitat_Bitterroot.tif")
map[is.na(map)] <- 0
map[map < 0] <- 0
```

#### Create Replicates

To begin a simulated population must first be created using the create
replicates function. This function uses a series of parameters to create
a spatially explicit population and then simulate perfect sampling of
that population using an array of grid cells of a specified size.

The first argument is n_runs, which represents the number of population
replicates to create. for brevity of this example we create 10 replicate
but recommend using 100 or more for more precise results.

``` r
n_runs <- 10
```

The next argument is a raster representing the species distribution or
predicted habitat of the species of interest. In this case, our raster
of spring snow representing predicted wolverine habitat. This raster
must be read as a SpatRast class and was already read in above.

Next we need a list of parameters that describe the population. These
parameters were originally described by Ellis et al. (2015). Parameters
can be entered through the `enter_parameters` function or through a
custom list. If using `enter_parameters` a dialog box will provide a
mechanism for entering all the necessary parameters. Only a single
argument needs to be provided to `enter_parameters`, `raster_type`. This
is the class of the raster that was read in.

``` r
Parameters <- enter_parameters(raster_type = class(map))
```

The corresponding list must take the following format:

``` r
Parameters <- list(
  # Intial population size
  N = 50, 
  # Population growth rate
  lmda = 0.933, 
  # Number of years to simulate
  n_yrs = 10, 
  # Proportion of each type of individual (in this case 2 types, females [60% of
  # population] and males [40% of population])
  MFratio = c(0.6, 0.4), 
  # Buffer distance between activity centers for each type of individual in 
  # kilometers (in this case female activity centers are >= 16 km apart and 
  # males are >= 25 km apart)
  buffer = c(16, 25), 
  # Movement radius for each type of individual in kilometers (in this case 
  # female movement radius is 8.5 km and male is 12.5 km apart)
  moveDist = c(8.5, 12.5),
  # Proportion of movements for each individual type that is within their 
  # respective movement radii (in this case 90% of female movement is within the 
  # 8.5 km radius and 70% of male movement is within their 12.5 km radius)
  moveDistQ = c(0.9, 0.7),
  # Maximum proportion of movements to include for each individual type with 
  # 1 indicating to include all movements (in this case 95% of movements are
  # included for both females and males)
  maxDistQ = c(0.95, 0.95),
  # Grid cell size for sampling in square kilometers
  grid_size = 100,
  # The minimum raster value for activity centers when creating the population
  habitat.cutoff = 1,
  # Minimum proportion of a grid cell that must be >= the habitat cutoff for 
  # sampling to occur in the grid cell
  sample.cutoff = 0.5,
  # Maximum number of visits to a grid cell per year to simulated (number of 
  # visits can be subset to smaller values during analysis process)
  n_visits = 6,
  # Whether or not activity centers should be weighted based on the underlying 
  # raster values. Setting this to TRUE can take a lot of processing power 
  # depending on the size of the focal area. 
  wghts = FALSE, 
  # The class of raster that was read in above, this can be a spatRaster or 
  # Rast depening on the function/package that was used
  raster_type = class(map))
```

The next argument is the folder directory to save the output files to.
This function will write several files to this location including the
encounter history for each replicate, an RData file with the
`Parameters` list, and abundance values and other information for each
replicate.

``` r
folder.dir <- paste0(getwd(), "/Outputs")
```

Next is two labels for the simulation, these will be used to label
several of the files created and should be something to denote what the
simulation represents. These labels are also used to search for existing
simulation files in the `folder.dir`

``` r
run.label <- "Wolverine_Example_Sim"
base.name <- paste0(run.label, "_")
```

Next is an optional argument for a second raster that can be used to
filter the provided habitat/probability of use layer for the species of
interest. This will be set to NULL for this example. If a filter map is
provided, a cut.off value for the filter map should be added to the
Parameter list (named filter.cutoff). If no filter cutoff value is
provided a default of 0.95 will be used if all values in the filter map
are $\le$ 1.

``` r
filter.map <- NULL
```

The next few arguments handle some of the setting options for the
creating the simulations. For this example, all these will be set to
their default values.

``` r
# A value of 1 or 0 to indicate whether or not to save the map files that are 
# created of the simulation grid with 1 indicating to save them.
printN <- 1

# A value of 1 or 0 to indicate whether or not to save the simulation parameters
# to an RData file with 1 indicating to save them.
saveParameters <- 1

# A value of 1 or 0 to indicate whether or not to save a raster file of the
# simulation grid that is created during the simulation with 1 indicating to
# save it. In this raster, values will indicate which grid cell each pixel 
# belongs to. 
saveGrid <- 0

# A TRUE/FALSE operator to indicate whether or not to confirm that is it alright 
# for rSPACE to write text files before running simulations
skipConfirm <- FALSE

# A TRUE/FALSE operator to indicate whether or not to add to existing simulation 
# files
add <- FALSE

# A TRUE/FALSE operator to indicate whether or not to overwrite existing 
# simulation files
overwrite <- TRUE

# A TRUE/FALSE operator to indicate whether or not to display simulation maps 
# for each simulated year in the Plots window as they are created. If creating 
# more than one replicate it is recommended to set this to FALSE as it 
# drastically slows the processing time. In practice a single replicate can be 
# created with showSteps set to TRUE in order to check the settings then more 
# replicates can be created by reading in the save parameters and using the same 
# run.label with add = TRUE and overwrite = FALSE.
showSteps <- ifelse(n_runs == 1, TRUE, FALSE)
```

Now we can run the `create_replicates` function.

``` r
create_replicates(n_runs = n_runs,
                  map = map,
                  Parameters = Parameters,
                  folder.dir = folder.dir,
                  run.label = run.label,
                  base.name = base.name,
                  filter.map = filter.map,
                  printN = printN,
                  saveParameters = saveParameters,
                  saveGrid = saveGrid,
                  skipConfirm = skipConfirm, # Not the default selection
                  add = add,
                  overwrite = overwrite, 
                  showSteps = showSteps)
```

    ## 
    ##  rSPACE creates text files.
    ##         If you're ok with this, press ENTER to continue.
    ##         Typing anything else will exit.

    ## Restarting C:/Users/JordanHeiman/Box/Jordan_Workspace/Jordan_Masters/3.Writing_and_Presentations/3.Thesis/Power_Manuscript/rSPACE_Modification/Outputs/Wolverine_Example_Sim/output/N_final.txt

    ## Warning in file.remove(printN): cannot remove file
    ## 'C:/Users/JordanHeiman/Box/Jordan_Workspace/Jordan_Masters/3.Writing_and_Presentations/3.Thesis/Power_Manuscript/rSPACE_Modification/Outputs/Wolverine_Example_Sim/output/N_final.txt',
    ## reason 'No such file or directory'

    ## n 1

    ## Warning in write.table(PopulationTotals, file = printN, row.names = F,
    ## col.names = ifelse(rn == : appending column names to file

    ## n 2 
    ## n 3 
    ## n 4 
    ## n 5 
    ## n 6 
    ## n 7 
    ## n 8 
    ## n 9 
    ## n 10

    ## $DIR
    ## [1] "C:/Users/JordanHeiman/Box/Jordan_Workspace/Jordan_Masters/3.Writing_and_Presentations/3.Thesis/Power_Manuscript/rSPACE_Modification/Outputs/Wolverine_Example_Sim"
    ## 
    ## $filenames
    ##  [1] "Wolverine_Example_Sim_0001.txt" "Wolverine_Example_Sim_0002.txt"
    ##  [3] "Wolverine_Example_Sim_0003.txt" "Wolverine_Example_Sim_0004.txt"
    ##  [5] "Wolverine_Example_Sim_0005.txt" "Wolverine_Example_Sim_0006.txt"
    ##  [7] "Wolverine_Example_Sim_0007.txt" "Wolverine_Example_Sim_0008.txt"
    ##  [9] "Wolverine_Example_Sim_0009.txt" "Wolverine_Example_Sim_0010.txt"

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
