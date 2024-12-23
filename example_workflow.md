
# rSPACE Modification for Simulated Occupancy Sampling

# Example Workflow

[rSPACE original creator: Dr. Martha
Ellis](https://github.com/mmellis/rSPACE)

Script modifications by: Jordan Heiman

Last updated: 2024-12-20

*This workflow is still in development, however, functions within this
repo work as is and are well commented to aid in their use*

## 1. Introduction

These script files were originally designed to provide a framework to
analyze the statistical power of wildlife occupancy monitoring designs
for detecting trends in abundance of a population. These modified
scripts expand the capability of the framework to analyze a wider range
of sampling designs including those where sampling locations and effort
vary during every sampling cycle (i.e. season or year).

These scripts are not currently setup as a package format but this
repository can be cloned and setup as a local R project. This example
workflow follows the simulations done by Heiman et al. (2024).

## 2. Library / Functions / Data

### 2.1 Library

Begin by installing and loading the packages that are needed for the
workflow.

``` r
source("2.Code/1.Functions/00_package_loading.R")
```

### 2.2 Functions

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

### 2.3 Input Data

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

## 3. Create Replicates

To begin a simulated population must first be created using the create
replicates function. This function uses a series of parameters to create
a spatially explicit population and then simulate perfect sampling of
that population using an array of grid cells of a specified size. This
results in creating encounter histories for each replicate for a set
number of years with perfect detection probability.

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
  # [Optional] The population trend type, either "abundance-exponential" (Default) 
  # or "abundance-linear". Under abundance-exponential, the population 
  # experiences exponential growth with the same value of lambda every year. 
  # Under abundance-linear, the population experiences and overall growth rate 
  # of the above lambda value between year 1 and the last year of the simulation. 
  trendtype = "abundance-exponential",
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
  # Whether or not activity centers and  which individuals are removed should be
  # weighted based on the underlying raster values. Setting this to TRUE can 
  # take a lot of processing power depending on the size of the focal area. 
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
provided, the filter map must be a raster of 0s and 1s with 1 indicating
pixels to include in the simulation. A cut off value for the filter map
should be added to the Parameter list (named filter.cutoff). The filter
cutoff refers to the minimum proportion of the pixels within a grid cell
that equal 1 in order for it to be sampled. If no filter cutoff value is
provided a default of 0.95 will be used.

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
# for each simulated year in the Plots window as they are created and save them
# to the output folder. If creating more than one replicate it is recommended to 
# set this to FALSE as it drastically slows the processing time. In practice a 
# single replicate can be created with showSteps set to TRUE in order to check 
# the settings then more replicates can be created by reading in the save 
# parameters and using the same run.label with add = TRUE and overwrite = FALSE.
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

As the function runs it will print the replicate number that it is
working on, this is referred to as the run number. This will be followed
by a progress bar for the replicate itself and one for the overall
simulation of all replicates. Once complete, the location of the
outputted files will be printed and the names fo the simulation files.

## 4. Test Replicates

Once the replicates have been created the `test_replicates` function is
used to subset the encounter histories to simulate sampling the
population.

The first argument of this function identifies where the replicate
encounter histories and other associated files can be found. This should
be a folder within the `folder.dir`, named the same as the `run.label`.

``` r
folder <- paste0(folder.dir, "/", run.label)
```

Followed by a set of parameters in a similar format to those provided to
the `create_replicates` function. These first few are from the original
version of rSPACE.

``` r
# The number of visits to simulate to each grid cell each year. If simulating
# more than one number of visits, these should be in a vector of integers.
Parameters$n_visit_test <- c(3, 4, 5, 6)
# The detection probability to simulate for each visit to each grid cell, as a 
# decimal value. If simulating more than one detection probability, these should
# be in a vector of numeric values. 
Parameters$detP_test <- c(0.8, 0.9)
# The proportion of the total number of grid cells to simulate sampling. This 
# value will only be used for some sampling strategies but should always be
# provided. The sampling strategies that use these values are the "Annual",
# "Biannual", "Sample Matrix" and "Spatially Inconsistent". If simulating more
# than one proportion of the total cells, this should be a vector of numeric
# values. 
Parameters$grid_sample <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 
                            0.80, 0.90)
```

Next, one or more sampling frameworks must be chosen. There are 6 to
choose from in the updated version of rSPACE, with the first 3 being
original to the rSPACE package.

|           Name           | Number |                                                                                                                                                                   Description                                                                                                                                                                    |
|:------------------------:|:------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|
|          Annual          |   0    |                                                                                                                             Sampling occurs in the same cells every year with no changes in the amount of sampling.                                                                                                                              |
|         Biannual         |   1    |                                                                                                                          Sampling occurs in the same cells every other year with no changes in the amount of sampling.                                                                                                                           |
|      Sample Matrix       |   2    |                                                                                                                  Sampling occurs in each year according to a sampling matrix that is provided to the test_replicates function.                                                                                                                   |
|  Spatially Inconsistent  |   4    | A consistent amount of sampling occurs every year but different cells are randomly selected to be sampled each year. A proportion of sampled cells can be set to be spatially consistent through all years, i.e. a proportion of cells can be set to be sampled in all years, these cells will be randomly selected at the begining of sampling. |
| Fully Variable - Uniform |   5    |                                           A different proportion of the landscape is sampled every year with the amount of sampling selected from a uniform distribution described in other parameters. The cells sampled are randomly selected each year as with the spatially inconsistent sampling.                                           |
|  Fully Variable - Beta   |   6    |                                            A different proportion of the landscape is sampled every year with the amount of sampling selected from a beta distribution described in other parameters. The cells sampled are randomly selected each year as with the spatially inconsistent sampling.                                             |

To select a sampling framework, the number associated with it is
provided as a vector of integers in the list of `Parameters`. If any
sampling frameworks besides the fully variable options are selected,
more than one framework can be tested at once. However, it’s recommended
to run the fully variable sampling frameworks separate from other
sampling frameworks.

``` r
Parameters$alt_model <- c(0, 1, 2, 4)

# OR

# Parameters$alt_model <- c(5)

# OR

# Parameters$alt_model <- c(6)
```

### 4.1 Sampling Matrix Option

If the sampling matrix framework is chosen to be tested, a sampling
matrix needs to be provided to the `test_replicates` function. This is a
matrix where each row represents a grid cell and each column represents
a year of sampling, with a 1 or a 0 value indicating whether that
corresponding grid cell was sampled (1) or not sampled (0) during the
corresponding year. This is provided as a separate argument to the
function, outside of the `Parameters` list.

``` r
if (2 %in% Parameters$alt_model) {
  # The number of grid cells can be determined from the number of rows in one of 
  # the simulation output files
  grid_count <- local({
    sim_files <- list.files(path = folder, 
                            pattern = ".txt$", 
                            full.names = TRUE)
    
    read.csv(sim_files[[1]], 
             header = FALSE) %>% 
      nrow()
  })
  
  # Then a sampling matrix can be created, ideally this would represent how 
  # sampling is planned to be conducted rather then having the cells sampled 
  # being determined by a random draw from a binomial distribution.
  sampling_matrix <- matrix(data = rbinom(grid_count * Parameters$n_yrs, 
                                          1, 
                                          prob = 0.5), 
                            nrow = grid_count,
                            ncol = Parameters$n_yrs)
}
```

### 4.2 Spatially Inconsistent Option

If a spatially inconsistent sampling framework is chosen, the proportion
of the landscape to be held spatially consistent needs to be provided in
the `Parameters` list. This proportion represents an amount of cells
that will be randomly selected to be sampled throughout all years of the
simulation and is referred to as ‘spatial consistency’. As an example,
if `Parameters$grid_sample` = 0.55 and the spatial consistency is 0.10
in a simulations with 100 grid cells. Every year 50% (50 cells) will be
sampled, 90% of those cells (45 cells) will be randomly selected every
year of the simulation. The last 10% (5 cells), will be randomly chosen
in the first year fo the simulation but then will be those same cells
the rest of the years of the simulation. Under this definition, spatial
consistency of 0 means that all cells will be randomly chosen in all
years and spatial consistency of 1 means that the grid cells will always
be the same in every year (the same as the annual sampling framework).
Multiple values can be provided as a vector of values to test spatial
consistency.

``` r
if (4 %in% Parameters$alt_model) {
  Parameters$spatial_percents <- c(0.05, 0.30, 0.55, 0.80)
}
```

### 4.3 Fully Variable

If selecting a fully variable sampling framework, there are 2 versions
to choose from: one using a uniform distribution and one using a beta
distribution. Under both of these, the proportion of the grid cells to
be sampled each year of the simulation will be a random draw from the
corresponding distribution. Under this framework, the
`Parameters$grid_sample` values will be ignored because of this, however
it is still required to be provided. The details of the distribution
used does need to be provided in the `Parameters` list.

#### 4.3.1 Uniform Distribution

Under the fully variable - uniform sampling framework, a uniform
distribution is used to select the proportion of the landscape sampled
each year. For this distribution, a minimum and maximum proportion of
the landscape needs to be provided in the `Parameters` list.

``` r
if (5 %in% Parameters$alt_model) {
  Parameters$grid_min <- 0.01
  Parameters$grid_max <- 0.10
}
```

#### 4.3.1 Beta Distribution

Under the fully variable - beta sampling framework, a beta distribution
is used to select the proportion of the landscape sampled each year. For
this distribution, an [alpha and beta parameter for the
distribution](https://mathlets.org/mathlets/beta-distribution/) needs to
be provided in the `Parameters` list.

``` r
if (6 %in% Parameters$alt_model) {
  Parameters$alpha <- 1.5
  Parameters$beta <- 30
}
```

                    Parameters = params,
                    SubPop = NULL,
                    sample_matrix = NULL,
                    xxx = 1,
                    max_xxx = 1,
                    min_xxx = 1,
                    base.name = paste0(sim_name, "_"),
                    results.file = results_file,
                    n_runs = NULL,
                    FPCind = TRUE,
                    skipConfirm = T,
                    overwrite = overwrite,
                    add = !overwrite,
                    randomize = T

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

<div id="ref-heiman_2024" class="csl-entry">

Heiman, J. L., Jody M. Tucker, Sarah N. Sells, Joshua J. Millspaugh, and
Michael K. Schwartz. 2024. “Leveraging Local Wildlife Surveys for Robust
Occupancy Trend Estimation.” *Ecological Indicators* 169 (169).
<https://doi.org/10.1016/j.ecolind.2024.112863>.

</div>

</div>

# notes to self (mostly things I meant to fix a long time ago but do not affect functionality):

test reps: - alt model 3 - fix the awkward numbering - alt models 5 and
6 must be run separately- is this still true? - check that grid_sample
is ignored with alt models 5 and 6 (if so why is it still required?)

extra parameters - create replicates: - effective sampling area
(measured in sqkm)? (how to incorporate when selecting new cells every
year, currently would be the same effective area in each cell? Been
wanting to figure out how to make this work for a while.)

-repeat.groups? looks like for repeating an individual type, not sure
why you would want that? Get rid of it? Write up in instructions?
