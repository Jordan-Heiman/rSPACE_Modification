### Power
### Martha Ellis, updated and additions by Jordan Heiman
## Date: 2023-03-20

## Function purpose: Create encounter histories for sampling replicates

#################################### Intro #####################################

# Name: create_replicates
# Description:  Creates and save encounter histories for all replicated sampling 
#               of simulated population and sampling grid

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-20

################################# Arguments ####################################

# n_runs:
#     total number of replicated simulations to run
# map:
#     Probability of use raster for the species of interest
# Parameters:
#     List of parameters as created by the enter_parameters function or as 
#     created manually and checked by the check_parameters function
# folder.dir:
#     Default: getwd(); Character string of the folder directory where files
#     should be saved
# run.label:
#     Default: 'rSPACE_X'; Character string indicating the name of the 
#     simulation to be used for folder and file naming
# base.name:
#     Default: 'rSPACEx'; Character string used to name simulation files and 
#     search for existing files
# filter.map:
#     Default: NULL; A second raster that can be used to filter the provided
#     habitat/probability of use layer for the species of interest
# printN:
#     Default: 1; A value of one or zero indicating whether or not to save the 
#     files that are created
# saveParameters:
#     Default: 1; A value of one or zero indicating whether or not to save the 
#     parameters that are provided as an .Rdata file
# saveGrid:
#     Default: 0; A value of one or zero indicating whether or not to save the 
#     grid that is created as a raster file
# skipConfirm:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to 
#     confirm that is it alright for rSPACE to write text files before running
#     simulations
# add:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to add to 
#     existing simulation files
# overwrite:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to  
#     overwrite existing simulation files
# showSteps:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to 
#     display simulation maps for each simulated year in the Plots window as 
#     they are created. If creating more than one replicate it is recommended to
#     set this to FALSE as it drastically slows the processing time. In practice
#     a single replicate can be created with showSteps set to TRUE in order to 
#     check the settings then more replicates can be created by reading in the 
#     save parameters and using the same run.label with add = TRUE and 
#     overwrite = FALSE.

################################# Output #######################################

# list(DIR = folder.dir, 
#      filenames = paste0(base.name, (1:n_runs) + rn.start, '.txt')):
#     Returns a list of the saved files for simulation replications as well as 
#     the population summaries

################################################################################
## Function

create_replicates <- function(n_runs, 
                              map,
                              Parameters,
                              folder.dir = getwd(),
                              run.label = 'rSPACE_X',
                              base.name = 'rSPACEx',
                              filter.map = NULL,
                              printN = 1,
                              saveParameters = 1,
                              saveGrid = 0,
                              skipConfirm = F,
                              add = F,
                              overwrite = T,
                              showSteps = F){
  
  list.files("2.Code/1.Functions/22_create_replicates",
             full.names = TRUE) %>% 
    lapply(., source) %>% 
    invisible()
  
  #   0. Match argument list and set up files                               #### 
  # Confirm it is alright to save/write files (depending on skipConfirm arg)
  if (!skipConfirm) {
    askConfirm <- ("" == readline(prompt = "\n rSPACE creates text files.
        If you're ok with this, press ENTER to continue.
        Typing anything else will exit.\n"))
    if (!askConfirm) {
      message('Exiting function')
      return(0)
    }
  }
  
  # Check for a folder with the run label, if there is not already one, then 
  # create one
  folder.dir <- paste(folder.dir, run.label, sep = '/')
  if (!file.exists(folder.dir)) {
    dir.create(folder.dir)
  }
  
  # Check for an output folder, if there is not one, create one
  output.dir <- paste(folder.dir, 'output', sep = '/')
  if (!file.exists(output.dir)) {
    dir.create(output.dir)
  }
  
  # If printing was selected, store the file name for writing to later
  if (printN == 1) {
    printN <- paste0(output.dir, '/N_final.txt')
    
    # If overwrite was selected, print a message to indicate that the file will
    # be overwritten, then remove the previous version of the file
    if (overwrite) {
      message(paste('Restarting', printN))
      file.remove(printN)
    }
  }
  
  # Store the file name for the parameters data file
  prevPList <- paste0(output.dir, '/Parameters.rdata')
  
  #   1. Enter parameters                                                   ####
  # If the parameters argument is NULL...
  if (missing(Parameters)) {
    
    # Check for an existing parameter data file in the output folder...
    if (file.exists(prevPList)) {
      
      # If there is one, print a message that an existing parameters data file 
      # will be used and load it to the environment
      message(paste0('Using existing parameters list for scenario from: ',
                     prevPList))
      load(prevPList)
      
      # If there is not an existing parameters data file...
    } else {
      
      # And `add` was set as TRUE (parameters were meant to be loaded from the
      # existing parameter data file)...
      if (add == T) {
        
        # Error out because there is not a parameter data file to load
        stop('No parameter list available')
        
        # If `add` was set to FALSE (and no parameters were provided), open the
        # dialog box for entering parameters 
      } else { 
        Parameters <- enter.parameters()}
    }
    
    # If parameters were provided...
  } else {
    
    # And `add` is set to TRUE, warn user that existing parameter data file will 
    # be used, then remove the parameters in the environment and load the 
    # parameters data file
    if (add == T) {
      if (file.exists(prevPList)) {
        warning(paste0('Using existing parameters list for scenario from: ', 
                       prevPList))
        rm('Parameters')
        load(prevPList)
      }}
  }
  
  # Now use the check_parameters function, to check for a few pieces that might
  # be missing and add defaults for them
  Parameters <- check_parameters(Parameters, 
                                 list(filter.map = filter.map))
  
  #   2. Set up map + grid layer                                            ####
  # Make sure that a map layer was provided in the arguments, otherwise error 
  if (missing(map)) {
    stop("Missing habitat layer")
  }
  
  # Use the check_map function ensure that there isn't anything missing from the 
  # map and that it is in a proper coordinate system (either latitude and 
  # longitude or UTMs). Also check that any filter map has matching attributes
  map <- check_map(map, filter.map)
  
  # Use the create_grid function  to create a sampling grid for the map, this 
  # includes filtering by habitat, or map as well as resizing for effective 
  # sampling area of each cell
  grid_layer <- create_grid(map = map, 
                            pList = Parameters, 
                            filter.map = filter.map,
                            reNumber = T) # Default selection
  
  # Pull out the grid cell numbers in the sampling grid
  gridIDs <- unique(grid_layer)[unique(grid_layer) > 0]
  
  #      2.5 Shift start for indices if needed                              ####
  # Get the number of existing files in the created folder directory
  n.prevFiles <- length(dir(folder.dir, pattern = base.name))
  
  # Calculate the starting run number, if `add` is set to TRUE, the first run 
  # number will be the number of current files (because the run numbers are 
  # indexed at zero), otherwise the first run is run zero 
  rn.start <- ifelse(add, n.prevFiles, 0)
  
  # When there are previous run files either `add` or `overwrite` needs to be 
  # set as FALSE, give a warning for the user if this is not the case and error
  if (add == F 
      & n.prevFiles > 0 
      & overwrite == F) {
    stop(paste(
      '\nExisting rSPACE runs found in',
      folder.dir,
      '\n Use "overwrite = T" to replace or "add = T" to add to existing folder'))
  }
  
  # Set up a progress bar
  pb <- progress_bar$new(format = "Creating replicates [:bar] :percent eta: :eta", 
                         total = max(n_runs), 
                         clear = FALSE)
  
  #   3. Simulate encounter histories loop                                  ####
  # For the number of runs that were provided in the arguments (starting at the 
  # next run if `add` is set to TRUE)...
  for (rn in (1:n_runs) + rn.start) {
    
    # First print the run number to the console
    # flush.console() is mostly for clearing the console in certain OS and 
    # versions of R 
    cat('\rn', rn, '\n')
    flush.console()

    # Use the encounter_history function to create an encounter history for the 
    # current run
    ch <- encounter_history(map = map,
                            Parameters = Parameters, 
                            grid_layer = grid_layer, 
                            n_cells = length(gridIDs), 
                            printN = printN,
                            rn = rn, 
                            filter.map = NULL, # Default selection
                            showSteps = showSteps, 
                            out_folder = output.dir) 
    
    #   3.5 Output encounter history                                        ####
    # Create a character string for the file path 
    output_file <- paste0(folder.dir, '/', 
                          base.name, 
                          formatC(rn, 
                                  digits = 3, 
                                  format = "d",
                                  flag = "0"), 
                          ".txt")
    
    # Save the encounter history to the output file
    cat(paste("/*", gridIDs, "*/", ch, "1;"), 
        sep = "\n", 
        file = output_file)
    
    pb$tick()
    
  # End the run number loop 
  } 
  
  #   4. Save data files                                                    ####
  # If the `saveParameters` argument is selected save the parameters to an Rdata 
  # file for use later
  if (saveParameters) {
    save(Parameters, file = paste0(output.dir, '/Parameters.Rdata'))
  }
  
  # # If the `saveGrid` argument is selected save the parameters to an Rdata file 
  # # for use later
  # if (saveGrid) {
  #   writeRaster(setValues(map, grid_layer), 
  #               filename = paste0(output.dir, '/Grid.tif'), overwrite = T)
  # }
  # To the parent function, return the folder directory and the file names
  return(list(DIR = folder.dir,
              filenames = paste0(base.name,
                                 formatC((1:n_runs) + rn.start, 
                                         digits = 3, 
                                         format = "d",
                                         flag = "0"), 
                                 '.txt')))
}