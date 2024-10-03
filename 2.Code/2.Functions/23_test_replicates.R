### Project
### Jordan Heiman [others as needed]
## Date: 2023-03-24

## Function purpose: 

#################################### Intro #####################################

# Name: 
# Description:  

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# folder:
#     The folder containing the simulated encounter histories that will be 
#     analyzed, this folder should also contain an outputs folder with other 
#     information about the simulations and this output folder is where the 
#     results of the further simulations will be saved 
# Parameters:
#     List of parameters as created by the enter_parameters function or as 
#     created manually and checked by the check_parameters function
# SubPop:
#     Default: NULL; A raster with the sampling grid layer filtered to a 
#     sub-region
# sample_martrix:
#     Default: NULL; Required to not be NULL if and irregular sampling model 
#     will be tested; A matrix where each row represents a grid cell and each 
#     column represents a year of sampling, each cell contains a 1 or a 0 value 
#     indicating whether that corresponding grid cell was sampled (1) or not 
#     sampled (0) during the corresponding year
# xxx:
# min_xxx:
# max_xxx:
# base.name:
#     Default: 'rSPACEx'; Character string used to name simulation files and 
#     search for existing files
# results.file:
#     Default: "sim_results.txt"; A character string containing the name for the
#     results file that will be created
# n_runs:
#     Default: NULL; The total number of replicated simulations to run, if not 
#     provided this value will be taken from the provided parameters
# FPCind:
#     Default: TRUE; A TRUE/FALSE value indicating whether or not a finite 
#     population correction value should be used  
# skipConfirm:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to 
#     confirm that is it alright for rSPACE to write text files before running
#     simulations
# overwrite:
#     Default: TRUE; A TRUE/FALSE operator to indicate whether or not to  
#     overwrite existing simulation files
# add:
#     Default: FALSE; A TRUE/FALSE operator to indicate whether or not to add to 
#     existing simulation files
# randomize:
#     Default: TRUE; A TRUE/FALSE operator to indicate whether or not to 
#     randomly choose the grid cells that will be sampled every year of 
#     sampling, cells are not randomized for each sampling year but are 
#     consistently use for all sampling years once chosen

################################# Output #######################################

# proc.time()[3] - time1:
#     This prints the number of seconds that it took for the analysis to run on 
#     all the encounter histories  
#
# In addition, this function will save a simulation results file with occupancy
# analysis results from all the encounter histories and simulation parameters 
# that were selected. These results will be used by further functions to create 
# power curves and estimations for the number of grid cells required to reach a 
# desired power

################################################################################
## Function

test_replicates <- function(folder, # folder to save to
                            Parameters, # normal parameters
                            SubPop = NULL, #SubPop,
                            sample_matrix = NULL, #sample_matrix,
                            xxx = 1, # not sure the point of this
                            max_xxx = 1,# not sure the point of this
                            min_xxx = 1,# not sure the point of this
                            base.name = "rSPACEx",
                            results.file = "sim_results.txt",
                            n_runs = NULL, #n_runs,
                            FPCind = TRUE, # finite population correction
                            skipConfirm = F,
                            overwrite = T,
                            add = F,
                            randomize = T){
  
  # Source the function scripts needed for this function
  list.files("./2.Code/2.Functions/23_test_replicates",
             full.names = TRUE) %>% 
    grep("CopyOf", ., value = TRUE, invert = TRUE) %>% 
    lapply(., source) %>% 
    invisible()
  
  #   0. Set up for the function                                            ####
  # Confirm it is alright to save/write files
  if (!skipConfirm) {
    askConfirm <- ("" == readline(prompt = "\n rSPACE creates text files.
        If you're ok with this, press ENTER to continue.
        Typing anything else will exit.\n"))
    if (!askConfirm) { 
      message('Exiting function')
      return(0)
    }
  }
  
  # Print a status message to the console
  cat('\nSetting up...\n')
  
  # Start a stopwatch to keep track of how long this process takes
  time1 <- proc.time()[3]
  
  # Pull some parameter values from the parameters of the simulation
  n_visits <- Parameters$n_visits
  n_yrs <- Parameters$n_yrs
  
  # Create a list of the existing simulation files that will be analyzed
  output_files <- dir(folder,
                      full.names = T,
                      pattern = paste0('^', paste(base.name, collapse = '|')))
  
  # Set up output folder/file for results
  folder <- paste0(folder, '/output')
  if (!file.exists(folder)) {
    dir.create(folder)
  }
  
  # If variable sampling is being done, create an upper folder for the saved 
  # sampling matrices
  if ((3 %in% Parameters$alt_model) | 
      (4 %in% Parameters$alt_model) | 
      (5 %in% Parameters$alt_model) | 
      (6 %in% Parameters$alt_model)) {
    samp_mat_folder <- paste0(folder, "/", 
                              gsub("sim_results.txt", "", results.file),
                              "variable_sampling_matrices")
    
    if (!dir.exists(samp_mat_folder)) {
      dir.create(samp_mat_folder)
      
    } else if (dir.exists(samp_mat_folder)) {
      askConfirm <- ("" == readline(prompt = "\n Sampling matrices folder already exists, 
  rerunning the analysis will rewrite these matrices
  If you're ok with this, press ENTER to continue.
  Typing anything else will exit.\n"))
      if (!askConfirm) { 
        message('Exiting function')
        return(0)
      }
    }
  }
  
  # Create a character string to hold the name of the results file
  results_file <- paste(folder, results.file, sep = "/")
  
  # If the results file already exists and `overwrite` and `add` were both set 
  # to FALSE, error out and let the user know that there the results file 
  # already exists
  if (file.exists(results_file) & !overwrite & !add) {
    stop(paste0("'", 
                results.file,
                "' already exists; use overwrite = TRUE or add = TRUE or specify a new filename using results.file"))
  }
  
  # If the results file does not already exist, set `add` to FALSE as there is 
  # nothing to add to yet
  if (!file.exists(results_file)) {
    add <- FALSE
  }
  
  # If `add` is set to TRUE...
  if (add) {
    
    # Print a message to the console warning the user that this may be a bad 
    # idea if the parameters of the simulations are different
    message("Appending to previous results. If you've changed simulation parameters, this is a BAD IDEA!")
    
    # Read in the existing simulation results file
    sim_results <- read.table(results_file, header = T)
    
    # Create a data frame that contains the number of replicates from the 
    # simulation and if there is only 1, warn the user then change `add` to 
    # FALSE
    DF <- ddply(sim_results, .(rn), nrow)
    if (nrow(DF) == 1) {
      message('Only one previous scenario (unknown completion); restarting results file')
      add <- FALSE
    }
    
    # Calculate the number of rows for each replicate/run
    tab <- unique(tabulate(DF$V1))
    tab <- tab[tab != 0]
    
    # Set up an empty vector to populate
    drop.files <- c()
    
    # Not sure yet here
    if (length(DF) == 1) { # There weren't any results in the results file yet
      drop.files <- unique(paste(DF$rn))
    }
    if (length(tab) > 2) { # The number of results for each run was different
      stop('Inconsistent number of subsets in results; specify files to analyze using base.name argument')
    }
    if (length(tab) == 2 | length(tab) == 1) { # At least one run did not fully complete testing Or testing was halted between runs
      drop.files <- unique(paste(DF$rn[DF$V1 == max(DF$V1)]))
      
      if (length(tab) == 2) {
        # Drop the results for the run(s) that did not complete
        sim_results <- sim_results[sim_results$rn != paste(DF$rn[DF$V1 == min(DF$V1)]), ]
      }
    }
    
    output_files <- output_files[!(file_label(output_files) %in% drop.files)]
    rm(tab, drop.files)
  }
  
  # If `add` was set to FALSE...
  if (!add) {
    
    # Use the run_analysis function to set up a matrix for holding the analysis 
    # results later
    sim_results <- run_analysis(n_yrs)
    
    # In the results text file, add the first line of text as the column headers
    cat(c(names(sim_results), 
          "n_grid", "n_visits", "detP", "alt_model", "loc_per", 
          "grid_min", "grid_max", "rn", "\n"),
        file = results_file)
  }
  
  # If a `sample_matrix` was provided, check that the matrix has as many columns 
  # as there are years in the simulation. If these values do not match, warn the 
  # user and randomly sample the correct number of columns from the 
  # `sample_matrix`
  if (!is.null(sample_matrix)) {
    if (ncol(sample_matrix) != Parameters$n_yrs) {
      message('Number of years in sample_matrix does not match n_yrs.  Resampling from sample_matrix')
      sample_matrix <- sample_matrix[, sample(ncol(sample_matrix), 
                                              Parameters$n_yrs,
                                              replace = T)]
    }}
  
  # If testing parameters are NULL, set these to default values
  if (is.null(Parameters$n_visit_test)) {
    # Range of the number of visits to each grid cell to test
    Parameters$n_visit_test <- 2:Parameters$n_visits
  }
  if (is.null(Parameters$detP_test)) {
    # Range of detection probability to test
    Parameters$detP_test <- c(1, 0.8, 0.2)
  }
  if (is.null(Parameters$grid_sample)) {
    # Range of percentage of grid cells to sample
    Parameters$grid_sample <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.75, 0.95)
  }
  if (is.null(Parameters$alt_model)) {
    # Alternative years models to test, 0 is every year, 1 every other year, 2 
    # is 'skip years'?
    Parameters$alt_model <- c(0, 1)
  }
  
  # Based on the first simulation encounter history output, or a sub-region grid 
  # layer, determine the grid cell numbers to use in the analysis
  GRDuse <- set_grid(filetest = output_files[1], 
                     SubPop)
  
  # Calculate the total number of grid cells
  gridTotal <- length(GRDuse)
  
  # Calculate the number of grid cells to analyze based on the different 
  # percentages of grid cells that were selected to be sampled
  n_grid_sample <- round(Parameters$grid_sample * gridTotal)
  
  # Adjust the detection probabilities because the encounter histories are 
  # adjusted to these test detection probabilities in a compiling loop where the
  # detection probability has to be adjusted from the previous value to the next 
  # test value. For example, if the test detection probabilities are 1, 0.8, 
  # and 0.2, the original encounter history with a detection probability of 1
  # will first be analyzed as is, then the encounter history will get adjusted
  # to represent a detection probability of 0.8 and then that encounter history 
  # with a detection probability of 0.8 will get adjusted to a detection 
  # probability of 0.2, this function calculates the values that will account 
  # for this and will be used as the detection probabilities for re-sampling 
  # encounter histories
  detP_test <- adjust_det_p(detP_test = Parameters$detP_test)
  
  #   1. Main Loop                                                          ####
  # If the number of simulations was not provided in the function arguments 
  # (`n_runs`), calculate the number based on the number of output files that 
  # were in the output folder
  if (is.null(n_runs)) {
    n_runs <- length(output_files)
  }
  
  # Set up an index that can be used to limit the runs that are analyzed
  index <- rep(min_xxx:max_xxx, length.out = n_runs)
  
  # If other models besides 4 were included, make sure that there is a spatial
  # percent of 1 included
  if (sum(grep(4, Parameters$alt_model, invert = TRUE)) > 0) {
    if (max(Parameters$spatial_percents) < 1) {
      Parameters$spatial_percents <- c(Parameters$spatial_percents, 1)
    }
    if (min(Parameters$spatial_percents) > 0) {
      Parameters$spatial_percents <- c(0, Parameters$spatial_percents)
    }
  }
  
  # Set up a progress bar
  pb_run <- progress_bar$new(format = "Testing replicates [:bar] :percent eta: :eta", 
                             total = max(n_runs), 
                             clear = FALSE)
  
  # For every simulation (run), as limited by the `index` and `xxx`...
  for (rn in (1:n_runs)[index == xxx]) {
    
    # Print a status message to the console to keep track of what simulation is 
    # being worked on currently
    cat('\n', rn, ' ', file_label(output_files[rn]), ' ')
    flush.console()
    
    # Read in the simulation encounter history using the custom function 
    # read_input to create a data frame
    test <- read_input(output_files[rn])
    
    # Pull the grid cell numbers from the simulation encounter history
    GRD <- test$GridID
    
    # Replace the data frame of the simulation with just a vector of character
    # strings that are the encounter histories for each grid cell
    test <- test$ch
    
    # Reorder the grid cell numbers in a random order for selection
    use <- sample(match(GRDuse, GRD), gridTotal)
    
    # If the randomize argument is set to FALSE, switch back to all the grid 
    # cell numbers in order
    if (!is.null(randomize)) {
      if (!randomize) {
        use <- 1:gridTotal
      }
    }
    
    # If using variable sampling set up a sub folder for this run to save the 
    # sampling matrices
    if ((3 %in% Parameters$alt_model) |
        (4 %in% Parameters$alt_model) | 
        (5 %in% Parameters$alt_model) | 
        (6 %in% Parameters$alt_model)) {
      run_num <- substring(file_label(output_files[rn]), 
                           nchar(file_label(output_files[rn])) - 3)
      
      run_mat_folder <- paste0(samp_mat_folder, 
                               "/rn_", 
                               formatC(run_num, 
                                       flag = 0, 
                                       digits = 4, 
                                       format = "d"))
      if (!dir.exists(run_mat_folder)) {
        dir.create(run_mat_folder)
      }
    }
    
    # Sets a constant that will hold the previous detection probability value
    detPhold <- 1
    
    # # Set up a progress bar
    # pb_detp <- progress_bar$new(format = "Testing detection probabilities [:bar] :percent eta: :eta", 
    #                             total = length(Parameters$spatial_percents), 
    #                             clear = FALSE)
    
    # For each detection probabilities that will be analyzed...
    for (detPt in detP_test) {
      
      # cat('\n DetP: ', detPt)
      # flush.console()
      
      # Previous functions assumed a detection probability of 1, this will
      # adjust the detection probability to the values being tested by the
      # analysis, this test object will get rewritten with then new detection 
      # probability each time and thus the values for these probabilities have
      # been adjusted above
      test <- drop_det_p(ch = test,
                         detP = detPt)  
      
      # This adjusts the detection probability back to the original test value
      # for labeling the results
      detPhold <- detPhold * detPt
      
      # # Set up a progress bar
      # pb_mod <- progress_bar$new(format = "Testing alternate models [:bar] :percent eta: :eta", 
      #                            total = length(Parameters$alt_model), 
      #                            clear = FALSE)
      
      # For each alternate model that will be tested...
      for (altM in Parameters$alt_model) {
        
        # # Print a status message that just lets the user know where the 
        # # analysis is at 
        # cat('\n sampling model: ', case_when(altM == 0 ~ "annual",
        #                                      altM == 1 ~ "every other year",
        #                                      altM == 2 ~ "sampling matrix",
        #                                      altM == 3 ~ "spatially inconsistent", 
        #                                      altM == 4 ~ "partially spatially consistent", 
        #                                      altM == 5 ~ "fully variable"))
        # flush.console()
        # 
        # # Set up a progress bar
        # pb_grid <- progress_bar$new(format = "Testing grid percents [:bar] :percent eta: :eta", 
        #                               total = length(n_grid_sample), 
        #                               clear = FALSE)
        
        # For each number of grid cells that will be tested (multiple values 
        # because different percentages of the grid are getting tested)...
        for (n_grid in n_grid_sample) {  
          
          # cat('\n number grid cells: ', n_grid)
          # flush.console()
          
          if (n_grid != max(n_grid_sample) & (altM == 5 | altM == 6)) {
            # pb_grid$tick()
            next
            
          }
          
          # Remove objects from the environment that will get re-used to avoid any
          # accidental miscalculations
          suppressWarnings(rm("ch1"))
          
          # If variable sampling will be used set the encounter history to the
          # full set of encounter histories
          if (altM == 3 | altM == 4 | altM == 5 | altM == 6) {
            ch1 <- test
            grd1 <- GRD
            
          } else {
            
            # Using the random (or non-random in the case of `randomize` == FALSE) 
            # order of grid cells determined above, select a sub-sample of grid 
            # cells based on the number of cells being tested and pull their 
            # encounter histories
            ch1 <- test[use[1:n_grid]]   
            
            # Pull those grid cell numbers in case the numbering is different
            grd1 <- GRD[use[1:n_grid]]
            
          }
          
          # If the argument `FPCind` was set to TRUE, use the FPC function to 
          # calculate the finite population correction factor to be used
          fpc <- ifelse(FPCind, finite_pop_corr(n_grid, gridTotal), 1)
          
          # # Set up a progress bar
          # pb_visits <- progress_bar$new(format = "Testing visit variation [:bar] :percent eta: :eta", 
          #                               total = length(Parameters$n_visits_test), 
          #                               clear = FALSE)
          
          # For each total number of visits that will be tested (multiple values 
          # because different numbers of visits are getting tested)...
          for (n_visit in Parameters$n_visit_test) {
            
            # cat('\n number visits: ', n_visit)
            # flush.console()
            
            # Remove objects from the environment that will get re-used to avoid 
            # any accidental miscalculations
            suppressWarnings(rm("ch"))
            
            # Use the drop_visits function to re-sample encounter histories to 
            # only represent the number of visits that will be tested
            ch <- drop_visits(ch = ch1, 
                              n_visits, 
                              n_yrs, 
                              n_visit) 
            
            # # Set up a progress bar
            # pb_locper <- progress_bar$new(format = "Testing spatial percents [:bar] :percent eta: :eta", 
            #                            total = length(Parameters$spatial_percents), 
            #                            clear = FALSE)
            
            for (loc_percent in Parameters$spatial_percents) {
            
              # If partially spatially consistent sampling was selected, also 
              # create a sub folder for the location percentage
              if (altM == 4) {
                loc_per_folder <- paste0(run_mat_folder, 
                                         "/sp_percent_", 
                                         formatC(loc_percent * 100, 
                                                 flag = 0, 
                                                 digits = 1, 
                                                 format = "d"))
                if (!dir.exists(loc_per_folder)) {
                  dir.create(loc_per_folder)
                }
                
                enc_hist_folder <- paste0(run_mat_folder, 
                                          "/sp_percent_", 
                                          formatC(loc_percent * 100, 
                                                  flag = 0, 
                                                  digits = 1, 
                                                  format = "d"), 
                                          "/encounter_histories")
                if (!dir.exists(enc_hist_folder)) {
                  dir.create(enc_hist_folder)
                }
                
              } else if (altM != 4 & 
                         loc_percent != max(Parameters$spatial_percents)) {
                # pb_locper$tick()
                next
              }
            
              # Use the run_analysis function to analyze the encounter history
              sim_results <- run_analysis(n_yrs,
                                          ch, 
                                          n_visit, 
                                          sample_yr = altM, 
                                          FPC = ifelse(altM < 4, 
                                                       fpc,
                                                       1),
                                          sample_matrix = sample_matrix, 
                                          n_grid = n_grid,
                                          loc_percent = loc_percent,
                                          grid_min = Parameters$grid_min,
                                          grid_max = Parameters$grid_max, 
                                          alpha = Parameters$alpha, 
                                          beta = Parameters$beta, 
                                          base = Parameters$base)
                                          # This argument was in the original 
                                          # version but I didn't see where it 
                                          # would get used:
                                          # grdId = grd1 
            
              # If variable sampling was used, need to save both the sampling 
              # matrix and the simulation results that were returned
              if (altM == 3 | altM == 5 | altM == 6) {
                # First set up the file name of the text file to save to
                sim_mat_file <- paste0(run_mat_folder, "/", 
                                       detPt*100, "detP_",
                                       n_grid, "cells_",
                                       n_visit, "visits.txt")
                
                # Then write the table to the file
                write.table(sim_results$samp_mat, 
                            file = sim_mat_file,
                            row.names = FALSE,
                            col.names = FALSE)
              
                # Now write the simulation results to the sim_results file
                for (i in 1:nrow(sim_results$sim_results)) {
                  cat(c(unlist(sim_results$sim_results[i, ]),
                        n_grid,
                        n_visit,
                        detPhold,
                        altM,
                        loc_percent,
                        Parameters$grid_min,
                        Parameters$grid_max,
                        file_label(output_files[rn]), '\n'),
                      file = results_file,
                      append = T)
                }
              } else if (altM == 4) {
                # First set up the file name of the text file to save to
                sim_mat_file <- paste0(loc_per_folder, "/", 
                                       detPt*100, "detP_",
                                       n_grid, "cells_",
                                       n_visit, "visits.txt")
                
                # Then write the table to the file
                write.table(sim_results$samp_mat, 
                            file = sim_mat_file,
                            row.names = FALSE,
                            col.names = FALSE)
                
                # Set up a file to save the matching encounter history to
                enc_hist_file <- paste0(enc_hist_folder, "/", 
                                        detPt*100, "detP_",
                                        n_grid, "cells_",
                                        n_visit, "visits.txt")
                
                # Then write the encounter history
                write.table(sim_results$enc_hist, 
                            file = enc_hist_file, 
                            row.names = FALSE, 
                            col.names = FALSE)
                
                # Now write the simulation results to the sim_results file
                for (i in 1:nrow(sim_results$sim_results)) {
                  cat(c(unlist(sim_results$sim_results[i, ]),
                        n_grid,
                        n_visit,
                        detPhold,
                        altM,
                        loc_percent,
                        Parameters$grid_min,
                        Parameters$grid_max,
                        file_label(output_files[rn]), '\n'),
                      file = results_file,
                      append = T)
                }
              } else {
                
                # Add each row of simulation results to the output file as they are 
                # made for each model
                for (i in 1:nrow(sim_results)) {
                  cat(c(unlist(sim_results[i, ]),
                        n_grid,
                        n_visit,
                        detPhold,
                        altM,
                        loc_percent,
                        Parameters$grid_min,
                        Parameters$grid_max,
                        file_label(output_files[rn]), '\n'),
                      file = results_file,
                      append = T)
                }}
              # pb_locper$tick()
            }
            # pb_visits$tick()
          }
          # pb_grid$tick()
        }
        # pb_mod$tick()
      }
      # pb_detp$tick()
    }
    pb_run$tick()
  }
  
  # Print a status message for the user
  cat('\n')
  
  # Print the amount of time it took to run the analysis
  return(proc.time()[3] - time1)
}