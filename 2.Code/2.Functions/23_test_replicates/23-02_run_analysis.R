### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-24

## Function purpose: Run an analysis for an encounter history

#################################### Intro #####################################

# Name: run_analysis
# Description:  

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-24

################################# Arguments ####################################

# n_yrs: 
#     Number of seasons/years sampled
# ch:
#     A vector of encounter histories where each value in the vector is a 
#     character string of the encounter history for one grid cell 
# n_visit:
#     Number of visits to each site each season/year
# sample_yr:
#     Default: 0 ; An integer of value 0, 1, or 2 indicating the yearly sampling 
#     model used with 0 indicating grid cells are sampled every year, 1 
#     indicating grid cells are sampled every other year and 2 indicating 
#     consistent grid cells are sampled on an irregular sampling schedule
# FPC:
#     Default: 1; The finite population correction factor to be used  
# sample_martrix:
#     Default: NULL; Required to not be NULL if sample_yr is set to 2; A matrix
#     where each row represents a grid cell and each column represents a year of 
#     sampling, each cell contains a 1 or a 0 value indicating whether that 
#     corresponding grid cell was sampled (1) or not sampled (0) during the 
#     corresponding year

################################# Output #######################################

# sim_results:
#     A data frame containing select results of the occupancy analysis for the 
#     simulation being run  

################################################################################
## Function

### Analysis function for each observed encounter history
run_analysis <- function(n_yrs, 
                         ch = NULL,
                         n_visit = NULL, 
                         sample_yr = 0, 
                         FPC = 1, 
                         sample_matrix = NULL, 
                         n_grid = NULL,
                         loc_percent = 1,
                         grid_min = 1,
                         grid_max = 1, 
                         alpha = NULL, 
                         beta = NULL, 
                         base = 0){
  
  # When with `ch` is set to NULL, this will just set up an output data frame 
  # and return that to the parent function
  sim_results <- data.frame(p_est = 0,
                            trend = 0,
                            trendSE = 0, 
                            singular = 0,
                            matrix(0, 1, n_yrs))
  if (is.null(ch)) {
    return(sim_results)
  }

  # If the `ch` argument has a value, The rest of this analysis will get run
  
  # If `sample_yr` is set to zero, indicating that the analysis should be run 
  # with every year being sampled...
  if (sample_yr == 0) {
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1,length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))

    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1 # Local extinction rate
    test_ddl$Gamma$eps <- 1 # Colonization rate
    p.session <- list(formula = ~ session) 
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T, 
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance 
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
    
    # If `sample_yr` is set to one, indicating that the analysis should be run 
    # with every other year being sampled...
  } else if (sample_yr == 1) { 
    
    # Set up encounter histories to represent when only every other year is 
    # sampled for all grid cells
    ch_gap <- drop_years(ch, n_visit)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch_gap,
                            freq = rep(1, length(ch_gap)), 
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, 
                                                             n_yrs - 1))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    
    # Set up a gap matrix for modeling with RMark
    nt <- n_yrs %/% 2 - 1
    gap_matrix <- matrix(rep(c(1, 1, rep(0, 2 * nt)),
                             length.out = 2 * nt^2), 
                         nrow = 2 * nt, 
                         ncol = nt)
    
    # If the original number of years was an odd number, add an extra year to
    # the gap matrix 
    if (n_yrs %% 2 == 1) {
      gap_matrix <- rbind(cbind(gap_matrix, rep(0, nrow(gap_matrix))),
                          c(rep(0, ncol(gap_matrix)), 1))
    }
    
    # Set up more parameters to be used by the model in program MARK
    test_ddl$Epsilon$gap <- -1 * gap_matrix
    test_ddl$Gamma$gap <- gap_matrix
    p.even.indices <- which(as.numeric(test_ddl$p$session) %% 2 == 0)
    p.fixed.values <- rep(0, length(p.even.indices))
    p.session.fixed <- list(formula = ~ session,
                            fixed = list(index = p.even.indices,
                                         value = p.fixed.values))
    Epsilon.gapsampling <- list(formula = ~ -1 + gap,
                                share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.gapsampling,
                             p = p.session.fixed)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$ 
                                                real) == "p g1 s1 t1")])
  
    # If the model was created, adjust the outputs to reflect that only every
    # other year was sampled
    if (!is.null(RDoccupancy)) {
      derived_psi <- c(derived_psi, NA)
      new_derived_psi <- derived_psi[seq(1, n_yrs - 1, by = 2)]
      new_derived_psi_vcv <- derived_psi_vcv[seq(1, n_yrs - 1, by = 2),
                                             seq(1, n_yrs - 1, by = 2)]
    }
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1,seq(1, n_yrs - 1, by = 2))
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = new_derived_psi,
                                                      design = Trend_DM,
                                                      vcv = new_derived_psi_vcv))
    
    # If `sample_yr` is set to two, indicating that the analysis should be run 
    # with irregular years being sampled, this represents consistent cells that 
    # are sampled on irregular years so that the total number of cells sampled 
    # every year is different and the results are listed under the total number 
    # of cells that could be sampled that year (deemed irregular sampling)...
  } else if (sample_yr == 2) {    
    
    # Require that a `sample_matrix` is provided
    if (is.null(sample_matrix)) {
      stop('Argument "sample_matrix" is missing, with no default')
    }
    
    # Use the function drop_years to drop the appropriate years from the 
    # encounter histories based on the provided `sample_matrix`
    ch <- drop_years(ch,
                     n_visit,
                     samples = sample_matrix)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1, length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1
    test_ddl$Gamma$eps <- 1
    p.session <- list(formula = ~ session)
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
    
  } else if (sample_yr == 3) {
    stop("no sampling set up for alt_model 3, use alt_model 4 with 0% consistent")
    
    # If `sample year` is set to 3, assume that the total number of cells 
    # sampled each year is consistent but the cells themselves are different
    # each year (deemed variable sampling)...
  } else if (sample_yr == 4 & loc_percent == 0) {    
    
    # Set up a sampling matrix that samples the same number of cells but in 
    # inconsistent locations each year. First create and empty matrix
    sample_matrix <- matrix(0, 
                            nrow = length(ch),
                            ncol = n_yrs)
    
    # Then for each year in the matrix, randomly select the cells to be sampled
    for (i in 1:ncol(sample_matrix)) {
      
      cell_sampled <- sample(length(ch),
                             n_grid)
      
      # And use those cell numbers to replace the 0s in the matrix with 1s
      for (ii in 1:n_grid) {
        
        sample_matrix[cell_sampled[ii], i] <- 1
      }
    }
    
    # Use the function drop_years to drop the appropriate years from the 
    # encounter histories based on the provided `sample_matrix`
    ch <- drop_years(ch,
                     n_visit,
                     samples = sample_matrix, 
                     vari_samp = TRUE)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1, length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1
    test_ddl$Gamma$eps <- 1
    p.session <- list(formula = ~ session)
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
    
    # If `sample year` is set to 4, assume that the total number of cells 
    # sampled each year is consistent but the cells themselves are different
    # each year (deemed variable sampling)...
  } else if (sample_yr == 4 & loc_percent > 0) {    
    
    # Set up a sampling matrix that will hold what years which cells were sampled
    sample_matrix <- matrix(0, 
                            nrow = length(ch),
                            ncol = n_yrs)
    
    # For the first year in the matrix, randomly select all cells that will be
    # sampled
    cells_samp_yr1 <- sample(length(ch),
                             n_grid)
    
    # Replace the 0's of the sample matrix in the first year with 1's for the 
    # cells to be sampled
    sample_matrix[cells_samp_yr1, 1] <- 1
    
    # Next calculate the number of cells that will stay consistent each year
    num_consist <- round(n_grid * loc_percent)
    
    # Then randomly select the grid cells that will stay consistent (these must 
    # be cells that were sampled in year 1)
    cells_consist <- sample(cells_samp_yr1, 
                            num_consist, 
                            replace = FALSE)
    
    # Change the values for these consistent cells in the sample matrix
    sample_matrix[cells_consist, ] <- 1
    
    # Then for each year in the matrix, randomly select the rest of the cells to 
    # be sampled
    for (i in 2:ncol(sample_matrix)) {
      
      # First get the cells that are not consistently being sampled
      avail_cells <- which(sample_matrix[, i] == 0)
      
      # Then randomly sample the correct number from those cells
      cell_sampled <- sample(avail_cells,
                             n_grid - num_consist)
      
      # And use those cell numbers to replace the 0s in the matrix with 1s
      for (ii in 1:n_grid) {
        
        sample_matrix[cell_sampled[ii], i] <- 1
      }
    }
    
    # Use the function drop_years to drop the appropriate years from the 
    # encounter histories based on the provided `sample_matrix`
    ch <- drop_years(ch,
                     n_visit,
                     samples = sample_matrix, 
                     vari_samp = TRUE)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1, length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1
    test_ddl$Gamma$eps <- 1
    p.session <- list(formula = ~ session)
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
  } else if (sample_yr == 5) {    
    
    # Set up a sampling matrix that will hold what years wich cells were sampled
    sample_matrix <- matrix(0, 
                            nrow = length(ch),
                            ncol = n_yrs)
    
    # For each year randomly determine the number of cells that will be sampled 
    # this value must range between the min and max percents that were provided
    # in the parameter list
    for (i in 1:ncol(sample_matrix)) {
      
      # First determine the number of cells for that year
      num_cells_samp <- round(runif(1, min = grid_min, max = grid_max) * length(ch))
      
      # Then randomly sample the correct number from those cells
      cell_sampled <- sample(length(ch),
                             num_cells_samp)
      
      # And use those cell numbers to replace the 0s in the matrix with 1s
      for (ii in 1:n_grid) {
        
        sample_matrix[cell_sampled[ii], i] <- 1
      }
    }
    
    # Use the function drop_years to drop the appropriate years from the 
    # encounter histories based on the provided `sample_matrix`
    ch <- drop_years(ch,
                     n_visit,
                     samples = sample_matrix, 
                     vari_samp = TRUE)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1, length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1
    test_ddl$Gamma$eps <- 1
    p.session <- list(formula = ~ session)
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
  } else if (sample_yr == 6) {    
    
    # Set up a sampling matrix that will hold what years which cells were sampled
    sample_matrix <- matrix(0, 
                            nrow = length(ch),
                            ncol = n_yrs)
    
    # For each year randomly determine the number of cells that will be sampled 
    # this value must range between the min and max percents that were provided
    # in the parameter list
    for (i in 1:ncol(sample_matrix)) {
      
      # First determine the number of cells for that year
      num_cells_samp <- round((rbeta(1, alpha, beta) + base) * length(ch))
      
      # Then randomly sample the correct number from those cells
      cell_sampled <- sample(length(ch),
                             num_cells_samp)
      
      # And use those cell numbers to replace the 0s in the matrix with 1s
      for (ii in 1:n_grid) {
        
        sample_matrix[cell_sampled[ii], i] <- 1
      }
    }
    
    # Use the function drop_years to drop the appropriate years from the 
    # encounter histories based on the provided `sample_matrix`
    ch <- drop_years(ch,
                     n_visit,
                     samples = sample_matrix, 
                     vari_samp = TRUE)
    
    # Set up a data frame in the format required by RMark
    mark_data <- data.frame(ch = ch,
                            freq = rep(1, length(ch)),
                            stringsAsFactors = F)
    
    # Use RMark::process.data to set up the encounter history for analysis by 
    # the program MARK
    test_processed <- process.data(mark_data,
                                   model = "RDOccupEG",
                                   time.intervals = time_int(n_visit, n_yrs))
    
    # Set up the parameters to be used by the model in program MARK
    test_ddl <- make.design.data(test_processed)
    test_ddl$Epsilon$eps <- -1
    test_ddl$Gamma$eps <- 1
    p.session <- list(formula = ~ session)
    Epsilon.random.shared <- list(formula = ~ -1 + eps:time, 
                                  share = TRUE)
    model.parameters <- list(Epsilon = Epsilon.random.shared,
                             p = p.session)
    
    # Run model analysis in RMark and do not save an output file. This runs the 
    # function inside another function (try_mark) to catch errors
    RDoccupancy <- try_mark(mark(test_processed,
                                 test_ddl,
                                 model.parameters = model.parameters,
                                 silent = T,
                                 delete = T,
                                 output = F))
    
    # Pull psi (probability of occupancy) and P (detection probability) values 
    # from occupancy model results
    derived_psi <- try_n(RDoccupancy$results$derived[[1]][, 1])
    derived_psi_vcv <- try_n(RDoccupancy$results$derived.vcv[[1]])
    P_est <- try_n(RDoccupancy$
                     results$
                     real$
                     estimate[which(row.names(RDoccupancy$
                                                results$
                                                real) == "p g1 s1 t1")])
    
    # Set up columns to represent the design matrix
    Trend_DM <- cbind(1, 1:n_yrs)
    
    # Use RMark to compute estimated effects, standard errors and variance
    Random.effects.model <- try_w(var.components.reml(theta = derived_psi,
                                                      design = Trend_DM,
                                                      vcv = derived_psi_vcv))
  }
  
  # As long as the model was created...
  if (!is.null(Random.effects.model)) {
    
    # Add the estimated trend in occupancy and its standard error (as calculated
    # by the fpc_trend_se function) for the simulation to the simulation results
    sim_results$trend <- Random.effects.model$beta[2, 1]
    sim_results$trendSE <- fpc_trend_se(Random.effects.model,
                                        k = nrow(Trend_DM),
                                        FPC)
  }
  
  # If a derived psi (probability of occupancy) value was recorded...
  if (!is.null(derived_psi)) {
    
    # Add those derived psi values to the correct columns in the simulation
    # results table
    sim_results[1, grep('X', names(sim_results))] <- matrix(derived_psi,
                                                            nrow = 1)
    
    # Add the estimated probability of detection (P) to the simulation results 
    sim_results$p_est <- P_est
    
    # Add the number of singularities to the simulation results table
    sim_results$singular <- try_n(length(RDoccupancy$results$singular))
    
  } 
  
  if (sample_yr >= 3) {
    return(list(sim_results = sim_results,
                samp_mat = sample_matrix, 
                enc_hist = ch))
  } else {
    return(sim_results)
  }
}



