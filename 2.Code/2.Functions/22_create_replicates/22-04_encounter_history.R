### Power
### Jordan Heiman, Martha Ellis
## Date: 2023-03-21

## Function purpose: Simulates an encounter history for all cells in a grid

#################################### Intro #####################################

# Name: encounter_history
# Description:  Creates a simulated population and trend for that population 
#               then simulates the encounter history for all cells in a sampling 
#               grid over multiple years for that population

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-21

################################# Arguments ####################################

# map:
#     Raster of probability of use for species of interest
# Parameters:
#     Parameters for simulation as entered through enter_parameters functions or
#     as manually created in a list 
# n_cells:
#     Number of grid cells in the sampling grid 
# grid_layer:
#     Vector that is the length of all the pixel which holds the grid cell 
#     number that the respective pixel is assigned to, with zeros representing 
#     pixels that should not be assigned to any grid cell
# printN:
#     A TRUE/FALSE value indicating whether or not to write a table of 
#     population totals after calculating them
# rn:
#     The current run number that the encounter history is for
# filter.map:
#     Optional; map that will be used to filter the raster
# showSteps:
#     Defaults to FALSE; argument indicating whether or not to plot maps to show 
#     the steps taken in the simulation process

################################# Output #######################################

# encounter_history:
#     Depending on the arguments, returns an encounter history for all grid 
#     cells in all years of the simulation

################################################################################
## Function

encounter_history <- function(map, 
                              Parameters, 
                              n_cells, 
                              grid_layer,
                              printN, 
                              rn, 
                              filter.map = NULL,
                              showSteps = F,
                              out_folder = NULL){
  
  # Set up a progress bar
  pb <- progress_bar$new(format = "Creating replicates [:bar] :percent eta: :eta", 
                         total = max(Parameters$n_yrs), 
                         clear = FALSE)
  
  #   0. Set up for script                                                  ####
  # Use check_parameters function to make sure the parameters have all required 
  # options included
  Parameters <- check_parameters(Parameters, 
                                 list(n_cells = n_cells,
                                      grid_layer = grid_layer,
                                      filter.map = filter.map,
                                      printN = printN,
                                      rn = rn,
                                      showSteps = showSteps))
  
  # Pull out the desired number of visits and the number of years from the 
  # parameters list
  n_visits <- Parameters$n_visits
  n_yrs <- Parameters$n_yrs
  
  # If the `showSteps` argument is set to TRUE, print text to the console that 
  # landscapes are being built
  if (showSteps == T) {
    cat('\nBuilding landscape...\n')
    flush.console()
  }
  
  # If the provided grid is NULL..
  if (is.null(grid_layer)) {
    
    # Use the check_map function to check the map and filter
    map <- check_map(map, filter.map)
    
    # Then build a sampling grid using the create_grid function
    grid_layer <- create_grid(map, Parameters, filter.map)
    
    # Retrieve the number of grid cells from the sampling grid vector
    n_cells <- length(unique(grid_layer)[unique(grid_layer) > 0])
  }
  
  # Create an empty matrix for the encounter history, each row is a grid cell 
  # and each column is a year
  encounter_history <- matrix(0, nrow = n_cells, ncol = n_yrs)
  
  #   1. Place individuals                                                  ####
  # Check that the starting population provided in the parameters is at least 0, 
  # error out if it is not
  if (Parameters$N <= 0) { 
    stop('Parameters$N <= 0')
  }
  
  # Create a vector for each individual type with pixels used
  individs <- add_n(dN = Parameters$N,
                    map, 
                    Parameters)
  
  # Turn that into a data frame with the types of individuals and the pixel each 
  #individual occupies
  pop.df <- pop_dataframe(individs)
  
  # Create a vector of lambda values depending on the trend type identified, 
  # default to an abundance - exponential trend
  if (is.null(Parameters$trendtype) 
     | Parameters$trendtype == 'abundance-exponential') {
    
    # If the trend type is abundance - exponential, lambda is the same every 
    # year for as many years as the simulation
    lmda <- rep(Parameters$lmda, length.out = (n_yrs - 1))
    
    # If the trend type is abundance - linear...
  } else if (Parameters$trendtype == 'abundance-linear') {
    
    # Annual lambda is calculated...
    lmda <- local({
      
      # By calculating the number of individuals to drop from the population as
      # (N*(1-totallambda)^years)/(years)
      indToDrop <- Parameters$N * (1 - Parameters$lmda^(n_yrs - 1))/(n_yrs - 1)
      
      # Then determining the annual lambda based on the number of individuals 
      # that should be lost total as 1-(totallost/(totalkept * yearnumber - 1))
      return(1 - indToDrop/(Parameters$N - indToDrop * (1:(n_yrs - 1) - 1)))
    })
    
    # Currently, scripts are only set up to use an abundance - exponential or an 
    # abundance - linear trend, if anything else is requested it will error
  } else { 
    stop('Unknown trend type') 
  }
  
  #   2. Make use surface                                                   ####
  # Create a vector of the probability of use by at least one individual in the 
  # simulated population for all pixels in the map
  useLayer <- build_use_layer(map,
                              individuals = individs, 
                              Parameters)
  
  # Start a running list of the use layers for each year to get totaled later
  # The use layers include all pixels.
  use_lst <- list(useLayer[which(values(map) > 0)])

  #   3. Calculate probability by grid                                      ####
  # Create an empty matrix to fill with the probability of presence and 
  # detection for each cell in each year
  P.pres <- matrix(0, nrow = n_cells, ncol = n_yrs)
  
  # Use the prob_pres function to calculate the probability that an individual 
  # is present and detected for each grid cell in the sampling grid for the 
  # first year of simulations
  P.pres[, 1] <- prob_pres(surface = useLayer, 
                           gridvec = grid_layer)
  
  # Create a vector with a length equal to the number of years that are being 
  # simulated, starting with a total population count of 0 for all years
  N <- rep(0, n_yrs)
  
  # For the first year, fill in the provided total starting population based on 
  # how many individuals were placed on the landscape 
  N[1] <- nrow(pop.df)
  
  #      3b. (Optional) Output plots with first year data                   ####
  # If the argument `showSteps` is set to TRUE, print plots...
  if (showSteps) {
    
    # Create placeholder objects
    x <- NULL
    y <- NULL
    z <- NULL
    type <- NULL
    
    # If the map is projected in latitude and longitude, calculate the aspect 
    # ratio for the map
    aspRatio <- local(
      if (grepl('longlat', crs(map, proj = TRUE))) {
        bbox <- as(extent(ext(map)[1:4]), "SpatialPoints")
        proj4string(bbox) <- CRS(ifelse(class(map) == "RasterLayer", 
                                        proj4string(map),
                                        crs(map)))
        return(unname(mapasp(bbox)))
      } else {
        return(1)
      })
    
    if (class(map) == "SpatRaster") {
      names(map) <- "z"
    }
    
    map_coord <- xyFromCell(map, 1:ncell(map))[, 1:2]
    
    # Create the first plot, just the habitat layer that was provided, save to 
    # the local environment to get reused below
    P1 <- ggplot(data = data.frame(map_coord,
                                   z = terra::values(map)),
                 aes(x = x, 
                     y = y)) +
      geom_raster(aes(fill = z)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_fixed(ratio = aspRatio) +
      scale_fill_continuous(low = 'white',
                            high = grey(.7), 
                            limits = c(0, 1)) +
      labs(fill = 'Habitat suitability',
           title = 'User supplied habitat suitability map')
    
    # Set up a simple function to randomly select a specified number of random 
    # colors from all colors with the exception of any white, grey, or black 
    # colors
    random.colors <- function(n) {
      sample(colors()[!grepl('white|gr.y|black', colors())], n, replace = T)
    }
    
    # Print a map of the sampling grid with each grid cell colored a random 
    # color using the random.color function above
    P2 <- ggplot(data = data.frame(map_coord, 
                                  z = factor(grid_layer, 
                                             levels = 0:n_cells)),
                aes(x = x, 
                    y = y, 
                    fill = z)) +
      geom_raster() +
      scale_fill_manual(values = c("white", 
                                   random.colors(n_cells)), 
                        guide = "none") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_fixed(ratio = aspRatio) +
      labs(title = 'Grid (colored for contrast only)')
    
    # Print a map of the initial population with the user provided habitat map 
    # as a base layer and different types of individuals represented by 
    # different colors
    P3 <- P1 +
      geom_point(data = pop_dataframe(pop.list = individs, map),
                 aes(shape = factor(type), 
                     colour = factor(type))) +
      # scale_colour_hue(l = 30, 
      #                  breaks = c(1, 2),
      #                  labels = c("Male", "Female")) + 
      # scale_shape_manual(breaks = c(1, 2),
      #                    labels = c("Male", "Female"),
      #                    values = c(16, 17)) +
      labs(fill = 'Habitat suitability',
           shape = 'Type',
           colour = 'Type',
           title = 'Individual activity center locations - Year 1')
    
    
    # Create an example of a random individual's probability of use raster 
    ExampleLayer <- build_use_layer(map, 
                                    list(individs[[1]][sample(length(individs[[1]]), 1)]), 
                                    Parameters, 
                                    Example = T)
    
    # Use that example raster to create a plot
    P4 <- ggplot(data.frame(map_coord, 
                            z = ExampleLayer),
                 aes(x = x, 
                     y = y, 
                     fill = z)) +
      geom_raster() +
      scale_fill_gradientn(colours = c('white',
                                       rev(heat.colors(20)))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_fixed(ratio = aspRatio) +
      labs(fill = ' P(use)  \nby pixel',
           title = 'Probability of use for one individual - example')
    
    # Create a plot showing the probability of use by at least one individual for
    # the entire simulated population
    P5 <- ggplot(data.frame(map_coord, 
                            z = useLayer),
                 aes(x = x, 
                     y = y,
                     fill = z)) + 
      geom_raster() +
      # xlim(575000, 625000) +
      # ylim(5150000, 5200000) +
      scale_fill_gradientn(colours = c('white',
                                       rev(heat.colors(20)))#, 
                           #limits = c(0, 0.001)
                           ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      coord_fixed(ratio = aspRatio) +
      labs(fill = 'Availability\n  by pixel',
           title = 'Probability of at least one individual present - Year 1')
    
    # Create a plot showing the probability of presence of at least one
    # individual by grid cell
    P6 <- ggplot(data.frame(map_coord,
                            z = c(0, P.pres[, 1])[match(grid_layer, 
                                                        unique(c(0, grid_layer)))]),
                 aes(x = x,
                     y = y, 
                     fill = z)) +
      geom_raster() +
      scale_fill_gradientn(colours = c('white', rev(heat.colors(20)))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      # xlim(575000, 625000) +
      # ylim(5150000, 5200000) +
      coord_fixed(ratio = aspRatio) +
      labs(fill = 'Availability\n   by cell',
           title = 'Probability of at least one individual present by cell - Year 1')
    
    plot_lst <- list(P1_Habitat = P1, 
                     P2_Grid = P2, 
                     P3_Individuals = P3, 
                     P4_Prob_use_Single_Ind = P4, 
                     P5_Prob_Presence = P5, 
                     P6_Prob_Presence_Cell = P6)
    
    plots_folder <- paste0(out_folder, "/plots")
    if (!dir.exists(plots_folder)) {
      dir.create(plots_folder)
    }  
    
    filenames <- paste0("run_", rn, "_", names(plot_lst), "_Y1.jpeg")
    
    if (rn == 1) {
      
      pwalk(list(filenames, plot_lst), 
            ggsave, 
            path = plots_folder, 
            width = 11,
            height = 8.5,
            units = "in")
      
    } else {
      
      pwalk(list(filenames[3:length(plot_lst)], 
                 plot_lst[3:length(plot_lst)]), 
            ggsave, 
            path = plots_folder, 
            width = 11,
            height = 8.5,
            units = "in")
      
    }
  }
  
  #   4. Sample detections in the first year                                ####
  # Using the probability of presence and detection for each cell and a binomial 
  # distribution, determine an encounter history for every grid cell for the 
  # first year for as many visits as was provided in the parameters
  encounter_history[, 1] <- sapply(1:n_cells,
                                   function(i){
                                     paste(rbinom(n = n_visits,
                                                  size = 1,
                                                  prob = P.pres[i, 1]), 
                                           collapse = '')
                                     })
  
  pb$tick()
  
  #   5. Loop over years to fill in encounter_history                       ####
  # For every year of the simulation...
  if (n_yrs > 1) {
    
    # Starting with year 2...
    for (tt in 2:n_yrs) {
      
      #   5a. Calculate population change between t and t+1                 ####
      # Using the lambda per year vector created earlier, determine the 
      # population change that would need to occur between years. `nrow(pop.df)` 
      # provides the starting number of individuals for the population
      dN <- nrow(pop.df) * (lmda[tt - 1] - 1) 
      
      # This then turns the change in population to an integer where the chance 
      # of rounding up or down is a random value from a binomial distribution 
      # with a probability of success equal to the remainder dividing the
      # population change by 1
      dN <- floor(dN) + rbinom(1, 1, prob = dN %% 1)
      
      #   5b. Implement population change                                   ####
      # If the population change is greater than zero, meaning the population 
      # will be growing...
      if (dN > 0) {
        
        # Determine the locations of the new individuals using the add_n 
        # function, this will return the pixels that will be the new activity 
        # centers for individuals
        new.ind <- add_n(dN, map, Parameters, pop.df)
        
        # Then use the build_use_layers function to build a new use layer for 
        # these new individuals and compile that with the current probability of 
        # use of at least one individual, this must be done using the 
        # probability of no use multiplied together then converted back in order 
        # for it to be the probability of at least one individual
        useLayer <- 1 - (1 - useLayer) * (1 - build_use_layer(map,
                                                              new.ind, 
                                                              Parameters))

        # Add these new individuals to the population data frame        
        pop.df <- rbind(pop.df, pop_dataframe(new.ind))
        
        # Otherwise, if the change in population is negative, meaning the 
        # population is declining
      } else if (dN < 0) {
        
        # Determine which individuals to lose from the population using the 
        # drop_n function, this will provide a list of the id of the individuals
        # removed and the pixels of those individuals sorted by individual type
        lost.ind <- drop_n(abs(dN),
                           map,
                           Parameters,
                           pop.df) 
        
        # Adjust the probability of use of at least one individual by removing 
        # the probability of those individuals from the values, this must be 
        # done using the probability of no use multiplied together then 
        # converted back in order for it to be the probability of at least one 
        # individual
        useLayer <- 1 - (1 - useLayer)/(1 - build_use_layer(map, 
                                                            individuals = lost.ind[[2]], 
                                                            Parameters))
        
        # If this created any NA values in the use layer, change those values to 
        # zero instead
        useLayer[is.na(useLayer)] <- 0
        
        # Remove the individuals from the population data frame
        pop.df <- pop.df[-lost.ind[[1]], ] 
      }
      
      individ_yr <- list()
      for (i in 1:max(pop.df$type)) {
        type_ind <- filter(pop.df, type == i) %>% 
          select(locID) %>% 
          unlist
        names(type_ind) <- NULL
        individ_yr[[i]] <- type_ind
      }
      
      #   5c. Sample detections and update encounter_history                ####
      # Add the probability of an individual being present and detected for each 
      # cell to the appropriate year column in the `P.pres` matrix
      P.pres[, tt] <- prob_pres(useLayer, grid_layer)
      
      # Add the use layer to the running use layer list
      use_lst[[tt]] <- useLayer[which(values(map) > 0)]
      
      if (showSteps) {
        
        P3 <- P1 +
          geom_point(data = pop_dataframe(individ_yr, map),
                     aes(shape = factor(type), 
                         colour = factor(type))) +
          scale_colour_hue(l = 30) +
          labs(fill = 'Habitat suitability',
               shape = 'Type',
               colour = 'Type',
               title = paste0('Individual activity center locations - Year ',
                              tt))
        
        P5 <- ggplot(data.frame(map_coord, z = useLayer),
                     aes(x = x, y = y, fill = z)) + 
          geom_raster() +
          scale_fill_gradientn(colours = c('white', rev(heat.colors(20)))) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          coord_fixed(ratio = aspRatio) +
          labs(fill = 'Availability\n  by pixel',
               title = paste0('Probability of at least one individual present - Year ', 
                              tt))
        
        P6 <- ggplot(data.frame(map_coord,
                                z = c(0, P.pres[, tt])[match(grid_layer, 
                                                             unique(c(0, grid_layer)))]),
                     aes(x = x, y = y, fill = z)) +
          geom_raster() +
          scale_fill_gradientn(colours = c('white', rev(heat.colors(20)))) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          coord_fixed(ratio = aspRatio) +
          labs(fill = 'Availability\n   by cell',
               title = paste0('Probability of at least one individual present by cell - Year ',
                              tt))
        
        plot_lst <- list(P3_Individuals = P3, 
                         P5_Prob_Presence = P5,
                         P6_Prob_Presence_Cell = P6)
        
        if (!dir.exists(plots_folder)) {
          dir.create(plots_folder)
        }  
        
        filenames <- paste0("run_", rn, "_", names(plot_lst), "_Y", tt, ".jpeg")
        
        pwalk(list(filenames, 
                   plot_lst), 
              ggsave, 
              path = plots_folder, 
              width = 11,
              height = 8.5,
              units = "in")
      }
      
      # Add the total population abundance to the population abundance matrix 
      # for the appropriate year
      N[tt] <- nrow(pop.df)
      
      # Then add an encounter history for the appropriate year to the encounter
      # history matrix using the binomial distribution again 
      encounter_history[,tt] <- sapply(1:n_cells,
                                       function(i){
                                         paste(rbinom(n = n_visits, 
                                                      size = 1,
                                                      prob = P.pres[i, tt]),
                                               collapse = '')
                                       })
      
      pb$tick()
      
      # Now end the year loop and if statement 
    }}
  
  
  
  
  
  
  #   6. Save simulation information                                        ####
  # Set up a data frame containing some summary information about the population
  PopulationTotals <- data.frame(rn = rn,
                                 Year = 1:n_yrs - 1,
                                 N = N,
                                 # Calculate psi (occupancy) based on one visit
                                 truePsi_1visit = unname(apply(P.pres, 
                                                               2, sum)/nrow(P.pres)),
                                 # Calculate psi (occupancy) based on the maximum 
                                 # number of visits provided in the parameters
                                 truePsi_MaxVisits = unname(apply(P.pres, 
                                                                  2, function(x){
                                                                    sum(1 - (1 - x)^n_visits)
                                                                  })/nrow(P.pres)),
                                 # Calculate the asymptotic psi (occupancy) based 
                                 # on if an infinite number of visits were possible
                                 truePsi_Asymptotic = unname(apply(P.pres,
                                                                   2, function(x){
                                                                     sum(x > 0)
                                                                     })/nrow(P.pres)),
                                 truePsi_Pixel = unlist(lapply(use_lst, 
                                                               function(x) {
                                                                 sum(x > 0)/length(x)
                                                                 })))
    
  # Save the population summary table to the file designated by the parent 
  # function, if `showSteps` is set to TRUE this will not happen and the 
  # summary table will just be returned to the parent function
  write.table(PopulationTotals,
              file = printN, 
              row.names = F,
              col.names = ifelse(rn == 1, T, F),
              append = T)
  
  # If the `showSteps` argument was set to TRUE...
  if (showSteps) {
    
    # Print a status message
    cat(paste0('\nPopulation totals by year for run ', rn, '...\n'))
    flush.console()
    
    # Print the population summary table to the console without year zero
    print(PopulationTotals[, -1],
          row.names = F)
    
    # TSave the grid file as a raster if it is the first run
    if (rn == 1) {
      writeRaster(setValues(map, grid_layer), 
                  paste0(out_folder, '/ExampleGrid.tif'), 
                  overwrite = T)
      cat("  Saved as", paste0(out_folder, '/ExampleGrid.tif'), "\n")
    }
    
  # Print a status message
  cat('\nOutputting P.pres and encounter histories by year...\n')
  
  }  
  
  # Create a data frame with the encounter histories for each cell and year as 
  # well as the probability of presence and detection, set the column names to 
  # appropriate names, and arrange the columns by year 
  pres_encounter <- data.frame(round(P.pres, 3), encounter_history)
  names(pres_encounter) <- paste(rep(paste0('Yr', 1:n_yrs), 2),
                                 rep(c('p', 'ch'), each = n_yrs), sep = '.')
  pres_encounter <- pres_encounter[, order(rep(1:n_yrs, 2))]
  
  # Then return this encounter history to the parent function
  write.table(pres_encounter,
              paste0(dirname(printN), 
                     "/prob_presence_and_encounter_hist", 
                     formatC(rn, 
                             digits = 3, 
                             format = "d",
                             flag = "0"), 
                     ".txt"),
              row.names = F,
              col.names = T)
  
  # Return the encounter history data frame as just zeros and ones
  return(apply(encounter_history, 1, paste, collapse = ''))

}
