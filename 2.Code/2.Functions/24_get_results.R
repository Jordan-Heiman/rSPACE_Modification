### Power
### Jordan Heiman
## Date: 2023-03-29

## Function purpose: Creates a first pass power plot for simulations

#################################### Intro #####################################

# Name: get_results
# Description:  Using the get_data and sum_data functions, creates a power plot 
#               with detection probability set to 1 for the various simulations 
#               that were run

################################## Creation ####################################

# Author: Jordan Heiman
# Date Updated: 2023-03-29

################################# Arguments ####################################

# folder:
#       Folder path for folder that contains both the simulation results text 
#       file and the parameters data file, these files can be nested within 
#       other folders in the folder provided
# CI:
#       Default: 0.95; The desired confidence interval to use for the trend 
#       analysis
# returnData: 
#       Default: 1; An integer of 0, 1 or 2 indicating which data frame if any 
#       to return if any. 0: do not return a data frame; 1: return the full data 
#       frame from the simulation results text file with an included count
#       column indicating whether or not the simulation detected the known trend
#       2: return a summary data frame with the simulation results summarized 
#       for each scenario
# plot:
#       Default: TRUE; A TRUE/FALSE argument indicating whether or not to  
#       display a plot of the simulation results

################################# Output #######################################

# NULL OR dta OR dtaS:
#       Dependent on the `returnData` argument either nothing is returned, the 
#       full data frame from the simulation results text file with an included 
#       count column indicating whether or not the simulation detected the known 
#       trend is returned or a summary data frame with the simulation results 
#       summarized for each scenario is returned

################################################################################
## Function

get_results <- function(folder, 
                        sing_max = 5,
                        CI = 0.95, 
                        returnData = 1, 
                        plot = T) {  
  
  list.files("./2.Code/2.Functions/24_get_results",
             full.names = TRUE) %>% 
    lapply(., source) %>% 
    invisible()
  
  # Pull in the simulation results with a count column for plotting using the 
  # get_data function
  dta <- get_data(folder = folder,
                  sing_max = sing_max,
                  CI = CI) 
  
  # Summarize the data using the sum_data function
  dtaS <- sum_data(dta)
  
  # Set up some placeholder objects
  count <- n_runs <- n_grid <- n_visits <- detP <- alt_model <- loc_per <- NULL
  
  # If `plot' was set to TRUE, create a power plot
  if (plot) {
    
    # Display a plot using only simulations with a detection probability of 1
    print(ggplot(subset(dtaS, detP < 1 & alt_model < 4), 
                 # Set the x-axis as the number of grid cells and the y-axis as
                 # the proportion of simulations that were able to detect the 
                 # known trend in the population
                 aes(x = n_grid, y = (count/n_runs),
                     # Group these simulations by the number of visits per grid 
                     # cell per season, the detection probability and what 
                     # sampling model (annual, biannual, etc.) was used
                     group = interaction(n_visits, detP, alt_model, loc_per))) +
            # Color the plot lines by the number of visits per grid cell per
            # season
            geom_line(aes(colour = n_visits, 
                          # Use different line types for different detection 
                          #probabilities
                          linetype = factor(detP)), 
                      # Set the line width as a constant size
                      linewidth = 1.25) +
            # Set up the legend for the coloration of the lines with an
            # appropriate label
            scale_colour_gradient(name = "# visits",
                                  # Set the type of legend to use (color bar vs 
                                  # legend)
                                  guide = "legend") +
            # Set up the legend for the line types with an appropriate label for 
            # the simulated detection probability
            scale_linetype_discrete(name = expression(p["sim"])) +
            # Limit the y-axis to range between 0 and 1
            scale_y_continuous(limits = c(0, 1)) +
            # Label the x- and y-axis appropriately
            labs(x = "Number of cells sampled", 
                 y = "Detected trend/Number of replicates",
                 # Title the plot with the name of the simulation that was 
                 # created and used as the folder name
                 title = basename(folder)) +
            # Use facet wrap to separate the plot by the sampling model (annual, 
            # biannual, etc.) that is represented 
            facet_wrap( ~ alt_model))
  }
  
  # Depending on what the argument `returnData` was set to return either the 
  # full data or the summarized data
  if (returnData == 1) {
    return(dta)
  }
  if (returnData == 2) {
    return(dtaS)
  }
}