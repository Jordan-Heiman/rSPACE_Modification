#   2.0 Option to test replicates                                           ####
# Test the power of replicates that were created by the create replicate section
# above. This can be run on its own if replicates have been previously created.
if (svDialogs::dlg_message("Test replicates?", 
                           type = "yesno")$res == "yes") {
  
  # Create a table to hold information about the sampling strategy. 0-2 were 
  # original to rSPACE, the rest have been added. Three is skipped as it was
  # created then deemed unnecessary and I never went back to renumber things
  model_tbl <- data.table(name = c("Annual", "Biannual", "Sample Matrix", 
                                   # Different cells are selected every season, 
                                   # optional to vary the percentage that is 
                                   # allowed to change:
                                   "Spatially Inconsistent", 
                                   # Different cells are selected every season, 
                                   # and the total number of cells is drawn from
                                   # a uniform distribution with a user defined
                                   # range:
                                   "Fully Variable - Uniform", 
                                   # Different cells are selected every season, 
                                   # and the total number of cells is drawn from
                                   # a beta distribution with a user defined
                                   # shape parameter values:
                                   "Fully Variable - Beta"),
                          number = c(0, 1, 2, 4, 5, 6))
  
  # Have the user select the existing simulation to test
  sim_name <- select.list(list.dirs(output_folder, 
                                    recursive = FALSE,
                                    full.names = FALSE), 
                          graphics = TRUE,
                          title = "Choose Existing Simulation")
  
  # This is a placeholder for later
  change <- TRUE
  
  # Check for existing testing parameters in the simulation folder
  if (file.exists(paste0(output_folder, "/", sim_name, 
                         "/output/Parameters_testing.Rdata"))) {
    
    # If those parameters exist, load them into the environment
    params <- local({load(paste0(output_folder, "/", sim_name, 
                                 "/output/Parameters_testing.Rdata"))
      Parameters})
    
    # And confirm with user that the same testing parameters are desired
    change <- svDialogs::dlg_message(paste("Current testing parameters: ", 
                                           "Number of visits: ", 
                                           paste0(params$n_visit_test, 
                                                  collapse = ", "),
                                           "Detection Probability: ",
                                           paste0(params$detP_test,
                                                  collapse = ", "),
                                           "Grid Percentage: ", 
                                           paste0(params$grid_sample, 
                                                  collapse = ", "), 
                                           "Sampling Model:", 
                                           paste0(model_tbl[number %in%
                                                              params$alt_model,  
                                                            name], 
                                                  collapse = ", "),
                                           "Change these settings?"),
                                     type = "yesno")$res
    
  } 
  
  # If changing the parameters was selected and the testing parameters were 
  # saved in the simulation folder
  if (change == "yes" | 
      !file.exists(paste0(output_folder, "/", sim_name, 
                          "/output/Parameters_testing.Rdata"))) {

    
    # If spatial inconsistency is selected, the user needs to provide the 
    # percentages of cells that should be kept consistent each season. 
    if (4 %in% params$alt_model) {
      params$spatial_percents <- svDialogs::dlg_list(
        seq(.05, 1, 0.05),
        preselect = seq(.05, .95, 
                        by = .25),
        multiple = TRUE, 
        title = "Percent of cells to keep spatially consistent")$res %>% 
        as.numeric()
    } else {
      params$spatial_percents <- 1
    }
    
    # If fully variable from a uniform distribution is selected, the user must 
    # provide a maximum and minimum percent of the landscape to sample each 
    # season
    if (5 %in% params$alt_model) {
      grid_range <- svDialogs::dlg_input(
        paste0("Enter a minimum and maximum percent of the landscape to sample ",
               "for each year in the format (min, max)"),
        default = "0.01, 0.10")$res %>% 
        strsplit(", ") %>% 
        unlist() %>% 
        as.numeric()
      params$grid_min <- grid_range[[1]]
      params$grid_max <- grid_range[[2]]
    } else {
      params$grid_min <- 1
      params$grid_max <- 1
    }
    
    # If fully variable from a beta distribution is selected, the user must 
    # provide the alpha and beta shape parameters for the distribution. The 
    # defaults provide a distribution where the average percent of the landscape
    # sampled is approximately 4.8%, and sampling more than 20% of the landscape 
    # is highly unlikely. 
    if (6 %in% params$alt_model) {
      beta_params <- svDialogs::dlg_input(
        pate0("Enter the alpha and beta parameters for the beta distribution ",
              "seperated by a comma."),
        default = "1.5, 30")$res %>% 
        strsplit(", ") %>% 
        unlist() %>% 
        as.numeric()
      params$alpha <- beta_params[[1]]
      params$beta <- beta_params[[2]]
      
      # In order to also test baseline sampling, where a small percentage of the 
      # landscape is always sampled, the user must provide the baseline percent
      base <- svDialogs::dlg_input(
        paste0("Enter the lowest percentage of the landscape to be sampled as ",
               "a decimal, for example for 5% baseline sampling, enter 0.05"),
        default = "0")$res %>% 
        as.numeric()
      params$base <- params$grid_min <- base
    } else {
      params$alpha <- NULL
      params$beta <- NULL
      params$base <- NULL
    }
  }
    
  # Suggest a name for the results file, this should always include _sim_results 
  # and current notation is n = PERCENT of grid cells sampled; v = number of 
  # visits tested; d = psim value
  if (!(6 %in% params$alt_model)) {
    results_file <- paste0("n", ifelse(length(params$grid_sample) > 1,
                                       "range", 
                                       params$grid_sample*100), 
                           "_v", paste(sort(params$n_visit_test), collapse = ""), 
                           "_d", paste(sort(params$detP_test)*100, collapse = ""),
                           "_mod", paste(sort(params$alt_model), collapse = ""),
                           "_sim_results.txt")
  } else {
    results_file <- paste0("n", ifelse(length(params$grid_sample) > 1,
                                       "range", 
                                       params$grid_sample*100), 
                           "_v", paste(sort(params$n_visit_test), collapse = ""), 
                           "_d", paste(sort(params$detP_test)*100, collapse = ""),
                           "_", gsub("//.", "", params$alpha), 
                           "_", params$beta, 
                           "_", gsub("//0.0", "$", params$base),
                           "_sim_results.txt") 
  }
  
  results_file <- svDialogs::dlg_input("Results file name:",
                                       default = results_file)$res
  
  # If that sim_results file already exists, ask if it should be overwritten, 
  # this is often used if testing was never fully run. Testing can pick up where 
  # it left off to some extent but at least one full replicate needs to have 
  # been tested
  if (file.exists(paste0(output_folder, "/", sim_name, "/output/", results_file))) {
    overwrite <- ifelse(svDialogs::dlg_message("Overwite exisiting results file?",
                                               type = "yesno")$res == "yes",
                        TRUE,
                        FALSE)
    
  } else {
    overwrite <- FALSE
  }
  
  # For debugging purposes, provide the option to step through the 
  # test_replicates function
  if (svDialogs::dlg_message("Step through testing?", 
                             type = "yesno")$res == "yes") {
    
    # Create all the variables needed for debugging
    folder <- paste0(output_folder, "/", sim_name)
    Parameters <- params
    SubPop <- NULL
    sample_matrix <- NULL
    xxx <- 1
    max_xxx <- 1
    min_xxx <- 1
    base.name <- paste0(sim_name, "_")
    results.file <- results_file
    n_runs <- NULL
    FPCind <- TRUE
    skipConfirm <- T
    overwrite <- overwrite
    add <- !overwrite
    randomize <- T
    
  } else {
    
    # Test the replicates based on above information that was provided 
    test_replicates(folder = paste0(output_folder, "/", sim_name),
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
                    randomize = T)
  }
}

# rMARK does not seem to do a great job cleaning up it's mess of files. This can
# go away if we switch away from MARK. Typically I would never run a file.remove 
# without extreme precautions. Makes me nervous to leave this in here if it will 
# be used for others, which is why it is commented out here
# file.remove(list.files(path = "./",
#                        pattern = "^mark",
#                        full.names = T))

#   3.0 View a first pass of the results                                    ####
#     3.1 Compile Simulation Results Files                                  ####
# Select an existing simulation to view results for
sim_name <- select.list(list.dirs(output_folder, 
                                  recursive = FALSE,
                                  full.names = FALSE), 
                        graphics = TRUE,
                        title = "Choose Existing Simulation",
                        multiple = TRUE)

# Because testing gets run separately for model 5 and 6, some of the results 
# files might need to be compiled into one file
if (length(sim_name) == 1) {
  if (svDialogs::dlg_message(
    "Would you like to compile simulation results into one file?",
    type = "yesno")$res == "yes") {
  
    sim_files <- list.files(path = paste0(output_folder, 
                                          "/", sim_name, "/output"),
                            pattern = "sim_results.txt$")
    
    names(sim_files) <- list.files(path = paste0(output_folder, 
                                                 "/", sim_name, "/output"),
                                   pattern = "sim_results.txt$",
                                   full.names = TRUE)
    
    sim_files <- sim_files[grep("compile", sim_files, invert = TRUE)]
    
    to_compile <- svDialogs::dlg_list(choices = sim_files, 
                                      multiple = TRUE)$res
    
    sim_results <- names(sim_files)[sim_files %in% to_compile] %>%  
      lapply(read.table,
             header = TRUE) %>% 
      rbindlist(use.names = TRUE)
    
    write.table(sim_results, 
                file = paste0(output_folder, "/", sim_name, 
                              "/output/compiled_sim_results.txt"))
    
  }
}

# Use the get_results function to summarize the results for each set of testing
# parameters and create a first look graph (turned off for now because it easily 
# breaks and is not critical to the functionality)
dtaS <- list()
grid_total <- data.table(sim = sim_name)
for (i in 1:length(sim_name)) {
  dtaS[[i]] <- get_results(folder = paste0(output_folder, "/", sim_name[[i]]),
                           sing_max = 5,
                           CI =  .90,
                           returnData = 2,
                           plot = F)
  dtaS[[i]]$sim <- sim_name[[i]]
  
  # Use the population encounter histories to get the number of grid cells
  pop_files <- list.files(paste0(output_folder, "/", sim_name[[i]]), 
                          full.names = TRUE) %>% 
    grep("output", ., invert = TRUE, value = TRUE)
  
  pop_grid <- lapply(pop_files, function(x){
    read.table(x) %>% 
      nrow()}) %>% 
    unlist() %>% 
    unique()
  
  if (length(pop_grid) != 1) {
    stop(paste0(
      "Grid sizes of populations not the same, please check replicates for ", 
      sim_name[[i]]))
  }
  
  grid_total[sim == sim_name[[i]], total_cells := pop_grid]
  
}

# This shouldn't really matter but worth warning about
# Confirm that all the maps have the same number of grid cells
if (length(unique(grid_total$grid_total)) > 1) {
  warning("Check total grid values for simulations, they are not all equal.")
}

# In case more than one simulation was tested, bring all the results into one 
# table.
dtaS <- rbindlist(dtaS) %>% 
  # Add the total cell count to the table
  left_join(grid_total, 
            by = "sim")

# Add a column that will just help with some graphing and formatting
dtaS[, grid_range := ifelse(grid_min == grid_max, 
                            paste0(round(100 * (n_grid/total_cells)), "%"),
                            paste0((100 * grid_min), "% - ", (100 * grid_max), 
                                   "%"))
     ][, ":="(N_init = case_when(substring(sim, 1, 1) == "C" ~ 700,
                                 substring(sim, 1, 1) == "R" ~ 350, 
                                 substring(sim, 1, 1) == "U" ~ 100), 
              lmda = case_when(substring(sim, 2, 2) == "M" ~ 0.977,
                               substring(sim, 2, 2) == "R" ~ 0.933),
              # Set up as factors for graphing
              grid_range = factor(grid_range, 
                                   levels = c("4%", "5%", "6%", "9%", "10%",
                                              "15%", "25%", "35%", "45%",
                                              "55%", "65%", "75%", "85%", "95%",
                                              "0% - 100%", "2.5% - 100%",
                                              "5% - 100%")))]

#   4.0 Plotting                                                            ####
# Setup labels for faceting later
facet_labs_detP <- c("Low Detection Probability",
                     "High Detection Probability",
                     "100% Detection Probability")
names(facet_labs_detP) <- sort(unique(dtaS$detP))

N_labs <- paste0("Initial population: ", 
                 sort(unique(dtaS$N_init)))
names(N_labs) <- sort(unique(dtaS$N_init))

lmda_labs <- paste0("True Lambda: ", 
                    sort(unique(dtaS$lmda)))
names(lmda_labs) <- sort(unique(dtaS$lmda))

sim_dt <- dtaS[, .(N_init, sim_name = substr(sim, 1, 3), lmda, detP, n_visits)] %>% 
  unique()

sim_dt[, sim_name_org := sim_name
       ][, full := case_when(sim_name == "URD" ~ "Rare Rapid Decline", 
                             sim_name == "RRD" ~ "Common Rapid Decline", 
                             sim_name == "CRD" ~ "Abundant Rapid Decline",
                             sim_name == "UMD" ~ "Rare Moderate Decline", 
                             sim_name == "RMD" ~ "Common Moderate Decline", 
                             sim_name == "CMD" ~ "Abundant Moderate Decline")
         ][, sim_name := case_when(sim_name == "URD" ~ "Rare-50%", 
                                   sim_name == "RRD" ~ "Common-50%", 
                                   sim_name == "CRD" ~ "Abundant-50%",
                                   sim_name == "UMD" ~ "Rare-20%", 
                                   sim_name == "RMD" ~ "Common-20%", 
                                   sim_name == "CMD" ~ "Abundant-20%")]

facet_text <- sim_dt[, .(N_init, lmda, sim_name)] %>% 
  unique()

# Select where to save plots
plot_dir <- svDialogs::dlg_dir(title = "Select location to save plots")$res 

#     4.1 Spatial Variability                                               ####
# This creates that plot that Mike drew out. The effects of spatial 
# inconsistency graph with some limits so there are not a ton of lines.
spin_plot_fxn <- function(visits = 4,
                          plot_title = ""){
  ggplot(data = subset(dtaS, alt_model == 4 & 
                         n_visits == visits & 
                         detP < 1 &
                         grid_range %in% c("5%", "15%", "25%", 
                                           "35%", "45%", "55%"))) +
    geom_line(aes(x = loc_per*100,
                  y = (count/total),
                  group = interaction(n_visits, n_grid, detP, grid_range, sim), 
                  colour = grid_range, 
                  linetype = factor(detP, levels = c(0.8, 0.3))), 
              linewidth = 0.75) +
    scale_linetype_discrete(name = expression(p["sim"])) +
    scale_color_brewer(palette = "Dark2",
                       name = "Percent of Landscape \nSampled per Year",
                       breaks = c("5%", "15%", "25%", "35%", "45%", "55%")) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = plot_title,
         x = "Percent Spatial Consistency", 
         y = "Statistical Power") +
    geom_hline(aes(yintercept = 0.8)) +
    theme(# legend.position = "none", 
          # strip.text = element_text(size = 15),
          # axis.title = element_text(size = 15), 
          # axis.text = element_text(size = 12),
          legend.title = element_text(# size = 13,
                                      hjust = 0.5), 
          plot.title = element_text(# size = 20,
                                    hjust = 0.5),
          plot.subtitle = element_text(# size = 10,
                                       hjust = 0.5), 
          legend.text = element_text(# size = 12, 
                                     hjust = 0.5),
          legend.box.just = "center") +
    guides(color = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    facet_grid(N_init ~ lmda, 
               labeller = labeller(N_init = N_labs, 
                                   lmda = lmda_labs)) +
    geom_text(data = facet_text,
              fontface = "bold", 
              aes(x = 83, 
                  y = 0.05, 
                  label = sim_name))
}

# Save this graph
ggsave(filename = "Effect_Spatial_Varaibility_V4.tiff", 
       plot = spin_plot_fxn(), 
       path = plot_dir,
       width = 21.59, 
       height = 27.94,  
       units = "cm", 
       dpi = 1000)

#        4.1.1 Supplemental Plots                                           ####
sup_visits <- c(3, 5, 6)

names(sup_visits) <- c("3 Sampling Occasions per Year",
                       "5 Sampling Occasions per Year", 
                       "6 Sampling Occasions per Year")
for (i in 1:length(sup_visits)) {

  ggsave(filename =  paste0("Effect_Spatial_Variability_Sup_V", 
                            sup_visits[[i]], 
                            ".tiff"), 
         plot = spin_plot_fxn(visits = sup_visits[[i]],
                              plot_title = names(sup_visits)[[i]]), 
         path = plot_dir,
         width = 21.59, 
         height = 27.94,  
         units = "cm", 
         dpi = 1000)
}

#        4.1.2 Summary Stats for Power                                      ####
pencil_sub <- subset(dtaS, alt_model == 4 & 
                       n_visits == 4 & 
                       detP < 1 &
                       grid_range %in% c("5%", "15%", "25%", "35%", "45%",
                                         "55%"))

pencil_sub[, power := count/total]

pow_summary <- pencil_sub[, .(min = min(power),
                              max = max(power), 
                              sd = sd(power), 
                              var = var(power),
                              mean = mean(power), 
                              range = max(power) - min(power)),
                          by = c("sim", "grid_range", "detP")]

var_summary <- pow_summary[, .(min = min(var),
                               max = max(var), 
                               sd = sd(var), 
                               mean = mean(var), 
                               range = max(var) - min(var)), 
                           by = c("detP")]

sim_summary <- pow_summary[, .(min = min(range),
                               max = max(range), 
                               sd = sd(range), 
                               mean = mean(range), 
                               range = max(range) - min(range)), 
                           by = c("sim", "detP")]

#        4.1.3 Presentation Plots                                           ####
(sp_var_present_plot <- ggplot(data = subset(dtaS, alt_model == 4 & 
                       n_visits == 4 & 
                       detP == 0.3 &
                       grid_range %in% c("5%", "15%", "25%", 
                                         "35%", "45%", "55%") &
                       sim == "RRD_new_params")) +
  geom_line(aes(x = loc_per*100,
                y = (count/total),
                group = interaction(n_visits, n_grid, detP, grid_range, sim), 
                colour = grid_range), 
            linewidth = 1) +
  scale_linetype_discrete(name = expression(p["sim"])) +
  scale_color_brewer(palette = "Dark2",
                     name = "Percent of Landscape \nSampled per Year",
                     breaks = c("5%", "15%", "25%", "35%", "45%", "55%")) +
   scale_y_continuous(limits = c(0, 1), 
                      expand = c(0, 0)) +
   scale_x_continuous(limits = c(0, 100), 
                      expand = c(0, 0)) +
  labs(title = "Effects of Spatial Variability",
       x = "Percent Spatial Consistency", 
       y = "Statistical Power") +
  geom_hline(aes(yintercept = 0.8)) +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 13,
                                    hjust = 0.5), 
        plot.title = element_text(size = 20,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5), 
        legend.text = element_text(size = 12, 
                                   hjust = 0.5),
        legend.box.just = "center"))

ggsave(filename = "Effect_Spatial_Varaibility_Presentation.tiff", 
       plot = sp_var_present_plot, 
       path = plot_dir,
       width = 13.333, 
       height = 7.5,  
       units = "in", 
       dpi = 1000)

#        4.1.4 Expectation pencil plot                                      ####
# Create a graph of what we expected to see with spatial inconsistency for 
# comparison
(expectation_plot <- ggplot(data = subset(dtaS, alt_model == 4 & 
                                            n_visits == 4 & 
                                            detP == 0.3 &
                                            grid_range %in% c("5%", "15%", "25%", 
                                                              "35%", "45%", "55%") &
                                            sim == "RRD_new_params"), 
                            aes(x = loc_per*100,
                                y = (count/n_runs),
                                group = interaction(n_visits, n_grid, detP, 
                                                    grid_range))) +
   geom_abline(aes(intercept = 0.25, 
                   slope = .0080),
               linewidth = 1.25) +
   scale_color_brewer(palette = "Dark2",
                      name = "Percent of Landscape \nSampled Annually",
                      breaks = c("5%", "15%", "25%", "35%", "45%", "55%")) +
   scale_y_continuous(limits = c(0, 1), 
                      expand = c(0, 0)) +
   scale_x_continuous(limits = c(0, 100), 
                      expand = c(0, 0)) +
   labs(title = "Effects of Spatial Variability",
        x = "Percent Spatial Consistency", 
        y = "Statistical Power") +
   annotate(geom = "text",
            x = 50, 
            y = 0.5, 
            label = 'EXPECTATION', 
            color = 'white',
            angle = 45,
            fontface = 'bold',
            size = 30,
            alpha = 0.9) + 
   geom_hline(aes(yintercept = 0.8)) +
   theme(legend.title.align = 0.5, 
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5, size = 10), 
         strip.text = element_text(size = 15),
         axis.title = element_text(size = 15), 
         axis.text = element_text(size = 12), 
         legend.title = element_text(size = 13), 
         legend.text = element_text(size = 12)))

# Save this graph UPDATE EVERYTIME TO AVOID OVERWRITING!
ggsave(filename = "Effect_Spatial_Varaibility_Expect.tiff", 
       plot = expectation_plot, 
       path = plot_dir,
       width = 11.25, 
       height = 7.5,  
       units = "in", 
       dpi = 1000)

#     4.2 Effort Variability                                                ####
# Create a list of the different types of sampling to graph
sampling_sets <- list(consistent_samp = c("4%", "6%", "9%"), 
                      full_vary = c("0% - 100%"),
                      baseline = c("5% - 100%", "2.5% - 100%"))

# For presentation plots:
tpin_present_sets <- list(sampling_sets$consistent_samp,
                          c(sampling_sets$consistent_samp,
                            sampling_sets$full_vary),
                          c(sampling_sets$consistent_samp,
                            sampling_sets$full_vary,
                            sampling_sets$baseline))

# For manuscript plots:
tpin_plot_sets <- list(c(sampling_sets$consistent_samp, 
                         sampling_sets$full_vary,
                         sampling_sets$baseline))

dtaS[, alpha_val := case_when(grid_range == "4%" | grid_range == "0% - 100%" ~ "low", 
                              grid_range == "6%" | grid_range == "2.5% - 100%" ~ "med", 
                              grid_range == "9%" | grid_range == "5% - 100%" ~ "high")
     ][, alpha_val := factor(alpha_val, levels = c("low", "med", "high"))]

# This function creates a graph for each sampling set that was defined above
tpin_plot_fxn <- function(samp_set, 
                          psim = 0.8,
                          plot_title = ""){
  
  ggplot(data = subset(dtaS, 
                       detP == psim & 
                         (alt_model != 4 &  
                            grid_range %in% samp_set))) +
    geom_line(aes(x = n_visits, 
                  y = (count/total),
                  colour = factor(alpha_val), 
                  linetype = factor(alt_model)), 
              linewidth = 0.75) +
    scale_colour_brewer(name = "Median Percent of \nLandscape Sampled Annually",
                        guide = "legend", 
                        palette = "Dark2",
                        breaks = c("low", "med", "high"), 
                        labels = c("4%", "6%", "9%")) +
    scale_linetype_manual(name = "", 
                          breaks = c("6", "0"), 
                          labels = c("Variable Effort", 
                                     "Consistent Effort"), 
                          values = c(1, 2)) + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(title = plot_title,
         x = "Sampling Occasions per Year", 
         y = "Statistical Power") +
    geom_hline(aes(yintercept = 0.8)) +
    theme(# legend.position = "none",
          # strip.text = element_text(size = 15),
          # axis.title = element_text(size = 15), 
          # axis.text = element_text(size = 12), 
          # legend.title = element_text(size = 13),
          legend.title = element_text(hjust = 0.5), 
          plot.title = element_text(# size = 20,
                                    hjust = 0.5),
          plot.subtitle = element_text(# size = 10,
                                       hjust = 0.5),
          legend.text = element_text(# size = 12, 
                                     hjust = 0.5), 
          legend.key.spacing.y = unit(.1, 'cm'), 
          legend.box.just = "center") +
    facet_grid(N_init ~ lmda, 
               labeller = labeller(N_init = N_labs, 
                                   lmda = lmda_labs)) +
    geom_text(data = facet_text,
              fontface = "bold", 
              aes(x = 5.4, 
                  y = 0.05, 
                  label = sim_name)) # + 
    # guides(linetype = guide_legend(byrow = TRUE))
}

# For presentation plots:
present_names <- c("consistent_samp", "full_vary", "baseline")

# For manuscript plots:
plot_names <- "Effect_Effort_Variability_psim080"

# Save all these graphs using the function created above and the graph names in 
# the line above
for (i in 1:length(tpin_plot_sets)) {
  
  (plot <- tpin_plot_fxn(unlist(tpin_plot_sets[i])))
  
  ggsave(filename = paste0(plot_names[i], ".tiff"),
         plot = plot,
         path = plot_dir,
         width = 21.59, 
         height = 27.94, 
         units = "cm",
         dpi = 1000)
  
}

#        4.2.1 Supplemental Plots                                           ####
# For manuscript plots:
sup_plot_names <- "Effect_Effort_Variability_Sup_psim030"

# Save all these graphs using the function created above and the graph names in 
# the line above
for (i in 1:length(tpin_plot_sets)) {
  
  sup_plot <- tpin_plot_fxn(
    unlist(tpin_plot_sets[i]), 
    psim = 0.3, 
    plot_title = expression(Low~per~Occasion~Detection~Probability~(p[sim]==0.3)))
  
  ggsave(filename = paste0(sup_plot_names[i], ".tiff"),
         plot = sup_plot,
         path = plot_dir,
         width = 21.59, 
         height = 27.94, 
         units = "cm",
         dpi = 1000)
  
}

#        4.2.2 Summary Stats                                                ####
samp_set <- unlist(tpin_plot_sets[length(tpin_plot_sets)])
samp_set_dt <- subset(dtaS, 
                      detP == 0.8 & 
                        (alt_model != 4 & grid_range %in% samp_set)) %>% 
  as.data.table()

samp_set_dt[, power := count/total]

samp_set_dt[, c("n_grid", "grid_min", "grid_max", "total", "count", "n_runs", 
                "total_cells", "sim", "alt_model", "alpha_val") := NULL]

samp_set_wide <- tidyr::pivot_wider(samp_set_dt, 
                                    names_from = "grid_range",
                                    values_from = "power") %>% 
  as.data.table()

samp_set_wide[, ":="(nobasediff = `4%` - `0% - 100%`, 
                     smallbasediff = `6%` - `2.5% - 100%`, 
                     bigbasediff = `9%` - `5% - 100%`)]

samp_set_wide[, .(nobasediff_mean = mean(nobasediff), 
                  nobasediff_sd = sd(nobasediff), 
                  smallbasediff_mean = mean(smallbasediff), 
                  smallbasediff_sd = sd(smallbasediff),
                  bigbasediff_mean = mean(bigbasediff), 
                  bigbasediff_sd = sd(bigbasediff)), 
              by = c("lmda", "N_init")]

#        4.2.3 Presentation Plots                                           ####
# Create a list of the different types of sampling to graph
sampling_sets <- list(consistent_samp = c("4%"), 
                      full_vary = c("0% - 100%"),
                      base_2.5 = c("6%", "2.5% - 100%"), 
                      base_5 = c("9%", "5% - 100%"))

# For presentation plots:
tpin_present_sets <- list(sampling_sets$consistent_samp,
                          c(sampling_sets$consistent_samp,
                            sampling_sets$full_vary),
                          c(sampling_sets$consistent_samp,
                            sampling_sets$full_vary,
                            sampling_sets$base_2.5), 
                          c(sampling_sets$consistent_samp,
                            sampling_sets$full_vary,
                            sampling_sets$base_2.5, 
                            sampling_sets$base_5))

dtaS[, alpha_val := case_when(grid_range == "4%" | grid_range == "0% - 100%" ~ "low", 
                              grid_range == "6%" | grid_range == "2.5% - 100%" ~ "med", 
                              grid_range == "9%" | grid_range == "5% - 100%" ~ "high")
][, alpha_val := factor(alpha_val, levels = c("low", "med", "high"))]

# This function creates a graph for each sampling set that was defined above
tpin_present_fxn <- function(samp_set, 
                             psim = 0.8){
  
  ggplot(data = subset(dtaS, 
                       detP == psim & 
                         sim == "RRD_new_params" &
                         (alt_model != 4 &  
                            grid_range %in% samp_set))) +
    geom_line(aes(x = n_visits, 
                  y = (count/total),
                  colour = factor(alpha_val), 
                  linetype = factor(alt_model)), 
              linewidth = 0.75) +
    scale_colour_brewer(name = "Median Percent of \nLandscape Sampled Annually",
                        guide = "legend", 
                        palette = "Dark2",
                        breaks = c("low", "med", "high"), 
                        labels = c("4%", "6%", "9%")) +
    scale_linetype_manual(name = "", 
                          breaks = c("6", "0"), 
                          labels = c("Variable Effort", 
                                     "Consistent Effort"), 
                          values = c(1, 2)) + 
    scale_y_continuous(limits = c(0, 1), 
                       expand = c(0, 0.05)) +
    labs(title = "Effect of Effort Variability",
         x = "Sampling Occasions per Year", 
         y = "Statistical Power") +
    geom_hline(aes(yintercept = 0.8)) +
    theme(axis.title = element_text(size = 15), 
          axis.text = element_text(size = 12), 
          legend.title = element_text(size = 13, 
                                      hjust = 0.5), 
          plot.title = element_text(size = 20,
                                    hjust = 0.5),
          plot.subtitle = element_text(size = 10,
                                       hjust = 0.5),
          legend.text = element_text(size = 12, 
                                     hjust = 0.5), 
          legend.key.spacing.y = unit(.1, 'cm'), 
          legend.box.just = "center") # + 
  # guides(linetype = guide_legend(byrow = TRUE))
}

tpin_present_fxn(unlist(tpin_present_sets[4]))



# For presentation plots:
present_names <- c("consistent_samp", "full_vary", "base_25", "base_5")

# Save all these graphs using the function created above and the graph names in 
# the line above
for (i in 1:length(tpin_present_sets)) {
  
  (plot <- tpin_present_fxn(unlist(tpin_present_sets[i])))
  
  ggsave(filename = paste0(present_names[i], ".tiff"),
         plot = plot,
         path = plot_dir,
         width = 13.333, 
         height = 7.5,  
         units = "in", 
         dpi = 1000)
  
}


#     4.3 Graphical Abstract Plot(s)                                        ####
visits <- 4
plot_title <- ""
sp_abs_plot <- ggplot(data = subset(dtaS, 
                                    alt_model == 4 & 
                                      n_visits == visits & 
                                      detP == 0.8 &
                                      grid_range == "5%" &
                                      lmda == 0.933 &
                                      N_init == 350)) +
  geom_line(aes(x = loc_per*100,
                y = (count/total)), 
            linewidth = 1.5, 
            color = "#E97132") +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 1), 
                     minor_breaks = c(0.25, 0.50, 0.75),
                     labels = c("0", "1"), 
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 100), 
                     breaks = c(0, 100), 
                     minor_breaks = c(25, 50, 75),
                     labels = c("0%", "100%"), 
                     expand = c(0, 0)) +
  labs(title = plot_title,
       x = "Spatial Consistency", 
       y = "Statistical Power") +
  theme(axis.title = element_text(size = 30), 
        axis.title.x = element_text(vjust = 5),
        axis.title.y = element_text(vjust = -2),
        axis.text = element_text(size = 20), 
        panel.grid.minor = element_line(colour = "darkgray"),
        plot.margin = unit(c(0, 1.25, 0.25, 0.25), "cm"))

ggsave(filename = "sp_variability_grap_abs.tiff",
       plot = sp_abs_plot,
       path = plot_dir,
       width = 30, 
       height = 20, 
       units = "cm",
       dpi = 1000)

dtaS[, label := case_when(grid_range == "4%" & n_visits == 6 ~ "Consistent Effort", 
                          grid_range == "0% - 100%" & 
                            n_visits == 6 ~ "Variable Effort")]

ef_abs_plot <- ggplot(data = subset(dtaS, 
                     detP == 0.8 & 
                       (alt_model != 4 &  
                          grid_range %in% c("4%", "0% - 100%")) &
                       lmda == 0.933 &
                       N_init == 350)) +
  geom_line(aes(x = n_visits, 
                y = (count/total),
                colour = factor(alt_model)), 
            linewidth = 1.5) +
  scale_colour_manual(name = "", 
                      breaks = c("6", "0"), 
                      labels = c("Variable Effort", 
                                   "Consistent Effort"), 
                      values = c("#4E95D9", "#47D45A")) + 
  ggrepel::geom_text_repel(aes(label = label, 
                                x = n_visits, 
                                y = (count/total),
                                colour = factor(alt_model)),
                           na.rm = TRUE, 
                           nudge_y = 0.05,
                           nudge_x = -0.5,
                           fontface = "bold", 
                           size = 10, 
                           min.segment.length = Inf) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 1), 
                     minor_breaks = c(0.25, 0.50, 0.75),
                     labels = c("0", "1"), 
                     expand = c(0, 0)) +
  scale_x_discrete(limits = c(3, 4, 5, 6), 
                   labels = c("3", "", "", "6"), 
                   expand = c(0, 0)) +
  labs(x = "Sampling Occasions per Year", 
       y = "Statistical Power") +
  theme(legend.position = "none",
        axis.title = element_text(size = 30), 
        axis.title.x = element_text(vjust = 5),
        axis.title.y = element_text(vjust = -2),
        axis.text = element_text(size = 20), 
        panel.grid.major = element_line(colour = "darkgray"),
        panel.grid.minor = element_line(colour = "darkgray"),
        plot.margin = unit(c(0.5, 1.25, 0.25, 0.25), "cm")) 

ggsave(filename = "ef_variability_grap_abs.tiff",
       plot = ef_abs_plot,
       path = plot_dir,
       width = 30, 
       height = 20, 
       units = "cm",
       dpi = 1000)

#
#     4.4 Trend Estimate Box Plots                                          ####
sim_name <- select.list(list.dirs(output_folder, 
                                  recursive = FALSE,
                                  full.names = FALSE), 
                        graphics = TRUE,
                        title = "Choose Existing Simulation",
                        multiple = TRUE)

# Use the get_results function to summarize the results for each set of testing
# parameters and create a first look graph (turned off for now because it easily 
# breaks and is not critical to the functionality)
dta <- list()
grid_total <- data.table(sim = sim_name)
for (i in 1:length(sim_name)) {
  dta[[i]] <- get_results(folder = paste0(output_folder, "/", sim_name[[i]]),
                          sing_max = 5,
                           CI =  .90,
                           returnData = 1,
                           plot = F)
  dta[[i]]$sim <- sim_name[[i]]
  
  # Use the population encounter histories to get the number of grid cells
  pop_files <- list.files(paste0(output_folder, "/", sim_name[[i]]), 
                          full.names = TRUE) %>% 
    grep("output", ., invert = TRUE, value = TRUE)
  
  pop_grid <- lapply(pop_files, function(x){
    read.table(x) %>% 
      nrow()}) %>% 
    unlist() %>% 
    unique()
  
  if (length(pop_grid) != 1) {
    stop(paste0(
      "Grid sizes of populations not the same, please check replicates for ", 
      sim_name[[i]]))
  }
  
  grid_total[sim == sim_name[[i]], total_cells := pop_grid]
}

# This shouldn't really matter but worth warning about
# Confirm that all the maps have the same number of grid cells
if (length(unique(grid_total$grid_total)) > 1) {
  warning("Check total grid values for simulations, they are not all equal.")
}

dta_dt <- rbindlist(dta) %>% 
  # Add the total cell count to the table
  left_join(grid_total, 
            by = "sim")

# Add a column that will just help with some graphing and formatting
dta_dt[, grid_range := ifelse(grid_min == grid_max, 
                              paste0(round(100 * (n_grid/total_cells)), "%"),
                              paste0((100 * grid_min), "% - ", 
                                     (100 * grid_max), "%"))]

dta_dt[, grid_range := factor(grid_range, 
                              levels = c("0% - 100%", "4%", "2.5% - 100%", "6%", 
                                         "5% - 100%", "5%", "9%", "10%", "15%", 
                                         "25%", "35%", "45%", "55%", "65%", "75%", 
                                         "85%", "95%"))
       ][alt_model == 4, landscape_per := round(n_grid/total_cells, 2)
         ][alt_model == 4, loc_per_total := paste0(loc_per, "_", landscape_per)
           ][, ":="(N_init = case_when(substring(sim, 1, 1) == "C" ~ 700,
                                       substring(sim, 1, 1) == "R" ~ 350, 
                                       substring(sim, 1, 1) == "U" ~ 100), 
                    lmda = case_when(substring(sim, 2, 2) == "M" ~ 0.977,
                                     substring(sim, 2, 2) == "R" ~ 0.933),
                    alpha_val = case_when(
                      grid_range == "0% - 100%" | grid_range == "4%" ~ "Low", 
                      grid_range == "2.5% - 100%" | grid_range == "6%" ~ "Moderate", 
                      grid_range == "5% - 100%" | grid_range == "9%" ~ "High"))
             ][, alpha_val := factor(alpha_val, levels = c("Low", "Moderate", 
                                                           "High"))]

hline_data <- dta_dt[, .(N_init, lmda, trend)
                     ][, trend_avg := median(trend, na.rm = TRUE), 
                       by = c("N_init", "lmda")
                       ][, trend := NULL] %>% 
  unique()

# #        4.4.1 Spatial Variability                                          ####
# ggplot(data = subset(dta_dt, 
#                      alt_model == 4 & 
#                        detP == 0.8 & 
#                        n_visits == 4 &
#                        !is.na(trendSE) &
#                        landscape_per < 0.6)) +
#   # geom_point(aes(color = as.factor(landscape_per)),
#   #                position = position_dodge2(width = 0.75)) +
#   geom_errorbar(aes(x = as.factor(loc_per), 
#                     y = trend, 
#                     ymin = trend - (1.96 * trendSE),
#                     ymax = trend + (1.96 * trendSE), 
#                     color = as.factor(landscape_per)),
#                 width = 0.75,
#                 position = position_dodge2(width = 1)) +
#   geom_boxplot(aes(x = as.factor(loc_per), 
#                    y = trend, 
#                    fill = as.factor(landscape_per)),
#                alpha = 0.8,
#                outlier.shape = NA) +
#   # geom_jitter(color = "black",
#   #             # aes(color = as.factor(detP)),
#   #             size = 0.4) +
#   geom_hline(yintercept = 0, 
#              linetype = 2, 
#              color = "#7570B3", 
#              linewidth = 1) +
#   scale_fill_brewer(palette = "Dark2",
#                     name = "Proportion of \nLandscape Sampled") +
#   scale_color_brewer(palette = "Dark2",
#                     name = "Proportion of \nLandscape Sampled") +
#   labs(x = "Proportion of Cells that are Annually Consistent", 
#        y = "Trend Estimate", 
#        caption = expression(p[sim] == 0.8~~n[visits] == 4)) +
#   #ylim(-0.175, 0.175) +
#   facet_grid(N_init ~ lmda, 
#              scales = "free", 
#              labeller = labeller(N_init = N_labs, 
#                                  lmda = lmda_labs)) +
#   theme(legend.spacing.y = unit(0.5, "cm"), 
#         legend.text = element_text(hjust = 0.5)) +
#   guides(fill = guide_legend(byrow = TRUE)) +
#   geom_text(data = facet_text,
#             fontface = "bold", 
#             aes(x = 0.95, 
#                 y = -0.15, 
#                 label = sim_name))

#        4.4.1 Effort Variability                                           ####
trend_tpin_fxn <- function(visits = 4, 
                           psim = 0.8, 
                           plot_title = "") {
  
  plot_data <- subset(dta_dt, 
                      alt_model != 4 & 
                        detP == psim & 
                        n_visits == visits &
                        (grid_range %in% c("0% - 100%", "2.5% - 100%", 
                                           "5% - 100%", "4%", "6%", "9%")))
  
  ggplot(data = plot_data) +
    geom_errorbar(aes(x = grid_range, 
                      y = trend, 
                      ymin = trend - (1.28 * trendSE),
                      ymax = trend + (1.28 * trendSE), 
                      color = alpha_val,
                      linetype = as.factor(alt_model)),
                  width = 0.75,
                  position = position_dodge2(width = 1)) +
    geom_boxplot(aes(x = grid_range, 
                     y = trend, 
                     fill = alpha_val,
                     linetype = as.factor(alt_model)),
                 alpha = 0.8, 
                 outlier.shape = NA, 
                 fatten = 1) +
    geom_hline(yintercept = 0, 
               alpha = 0.5) +
    scale_x_discrete(breaks = c("0% - 100%", "4%", "6%", "2.5% - 100%",
                                "5% - 100%", "9%"),
                     labels = c("0.037", "0.04", "0.06", "0.064*", 
                                "0.088*", "0.09")) +
    scale_colour_brewer(name = "",
                        guide = "legend", 
                        palette = "Dark2", 
                        labels = c("4%", "6%", "9%")) +
    scale_fill_brewer(name = "",
                        guide = "legend", 
                        palette = "Dark2", 
                      labels = c("4%", "6%", "9%")) +
    scale_linetype_manual(name = "", 
                          breaks = c("6", "0"), 
                          labels = c("Variable Effort", 
                                     "Consistent Effort"), 
                          values = c(1, 2)) + 
    labs(title = plot_title, 
         x = "Median Proportion of Landscape Sampled Annually", 
         y = "Trend Estimate") +
    ylim(-0.15, 0.15) +
    facet_grid(N_init ~ lmda, 
               labeller = labeller(N_init = N_labs, 
                                   lmda = lmda_labs)) +
    theme(legend.spacing.y = unit(0.5, "cm"), 
          legend.text = element_text(hjust = 0.5), 
          legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5)) +
    guides(fill = guide_legend(byrow = TRUE)) +
    geom_text(data = facet_text,
              fontface = "bold", 
              aes(x = "5% - 100%", 
                  y = -0.14, 
                  label = sim_name)) + 
    geom_hline(data = hline_data, 
               aes(yintercept = trend_avg),
               linetype = 2, 
               linewidth = 1)
  
}

tpin_combos <- data.table(psim = rep(c(0.8, 0.3), times = 4),
                          visits = rep(3:6, each = 2))

tpin_combos[!(psim == 0.8 & visits == 4),
            file_name := paste0("Trend_Est_Effort_Variability_Sup_V", 
                                visits, 
                                "_psim", 
                                gsub("\\.", "", psim),
                                "0.tiff")
            ][psim == 0.8 & visits == 4,
              file_name := paste0("Trend_Est_Effort_Variability_V", 
                                  visits, 
                                  "_psim", 
                                  gsub("\\.", "", psim),
                                  "0.tiff")]

for (i in 1:nrow(tpin_combos)) {
  ggsave(filename = tpin_combos[i, file_name], 
         plot = trend_tpin_fxn(
           visits = tpin_combos[i, visits],
           psim = tpin_combos[i, psim], 
           plot_title = paste0(
             tpin_combos[i, visits], 
             " Sampling Occasions per Year\n", 
             ifelse(
               tpin_combos[i, psim] == 0.8, 
               "High per Occasion Detection Probability, Psim = 0.8",
               "Low per Occasion Detection Probability, Psim = 0.3"))), 
         path = plot_dir,
         width = 21.59, 
         height = 27.94, 
         units = "cm", 
         dpi = 1000)
}

# CI Interval Stats - no baseline
CI_data <- subset(dta_dt, 
                  detP < 1.0 &
                    # n_visits == 4 &
                    # detP == 0.8 &
                    alt_model != 4 &
                    (grid_range %in% c("0% - 100%", "2.5% - 100%", 
                                       "5% - 100%", "4%", "6%", "9%"))
                  )[, ':='(upper = trend + (1.28 * trendSE), 
                           lower = trend - (1.28 * trendSE), 
                           median_grid = case_when(grid_range == "0% - 100%" ~ "4%",
                                                   grid_range == "2.5% - 100%" ~ "6%",
                                                   grid_range == "5% - 100%" ~ "9%",
                                                   grid_range == "4%" ~ "4%",
                                                   grid_range == "6%" ~ "6%",
                                                   grid_range == "9%" ~ "9%"))
                    ][, ci_width := upper - lower]

CI_sum <- CI_data[, .(ci_width_mean = mean(ci_width, na.rm = TRUE), 
                      ci_width_se = sd(ci_width, na.rm = TRUE), 
                      ci_width_min = min(ci_width, na.rm = TRUE), 
                      ci_width_max = max(ci_width, na.rm = TRUE),
                      total_counted = sum(!is.na(count))), 
                      by = .(detP, 
                             lmda, N_init,
                             n_visits,  
                             alt_model,
                             median_grid)]

CI_sum[, .(ci_diff = ci_width_mean[[1]] - ci_width_mean[[2]], 
           smaller_ci = ifelse(ci_width_mean[[1]] < ci_width_mean[[2]], 
                               "constant sampling", 
                               "variable sampling")), 
       by = .(detP, n_visits, N_init, lmda, median_grid)]

t.test(ci_width_mean ~ alt_model, 
       data = CI_sum, 
       paired = TRUE)

CI_sum_manu <- CI_sum[, .(detP, lmda, N_init, n_visits, alt_model, median_grid, 
                          ci_width_mean, ci_width_se)
                      ][, sampling := ifelse(alt_model == 0, 
                                             "Consistent", 
                                             "Variable")
                        ][, alt_model := NULL]

setcolorder(CI_sum_manu, 
            c("sampling", "median_grid", "detP", "lmda", "N_init", "n_visits", 
              "ci_width_mean", "ci_width_se"))

setorder(CI_sum_manu, 
         median_grid, sampling, detP, lmda, N_init, n_visits)

setnames(CI_sum_manu, 
         c("sampling", "median_grid", "detP", "lmda", "N_init", "n_visits", 
           "ci_width_mean", "ci_width_se"),
         c("Sampling Strategy", "Median Percent Landscape Sampled", 
           "P_sim", "Lambda", "N_0", "Sampling Occasions per Year", 
           "Mean 90% Confidence Interval Width", 
           "SD of 90% Confidence Interval Width"))

fwrite(CI_sum_manu,
       paste0(plot_dir, "/trend_conf_int_summary.csv"))

#        4.4.3 Annual Psi Estimates                                         ####
# Need to melt the data
sp_labs <- paste0(sort(unique(dta_dt$loc_per))*100, "% Spatially Consistent")
names(sp_labs) <- sort(unique(dta_dt$loc_per))

land_labs <- paste0(
  round(sort(unique(dta_dt$landscape_per))*unique(dta_dt$total_cells)), " Cells")
names(land_labs) <- sort(unique(dta_dt$landscape_per))

dta_dt_long <- tidyr::pivot_longer(dta_dt, 
                                   cols = paste0("X", 1:11), 
                                   names_to = "year", 
                                   values_to = "psi_est",
                                   names_prefix = "X") %>% 
  mutate(year = as.numeric(year), 
         psi_est = ifelse(singular > 5, 
                          NA, 
                          psi_est))

setDT(dta_dt_long)

# Also get true psi for each year/population
n_dt_lst <- lapply(sim_name, function(x) {
  dir <- list.files(x,
                    path = output_folder, 
                    include.dirs = TRUE, 
                    full.names = TRUE)
  
  list.files(path = paste0(dir, "/output"), 
             pattern = "N_final.txt", 
             recursive = FALSE, 
             full.names = TRUE) %>% 
    read.table(header = TRUE)
})

names(n_dt_lst) <- basename(sim_name)

full_n_dt <- rbindlist(n_dt_lst,
                       idcol = "sim")

full_n_dt[, psi_est := mean(truePsi_MaxVisits, na.rm = TRUE), 
          by = c("sim", "Year")]

# Set up a table with the different loc_per, and landscape_per values for each 
# simulation so that they can be graphed over the different plots. These values
# are subset for those that are in the final plots.
loc_land_dt <- dta_dt_long[landscape_per < 0.6 &
                             loc_per %in% c(0.05, 
                                            0.25, 
                                            0.55,
                                            0.95), 
                           .(loc_per, landscape_per, sim)] %>% 
  unique()

full_n_dt <- full_n_dt[loc_land_dt, on = "sim", allow.cartesian = TRUE]

occ_tpin_plot_fxn <- function(psim = 0.8,
                              visits = 4, 
                              sim_fil = "RRD_new_params", 
                              title = plot_title, 
                              subtitle = plot_subtitle) {
  
  plot_data <- rbind(dta_dt_long, 
                       full_n_dt[, .(sim,
                                     psi_est, 
                                     year = Year + 1,
                                     truth = "True_value", 
                                     alt_model = 4, 
                                     loc_per, 
                                     landscape_per, 
                                     detP = psim, 
                                     n_visits = visits)], 
                       fill = TRUE) %>% 
    unique()
  
  plot_data[is.na(truth), truth := "Est_Value"]
  
  plot_data <- subset(plot_data, 
                      alt_model == 4 & 
                        detP == psim & 
                        n_visits == visits &
                        landscape_per < 0.6 &
                        loc_per %in% c(0.05, 
                                       0.25, 
                                       0.55,
                                       0.95) &
                        sim == sim_fil)
  
  ggplot() +
    # geom_point(#aes(color = as.factor(landscape_per)), 
    #            position = position_dodge2(width = 0.75), 
    #            size = 0.5) +
    geom_line(data = plot_data[truth == "Est_Value", ], 
              aes(x = year,
                  y = psi_est,
                  color = as.factor(rn)),
              alpha = 0.3#,
              #outlier.shape = NA
    ) +
    geom_line(data = plot_data[truth == "True_value", ], 
              aes(x = year,
                  y = psi_est), 
              linewidth = 0.5) +
    # scale_fill_brewer(palette = "Dark2",
    #                   name = "") +
    # scale_color_brewer(palette = "Dark2",
    #                    name = "") +
    scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
    ylim(0, 1) +
    labs(title = title,
         subtitle = subtitle,
         x = "Year",
         y = "Estimated Occupancy") +
    facet_grid(landscape_per ~ loc_per,
               #scales = "free",
               labeller = labeller(loc_per = sp_labs, 
                                   landscape_per = land_labs)) +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

psi_est_combos <- sim_dt[detP < 1, ]

psi_est_combos[, ":="(
  sim_name = paste0(sim_name, "_new_params"),
  file_name = paste0("Psi_Est_Spatial_Vari_", 
                     sim_name, 
                     "_V", n_visits, 
                     "_psim", gsub("\\.", "", detP), 
                     "0.tiff"), 
  plot_title = paste0(sim_name, ": ", full), 
  plot_subtitle = paste0(
    n_visits, 
    " Sampling Occasions per Year; ", 
    ifelse(detP == 0.8, 
           "High per Occasion Detection Probability, Psim = 0.8",
           "Low per Occasion Detection Probability, Psim = 0.3")))]

if (!dir.exists(paste0(plot_dir, "/Psi_Est_Plots"))) {
  dir.create(paste0(plot_dir, "/Psi_Est_Plots"))
}
  
for (i in 1:nrow(psi_est_combos)) {
  ggsave(filename = psi_est_combos[i, file_name], 
         plot = occ_tpin_plot_fxn(
           psim = psi_est_combos[i, detP],
           visits = psi_est_combos[i, n_visits],
           sim_fil = paste0(psi_est_combos[i, sim_name_org], "_new_params"), 
           title = psi_est_combos[i, plot_title],
           subtitle = psi_est_combos[i, plot_subtitle]), 
         path = paste0(plot_dir, "/Psi_Est_Plots"),
         width = 21.59, 
         height = 27.94, 
         units = "cm", 
         dpi = 1000)
}

psi_stats <- as.data.table(dta_dt_long)[alt_model == 4 & 
                                          detP == 0.8 & 
                                          n_visits == 4 &
                                          landscape_per < 0.6 &
                                          loc_per %in% c(0.05, 0.25, 0.55, 0.95), 
                                        .(mean = mean(psi_est),
                                          sd = sd(psi_est)), 
                                        by = c("n_grid", "loc_per", "sim")]

#     4.5 Calculate average cells sampled                                   ####
sim_name <- select.list(list.dirs(output_folder, 
                                  recursive = FALSE,
                                  full.names = FALSE), 
                        graphics = TRUE,
                        title = "Choose Existing Simulation",
                        multiple = TRUE)

samp_mat_folders <- list.dirs(paste0(output_folder, "/", sim_name, "/output"),
                              full.names = TRUE,
                              recursive = FALSE) %>% 
  grep("variable_sampling_matrices$", 
       ., 
       value = TRUE) %>% 
  grep("mod04", ., invert = TRUE, value = TRUE)

# Create a list to hold the averages
cell_avgs <- NULL
cell_meds <- NULL
avgs_lst <- list()
meds_lst <- list()

for (i in 1:length(samp_mat_folders)) {
  
  run_folders <- list.dirs(samp_mat_folders[[i]], recursive = FALSE)
  
  # Set up matrix with columns for different visit totals and rows for each
  # replicate
  mean_mat <- med_mat <- matrix(NA, 
                                nrow = length(run_folders), 
                                ncol = length(list.files(run_folders[[1]], 
                                                         full.names = TRUE)))
  
  for (j in 1:length(run_folders)) {
    
    mat_files <- list.files(run_folders[[j]], 
                            full.names = TRUE)
    
    if (length(mat_files) != 12) {
      stop("There are not 12 sampling matrix files")
    }
    
    for (k in 1:length(mat_files)) {
      
      samp_mat <- read.table(mat_files[[k]])
      
      ann_cells <-  apply(samp_mat, 2, sum) 
      
      ann_percent <- ann_cells/length(samp_mat[, 1]) 
      
      mean_mat[j, k] <- mean(ann_percent) 
      med_mat[j, k] <- median(ann_percent) 
      
    }
    
  }
  
  # Then add the mean of all runs to a data frame with the name of the upper folder
  cell_avgs[[i]] <- apply(mean_mat, 2, mean)
  cell_meds[[i]] <- apply(med_mat, 2, median)
  avgs_lst[[i]] <- mean_mat
  meds_lst[[i]] <- med_mat
  names(cell_avgs)[[i]] <- names(cell_meds)[[i]] <- names(avgs_lst)[[i]] <- names(meds_lst)[[i]] <- basename(samp_mat_folders[[i]]) %>% 
    gsub("(n100_v3456_d3080100_15_30_)(0|2\\$5|5)(_variable_sampling_matrices)", "\\2", .) %>% 
    gsub("\\$", ".", .) %>% 
    as.numeric()
  
}

range_means <- lapply(cell_avgs, mean)
names(range_means) <- names(cell_avgs) 

range_meds <- lapply(cell_meds, median)
names(range_meds) <- names(cell_meds) 

range_mean_sd <- lapply(cell_avgs, sd)
names(range_mean_sd) <- names(cell_avgs) 

range_med_sd <- lapply(cell_meds, sd)
names(range_med_sd) <- names(cell_meds) 

(range_df <- data.frame(baseline = names(range_means), 
                       mean_cell_per = round(unlist(range_means) * 100, 2),
                       med_cell_per = round(unlist(range_meds) * 100, 2), 
                       mean_sd_cell_per = round(unlist(range_mean_sd) * 100, 2), 
                       med_sd_cell_per = round(unlist(range_med_sd) * 100, 2), 
                       sim = gsub("(.*3.Outputs/)(.*)(/output.*)", "\\2", samp_mat_folders)))

overall_stats <- data.table(baseline = c(0, 2.5, 5))
overall_stats[baseline == 0, ":="(avg = avgs_lst[which(names(avgs_lst) == "0")] %>% 
                                    do.call(cbind, .) %>% 
                                    mean(), 
                                  avg_sd = avgs_lst[which(names(avgs_lst) == "0")] %>% 
                                    do.call(cbind, .) %>% 
                                    sd(), 
                                  med = meds_lst[which(names(meds_lst) == "0")] %>% 
                                    do.call(cbind, .) %>% 
                                    median(), 
                                  med_sd = meds_lst[which(names(meds_lst) == "0")] %>% 
                                    do.call(cbind, .) %>% 
                                    sd())
              ][baseline == 2.5, ":="(avg = avgs_lst[which(names(avgs_lst) == "2.5")] %>% 
                                      do.call(cbind, .) %>% 
                                      mean(), 
                                    avg_sd = avgs_lst[which(names(avgs_lst) == "2.5")] %>% 
                                      do.call(cbind, .) %>% 
                                      sd(), 
                                    med = meds_lst[which(names(meds_lst) == "2.5")] %>% 
                                      do.call(cbind, .) %>% 
                                      median(), 
                                    med_sd = meds_lst[which(names(meds_lst) == "2.5")] %>% 
                                      do.call(cbind, .) %>% 
                                      sd())
              ][baseline == 5, ":="(avg = avgs_lst[which(names(avgs_lst) == "5")] %>% 
                                        do.call(cbind, .) %>% 
                                        mean(), 
                                      avg_sd = avgs_lst[which(names(avgs_lst) == "5")] %>% 
                                        do.call(cbind, .) %>% 
                                        sd(), 
                                      med = meds_lst[which(names(meds_lst) == "5")] %>% 
                                        do.call(cbind, .) %>% 
                                        median(), 
                                      med_sd = meds_lst[which(names(meds_lst) == "5")] %>% 
                                        do.call(cbind, .) %>% 
                                        sd())
                ]# [, ":="(avg = round(avg * 100, 2), 
                         # avg_sd = round(avg_sd * 100, 2), 
                         # med = round(med * 100, 2), 
                         # med_sd = round(med_sd * 100, 2))]

#     4.6 Calculate detection probability                                   ####
sim_name <- list.dirs(output_folder, 
                      recursive = FALSE,
                      full.names = TRUE) %>% 
  grep("XX", 
       ., 
       value = TRUE, 
       invert = TRUE) %>% 
  select.list(multiple = TRUE, 
              title = "Select simulations", 
              graphics = TRUE)

sim_results_lst <- lapply(sim_name, function(x) {
  list.files(path = paste0(x, "/output"), 
             full.names = TRUE, 
             recursive = FALSE) %>% 
  grep("/compiled_sim_results", 
       .,
       value = TRUE) %>% 
  read.table(header = TRUE)
  })

names(sim_results_lst) <- basename(sim_name)
sim_results <- rbindlist(sim_results_lst, 
                         idcol = "sim")[, rn := as.numeric(substring(rn, 
                                                                     nchar(rn) - 3, 
                                                                     nchar(rn)))]

sim_results[, .(mean_p = mean(p_est), 
                sd_p = sd(p_est)), 
            by = c("sim", "detP")]

#     4.7 N-Final Asy psi                                                   ####
current_sims <- list.dirs(output_folder, 
          recursive = FALSE,
          full.names = TRUE) %>% 
  grep("XX", 
       ., 
       value = TRUE, 
       invert = TRUE) %>% 
  select.list(multiple = TRUE, 
              title = "Select simulations", 
              graphics = TRUE)

n_dt_lst <- lapply(current_sims, function(x) {
  list.files(path = paste0(x, "/output"), 
             pattern = "N_final.txt", 
             recursive = FALSE, 
             full.names = TRUE) %>% 
    read.table(header = TRUE)
})
names(n_dt_lst) <- basename(current_sims)

full_n_dt <- rbindlist(n_dt_lst,
                       idcol = "sim")

full_n_dt[Year == 0 | Year == 10,
          .(mean_psi_pix = mean(truePsi_Pixel), 
            sd_psi_pix = sd(truePsi_Pixel),
            mean_psi_cmv = mean(truePsi_MaxVisits), 
            sd_psi_cmv = sd(truePsi_MaxVisits),
            mean_psi_asy = mean(truePsi_Asymptotic), 
            sd_psi_asy = sd(truePsi_Asymptotic)), 
          by = c("sim", "Year")]

full_n_dt[, .(
  delta_psi_pix = (
    truePsi_Pixel[Year == 0] - truePsi_Pixel[Year == 10])/truePsi_Pixel[Year == 0],
  delta_psi_cmv = (
    truePsi_MaxVisits[Year == 0] - truePsi_MaxVisits[Year == 10])/truePsi_MaxVisits[Year == 0],
  delta_psi_asy = (
    truePsi_Asymptotic[Year == 0] - truePsi_Asymptotic[Year == 10])/truePsi_Asymptotic[Year == 0]), 
  by = c("sim", "rn")
          ][, .(mean_delta_psi_pix = mean(delta_psi_pix), 
                sd_delta_psi_pix = sd(delta_psi_pix),
                mean_delta_psi_cmv = mean(delta_psi_cmv), 
                sd_delta_psi_cmv = sd(delta_psi_cmv),
                mean_delta_psi_asy = mean(delta_psi_asy), 
                sd_delta_psi_asy = sd(delta_psi_asy)), 
            by = sim]

#   5.0 Testing with Unmarked                                               ####
library(unmarked)
library(progress)
library(pbapply)

enc_hist_files <- list.dirs(path = paste0(output_folder, "/URD_Extreme"), 
                            recursive = TRUE, 
                            full.names = TRUE) %>% 
  grep("encounter_histories", 
       .,
       value = TRUE) %>% 
  lapply(list.files, 
         recursive = TRUE, 
         full.names = TRUE) %>% 
  unlist()

enc_hist_lst <- lapply(enc_hist_files, read.table)
names(enc_hist_lst) <- gsub(paste0(output_folder, "/URD_Extreme/output/"), 
                            "", 
                            enc_hist_files)

enc_dt_lst <- list()
# pb <- progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
#                        total = length(enc_hist_lst),
#                        clear = FALSE,
#                        width = 60)

enc_fix <- function(enc_hist) {
  enc_dt <- matrix(nrow = nrow(enc_hist), 
                   ncol = nchar(enc_hist[1, ]))
  
  for (j in 1:nrow(enc_hist)) {
    
    cell_enc <- strsplit(enc_hist[j, ], 
                            split = "") %>% 
      unlist()
    
    cell_enc[cell_enc == "."] <- NA
    
    cell_enc <- as.numeric(cell_enc)
    
    enc_dt[j, ] <- cell_enc
  }
  
  return(enc_dt)
 
}

enc_dt_lst <- pblapply(enc_hist_lst, enc_fix)

occ_dt_fxn <- function(enc_dt) {

  year <- matrix(formatC(1:11, digits = 1, format = "d", flag = 0), 
                 nrow = nrow(enc_dt), 
                 ncol = 11, 
                 byrow = TRUE)

  umark_frame <- unmarkedMultFrame(y = enc_dt, 
                                   yearlySiteCovs = list(year = year), 
                                   numPrimary = 11)
  
  mod <- colext(psiformula = ~1, 
                gammaformula = ~ year-1, 
                epsilonformula = ~ year-1, 
                pformula = ~ year-1, 
                data = umark_frame)
  
  occ_dt <- data.table(projected = as.data.table(mod@projected.mean)[2, ], 
                       smoothed = as.data.table(mod@smoothed.mean)[2, ], 
                       year = 1:11)
  #occ_dt[, file_name := names(enc_dt_lst)[[1]]]
  
  return(occ_dt)
}

occ_dt_lst <- pbapply::pblapply(
  enc_dt_lst[grep("sp_percent_05/encounter_histories/100detP_366|sp_percent_100/encounter_histories/100detP_2318", 
                  names(enc_dt_lst))], 
  occ_dt_fxn)

unmark_results_dt <- rbindlist(occ_dt_lst, 
                               idcol = "file")

unmark_results_dt[, ":="(n_grid = as.numeric(gsub("(.*detP_)(\\d*)(cells_.*)", 
                                                  "\\2", 
                                                  file)), 
                         n_visits = as.numeric(gsub("(.*cells_)(\\d*)(visits.*)", 
                                                    "\\2", 
                                                    file)), 
                         detP = as.numeric(gsub("(.*histories/)(\\d*)(detP_.*)", 
                                                "\\2", 
                                                file))/100,
                         alt_model = 4, 
                         loc_per = as.numeric(gsub("(.*sp_percent_)(\\d*)(/encounter_.*)", 
                                                   "\\2", 
                                                   file))/100,
                         rn = gsub("(.*rn_)(\\d*)(/sp_percent_.*)", 
                                   "URD_Extreme_\\2", 
                                   file))
                  ][, file := NULL]

dta <- get_results(folder = paste0(output_folder, "/URD_Extreme"),
                   sing_max = 5,
                   CI =  .90,
                   returnData = 1,
                   plot = F) %>% 
  tidyr::pivot_longer(cols = paste0("X", 1:11), 
                      names_to = "year", 
                      values_to = "psi_est_PM",
                      names_prefix = "X") %>% 
  mutate(psi_est_PM = ifelse(singular > 5, 
                          NA, 
                          psi_est_PM), 
         year = as.numeric(year))
         
joined <- inner_join(dta, 
                     unmark_results_dt, 
                     by = join_by(n_grid, n_visits, detP, alt_model, loc_per,
                                  rn, year)) %>% 
  mutate(diff_pro = abs(psi_est_PM - projected), 
         diff_smo = abs(psi_est_PM - smoothed)) %>% 
  as.data.table()

summary(joined[loc_per == 1, ]$diff_pro)
summary(joined$diff_smo)

fac_labs <- c("5% Sp. Consistency, 15% of Landscape",
              "100% Sp. Consistency, 95% of Landscape")
names(fac_labs) <- c(0.05, 1)

ggplot(joined) +
 aes(x = diff_pro) +
 geom_histogram(bins = 50L, fill = "#112446") +
  xlim(0, 0.03) +
 labs(x = "|Program Mark estimate - Unmarked estimate|") +
 facet_wrap(~ loc_per, 
            labeller = labeller(loc_per = fac_labs))





# mod@smoothed.mean
# mod <- nonparboot(mod, B = 10)#way to few on B

# for (i in 1:length(enc_hist_lst)) {
#   pb$tick()
#   enc_dt_lst[[i]] <- data.table()
#   
#   for (j in 1:nrow(enc_hist_lst[[i]])) {
#     enc_dt_lst[[i]][j, ] <- strsplit(enc_hist_lst[[i]][j, ], 
#                                      split = "") %>% 
#       unlist()
#   }
# }

#   6.0 Plots with mod0 vs mod 4                                            ####
ggplot(dtaS[loc_per == 1 & 
              (alt_model == 0 | alt_model == 4) &
              detP == 0.3 &
              n_visits == 4, ], 
       aes(x = n_grid, 
           y = count/total, 
           color = factor(alt_model),
           #linetype = factor(detP),
           group = alt_model)) +
  geom_line() +
  facet_grid(N_init ~ lmda, 
             labeller = labeller(N_init = N_labs, 
                                 lmda = lmda_labs)) +
  labs(subtitle = "psim = 0.3",
       y = "power",
       x = "number of grid cells")

ggplot(data = subset(dta_dt, 
                     (alt_model == 4 | alt_model == 0) & 
                       detP == 0.8 & 
                       n_visits == 4 &
                       loc_per == 1), 
       aes(x = as.factor(n_grid), 
           y = trend, 
           fill = as.factor(alt_model)
       )) +
  # geom_point(aes(color = as.factor(landscape_per)),
  #                position = position_dodge2(width = 0.75)) +
  geom_errorbar(aes(ymin = trend - (1.96 * trendSE),
                    ymax = trend + (1.96 * trendSE), 
                    color = as.factor(alt_model)),
                width = 0.75,
                position = position_dodge2(width = 1)) +
  geom_boxplot(alpha = 0.8,
               outlier.shape = NA) +
  # geom_jitter(color = "black",
  #             # aes(color = as.factor(detP)),
  #             size = 0.4) +
  geom_hline(yintercept = 0, 
             linetype = 2, 
             color = "#7570B3", 
             linewidth = 1) +
  scale_fill_brewer(palette = "Dark2",
                    name = "alt_model") +
  scale_color_brewer(palette = "Dark2",
                     name = "alt_model") +
  labs(x = "Grid Cells sampled", 
       y = "Trend Estimate", 
       caption = expression(p[sim] == 0.8~~n[visits] == 4)) +
  ylim(-0.2, 0.2) +
  facet_grid(N_init ~ lmda, 
             #scales = "free", 
             labeller = labeller(N_init = N_labs, 
                                 lmda = lmda_labs)) +
  theme(legend.spacing.y = unit(0.5, "cm"), 
        legend.text.align = 0.5) +
  guides(fill = guide_legend(byrow = TRUE))

ggplot(data = subset(dta_dt_long, 
                     (alt_model == 4 | alt_model == 0) & 
                       detP == 0.8 & 
                       n_visits == 4 &
                       n_grid < 1342 &
                       loc_per == 1 &
                       sim == "RRD_low_samp_cutoff")#, 
       # aes(x = year, 
       #     y = psi_est, 
       #     color = as.factor(rn))
) +
  # geom_point(#aes(color = as.factor(landscape_per)), 
  #            position = position_dodge2(width = 0.75), 
  #            size = 0.5) +
  geom_line(aes(x = year, 
                y = psi_est, 
                color = as.factor(rn)), alpha = 0.3#,
            #outlier.shape = NA
  ) +
  # scale_fill_brewer(palette = "Dark2",
  #                   name = "") +
  # scale_color_brewer(palette = "Dark2",
  #                    name = "") +
  #ylim(0, 1) +
  labs(x = "Year",
       y = "Estimated Occupancy",
       caption = expression(p[sim] == 0.8~~n[visits] == 4~~N == 350~~lambda == 0.933)) +
  facet_grid(n_grid ~ alt_model,
             scales = "free",
             labeller = labeller(loc_per = sp_labs, 
                                 landscape_per = land_labs)) +
  theme(legend.position = "none")
################################################################################
samp_mats <- list.files(
  pattern = "80detP_610cells_4visits.txt", 
  path = paste0(
    output_folder, "/RRD_low_samp_cutoff/output/nrange_v3456_d3080100_mod04_variable_sampling_matrices"),
  recursive = TRUE, 
  full.names = TRUE) %>% 
  grep("sp_percent_95/", ., value = TRUE) %>% 
  lapply(read.table)
o



combined_mats <- samp_mats %>% 
  lapply(function(x){
    apply(x, 1, sum) %>% 
      as.matrix()
  }) %>% 
  do.call(cbind, .)

combined_mats[combined_mats == 11] <- NA

apply(combined_mats, 2, mean, na.rm = TRUE)



#   [Notes]                                                                 ####
# lol fisher params:
# lmda = 0.933,                # Pop growth rate
# n_yrs = 1,                   # Years to simulate
# MFratio = c(0.5, 0.5),       # Ratio of individual types (can have any number of types)
# buffer = c(4, 5),            # Distance between activity centers (km)
# moveDist = c(4.51, 5.64),    # Movement radius (km)
# moveDistQ = c(0.8, 0.8),     # Proportion of movements in radius
# maxDistQ = c(0.9, 0.9),      # Max proportion of movements to allow (1 = include all)
# grid_size = 25,              # Grid cell size (km2)
# habitat.cutoff = 0.3,        # Minimum habitat value for activity centers
# sample.cutoff = 0.2,         # Proportion of cell in habitat
# n_visits = 6                 # Maximum visits per year

# need to understand the difference in the detection probability thing
# understand if FPC is needed and what that really is

# project == population estimate
# smoothed == estimates for the sites that are sampled only

# JAGS model
# what distribution to use for theta priors, DOP uses dnorm(0, 0.1) or unif(-10, 10)
#       autoocc found online uses dlogis(0, 1)

# N_final is not getting saved right, saving headers in quotes and for every run

# Scrap Code                                                                ####
#   0. Raster Adjustments 
# pepe_lolo_hab <- rast("./1.Data/4.Use_Rasters/lolo_fisher_hab2.tif")
# pepe_lolo_hab[is.na(pepe_lolo_hab)] <- 0
# names(pepe_lolo_hab) <- "z"
# 
# null_hab <- copy(pepe_lolo_hab)
# null_hab[null_hab > 0] <- 1
# 
# null_ext <- ext(0, 50000, 0, 200000) # 50kmx200km
# null_sq <- rast(null_ext)
# res(null_sq) <- 10
# crs(null_sq) <- "epsg:32610"
# null_sq <- project(null_sq, "epsg:4269")
# values(null_sq) <- 1
# writeRaster(null_sq, "./1.Data/4.Use_Rasters/null_square_50x200.tif")
#
# Setting up data for UMBS stacked model in 03-02_power_run_analysis
# # Set up a stacked model for UBMS, need to rearrange data using this function
# wide_to_stacked <- function(input_df, nyears, surveys_per_year, grid_lst){
#   inds <- split(1:(nyears*surveys_per_year), rep(1:nyears, each = surveys_per_year))
#   split_df <- lapply(1:nyears, function(i){
#     out <- input_df[, inds[[i]]]
#     out$site <- grid_lst
#     out$year <- i
#     names(out)[1:surveys_per_year] <- paste0("obs", 1:surveys_per_year)
#     out
#   })
#   stack_df <- do.call("rbind", split_df)
#   stack_df$site <- as.factor(stack_df$site)
#   stack_df$year <- as.factor(stack_df$year)
#   stack_df
# }
#
# # definitely have this version of the data somewhere earlier than this
# y_stack <- wide_to_stacked(nyears = n_yrs,
#                            surveys_per_year = n_visit,
#                            input_df = unmark_data[, 2:ncol(unmark_data)],
#                            grid_lst = grid_ids)
# for (i in 1:ncol(y_stack)) {
#   y_stack[[i]] <- as.numeric(y_stack[[i]])
# }
#
# # Include just site as a site covariate
# umf_stack <- unmarkedFrameOccu(y = y_stack[1:n_visit],
#                                siteCovs = y_stack[c('site', 'year')])

# #y_stack[, "year"] <- as.factor(y_stack[, "year"])
#
# # y_split <- split.data.frame(y_stack[, 1:n_visit], y_stack$year) %>%
# #   as.array()
#
# data_lst <- list(y = y_array,
#                  nsites = length(grid_ids),
#                  nyrs = n_yrs,
#                  nvisits = n_visit)
#
# # character columns must be factors
# m1 <- rjags::jags.model(file = "./2.Code/3.JAGS_Models/basic_autolog_temporal.txt",
#                         data = data_lst,
#                         n.chains = 4,
#                         n.adapt = 1000)
#
#
# # currently in y[site, visit, year]
# # needs to be y[site, year, visit]
#
#
#
# lapply(m1, function(x){
#   split(x, f = x$site)
# })
# split(m1$`1`, f = m1$`1`$site)
# extra_occ <- full_join(predicted_occ, predicted_stack, by = "year")
#
#
#
# library(autoOcc)
# data("opossum_det_hist")
# # reducing sample size a bit to speed up example run
# opossum_det_hist <- split(
#   opossum_det_hist,
#   factor(
#     opossum_det_hist$Season,
#     levels = unique(opossum_det_hist$Season)
#   )
# )
# opossum_det_hist <- lapply(
#   opossum_det_hist,
#   function(x) head(x, 25)
# )
# opossum_det_hist <- do.call(
#   "rbind.data.frame",
#   opossum_det_hist
# )
#
# # create y array
# opossum_y <- format_y(
#   x = opossum_det_hist,
#   site_column = "Site",
#   time_column = "Season",
#   history_columns = "^Week", # regex for starts with Week
#   report = FALSE # defaults to TRUE, turned off for example
# )
#
# m2 <- auto_occ(formula = ~1 ~1,
#                y = opossum_y)


params

