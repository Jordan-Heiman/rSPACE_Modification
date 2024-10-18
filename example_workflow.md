
# rSPACE Modification for Simulated Occupancy Sampling

## Example Workflow

[rSPACE original creator: Dr. Martha
Ellis](https://github.com/mmellis/rSPACE) Script modifications by:
Jordan Heiman Last updated: 2024-10-18

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
analysis. As well as sourcing on file with c++ functions that are used
for efficiency

``` r
list.files("2.Code/1.Functions/",
           pattern = ".R", 
           full.names = TRUE, 
           recursive = FALSE, 
           include.dirs = FALSE) %>% 
  lapply(., source) 
```

    ## [[1]]
    ## [[1]]$value
    ## [[1]]$value[[1]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[2]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[3]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[4]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[5]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[6]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[7]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[8]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[9]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[10]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## [[1]]$value[[11]]
    ##  [1] "data.table" "purrr"      "dplyr"      "plyr"       "progress"  
    ##  [6] "terra"      "magrittr"   "RMark"      "ggplot2"    "tcltk2"    
    ## [11] "tcltk"      "raster"     "sp"         "stats"      "graphics"  
    ## [16] "grDevices"  "datasets"   "utils"      "methods"    "base"      
    ## 
    ## 
    ## [[1]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[2]]
    ## [[2]]$value
    ## function (Parameters = NULL) 
    ## {
    ##     tt <- tktoplevel()
    ##     tkwm.title(tt, "Parameters")
    ##     done <- tclVar(0)
    ##     if (is.null(Parameters)) {
    ##         Parameters = list(N = 50, lmda = 0.933, n_yrs = 10, MFratio = c(0.6, 
    ##             0.4), buffer = c(16, 25), moveDist = c(8.5, 12.5), 
    ##             moveDistQ = c(0.9, 0.7), maxDistQ = c(0.95, 0.95), 
    ##             grid_size = 100, habitat.cutoff = 1, sample.cutoff = 0.5, 
    ##             n_visits = 6)
    ##     }
    ##     tkgrid(tklabel(tt, text = " "))
    ##     tkgrid(tklabel(tt, text = "-------Population simulation-------"))
    ##     N.Val <- tclVar(Parameters$N)
    ##     enterN <- tk2entry(tt, textvariable = N.Val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Initial population size? "), enterN)
    ##     lmda.val <- tclVar(Parameters$lmda)
    ##     enterL <- tk2entry(tt, textvariable = lmda.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Population growth rate? "), enterL)
    ##     yrs.val <- tclVar(Parameters$n_yrs)
    ##     enter.yrs <- tk2entry(tt, textvariable = yrs.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Number of years? "), enter.yrs)
    ##     grps.val <- tclVar(length(Parameters$MFratio))
    ##     enterG <- tk2entry(tt, textvariable = grps.val, width = 2)
    ##     tkgrid(tklabel(tt, text = "Number of individual types? "), 
    ##         enterG)
    ##     MF.val <- tclVar(Parameters$MFratio)
    ##     enterMF <- tk2entry(tt, textvariable = MF.val, width = 15)
    ##     tkgrid(tklabel(tt, text = "Proportion of population by type? "), 
    ##         enterMF)
    ##     tkgrid(tklabel(tt, text = " "))
    ##     tkgrid(tklabel(tt, text = "-----Movement parameters-----"))
    ##     buff.val <- tclVar(Parameters$buffer)
    ##     enterBuff <- tk2entry(tt, textvariable = buff.val, width = 15)
    ##     tkgrid(tklabel(tt, text = "Buffer distance between activity centers (km)? "), 
    ##         enterBuff)
    ##     how.far.val <- tclVar(Parameters$moveDist)
    ##     enterHF <- tk2entry(tt, textvariable = how.far.val, width = 15)
    ##     tkgrid(tklabel(tt, text = "Movement radius (km)? "), enterHF)
    ##     how.much.val <- tclVar(Parameters$moveDistQ)
    ##     enterHM <- tk2entry(tt, textvariable = how.much.val, width = 15)
    ##     tkgrid(tklabel(tt, text = "Proportion of movements in radius? "), 
    ##         enterHM)
    ##     maxDistQ.val <- tclVar(Parameters$maxDistQ)
    ##     entermaxDistQ <- tk2entry(tt, textvariable = maxDistQ.val, 
    ##         width = 15)
    ##     tkgrid(tklabel(tt, text = "Max proportion of movements to allow (1 = include all) "), 
    ##         entermaxDistQ)
    ##     tkgrid(tklabel(tt, text = " "))
    ##     tkgrid(tklabel(tt, text = "---------------Sampling Design---------------"))
    ##     grid.val <- tclVar(Parameters$grid_size)
    ##     enter.grid <- tk2entry(tt, textvariable = grid.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Cell size (in km²)? "), enter.grid)
    ##     HRcutoff.val <- tclVar(Parameters$habitat.cutoff)
    ##     enterHRcut <- tk2entry(tt, textvariable = HRcutoff.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Minimum habitat value for activity centers? "), 
    ##         enterHRcut)
    ##     gridcut.val <- tclVar(Parameters$sample.cutoff)
    ##     enter.grid2 <- tk2entry(tt, textvariable = gridcut.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Proportion of cell in habitat? "), 
    ##         enter.grid2)
    ##     visit.val <- tclVar(Parameters$n_visits)
    ##     enter.visit <- tk2entry(tt, textvariable = visit.val, width = 5)
    ##     tkgrid(tklabel(tt, text = "Maximum visits per year? "), enter.visit)
    ##     tkgrid(tklabel(tt, text = " "))
    ##     check.values <- function(Parameters, n.grps) {
    ##         ok.check <- 1
    ##         if (length(Parameters$buffer) < n.grps) {
    ##             ok.check <- 0
    ##         }
    ##         if (length(Parameters$maxDistQ) < n.grps) {
    ##             ok.check <- 0
    ##         }
    ##         if (length(Parameters$moveDistQ) < n.grps) {
    ##             ok.check <- 0
    ##         }
    ##         if (length(Parameters$moveDist) < n.grps) {
    ##             ok.check <- 0
    ##         }
    ##         if (ok.check == 0) {
    ##             cat("Something's wrong!\n")
    ##             flush.console()
    ##         }
    ##         return(ok.check)
    ##     }
    ##     OK.but <- tkbutton(tt, text = "  OK  ", command = function() tclvalue(done) <- 1)
    ##     Cancel.but <- tkbutton(tt, text = "Cancel", command = function() tclvalue(done) <- 2)
    ##     tkgrid(OK.but)
    ##     tkgrid(Cancel.but)
    ##     tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)
    ##     tkwait.variable(done)
    ##     doneVal <- as.integer(tclvalue(done))
    ##     if (doneVal == 1) {
    ##         Parameters = list(N = as.numeric(tclvalue(N.Val)), lmda = as.numeric(tclvalue(lmda.val)), 
    ##             n_yrs = as.numeric(tclvalue(yrs.val)), MFratio = as.numeric(unlist(strsplit(tclvalue(MF.val), 
    ##                 split = " "))), buffer = as.numeric(unlist(strsplit(tclvalue(buff.val), 
    ##                 split = " "))), moveDist = as.numeric(unlist(strsplit(tclvalue(how.far.val), 
    ##                 split = " "))), moveDistQ = as.numeric(unlist(strsplit(tclvalue(how.much.val), 
    ##                 split = " "))), maxDistQ = as.numeric(unlist(strsplit(tclvalue(maxDistQ.val), 
    ##                 split = " "))), grid_size = as.numeric(tclvalue(grid.val)), 
    ##             habitat.cutoff = as.numeric(tclvalue(HRcutoff.val)), 
    ##             sample.cutoff = as.numeric(tclvalue(gridcut.val)), 
    ##             n_visits = as.numeric(tclvalue(visit.val)))
    ##         n.grps <- as.numeric(tclvalue(grps.val))
    ##         check.values(Parameters, n.grps)
    ##     }
    ##     tkdestroy(tt)
    ##     return(Parameters)
    ## }
    ## 
    ## [[2]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[3]]
    ## [[3]]$value
    ## function (n_runs, map, Parameters, folder.dir = getwd(), run.label = "rSPACE_X", 
    ##     base.name = "rSPACEx", filter.map = NULL, printN = 1, saveParameters = 1, 
    ##     saveGrid = 0, skipConfirm = F, add = F, overwrite = T, showSteps = F) 
    ## {
    ##     list.files("./2.Code/2.Functions/22_create_replicates", full.names = TRUE) %>% 
    ##         lapply(., source) %>% invisible()
    ##     if (!skipConfirm) {
    ##         askConfirm <- ("" == readline(prompt = "\n rSPACE creates text files.\n        If you're ok with this, press ENTER to continue.\n        Typing anything else will exit.\n"))
    ##         if (!askConfirm) {
    ##             message("Exiting function")
    ##             return(0)
    ##         }
    ##     }
    ##     folder.dir <- paste(folder.dir, run.label, sep = "/")
    ##     if (!file.exists(folder.dir)) {
    ##         dir.create(folder.dir)
    ##     }
    ##     output.dir <- paste(folder.dir, "output", sep = "/")
    ##     if (!file.exists(output.dir)) {
    ##         dir.create(output.dir)
    ##     }
    ##     if (printN == 1) {
    ##         printN <- paste0(output.dir, "/N_final.txt")
    ##         if (overwrite) {
    ##             message(paste("Restarting", printN))
    ##             file.remove(printN)
    ##         }
    ##     }
    ##     prevPList <- paste0(output.dir, "/Parameters.rdata")
    ##     if (missing(Parameters)) {
    ##         if (file.exists(prevPList)) {
    ##             message(paste0("Using existing parameters list for scenario from: ", 
    ##                 prevPList))
    ##             load(prevPList)
    ##         }
    ##         else {
    ##             if (add == T) {
    ##                 stop("No parameter list available")
    ##             }
    ##             else {
    ##                 Parameters <- enter.parameters()
    ##             }
    ##         }
    ##     }
    ##     else {
    ##         if (add == T) {
    ##             if (file.exists(prevPList)) {
    ##                 warning(paste0("Using existing parameters list for scenario from: ", 
    ##                   prevPList))
    ##                 rm("Parameters")
    ##                 load(prevPList)
    ##             }
    ##         }
    ##     }
    ##     Parameters <- check_parameters(Parameters, list(n_runs, map, 
    ##         Parameters, folder.dir = getwd(), run.label = "rSPACE_X", 
    ##         base.name = "rSPACEx", filter.map = NULL, printN = 1, 
    ##         saveParameters = 1, saveGrid = 0, skipConfirm = F, add = F, 
    ##         overwrite = T))
    ##     if (missing(map)) {
    ##         stop("Missing habitat layer")
    ##     }
    ##     map <- check_map(map, filter.map)
    ##     grid_layer <- create_grid(map = map, pList = Parameters, 
    ##         filter.map = filter.map, reNumber = T)
    ##     gridIDs <- unique(grid_layer)[unique(grid_layer) > 0]
    ##     n.prevFiles <- length(dir(folder.dir, pattern = base.name))
    ##     rn.start <- ifelse(add, n.prevFiles, 0)
    ##     if (add == F & n.prevFiles > 0 & overwrite == F) {
    ##         stop(paste("\nExisting rSPACE runs found in", folder.dir, 
    ##             "\n Use \"overwrite=T\" to replace or \"add=T\" to add to existing folder"))
    ##     }
    ##     pb <- progress_bar$new(format = "Creating replicates [:bar] :percent eta: :eta", 
    ##         total = max(n_runs), clear = FALSE)
    ##     for (rn in (1:n_runs) + rn.start) {
    ##         cat("\rn", rn, "\n")
    ##         flush.console()
    ##         ch <- encounter_history(map = map, Parameters = Parameters, 
    ##             grid_layer = grid_layer, n_cells = length(gridIDs), 
    ##             printN = printN, rn = rn, filter.map = NULL, showSteps = showSteps, 
    ##             out_folder = output.dir)
    ##         output_file <- paste0(folder.dir, "/", base.name, formatC(rn, 
    ##             digits = 3, format = "d", flag = "0"), ".txt")
    ##         cat(paste("/*", gridIDs, "*/", ch, "1;"), sep = "\n", 
    ##             file = output_file)
    ##         pb$tick()
    ##     }
    ##     if (saveParameters) {
    ##         save(Parameters, file = paste0(output.dir, "/Parameters.Rdata"))
    ##     }
    ##     return(list(DIR = folder.dir, filenames = paste0(base.name, 
    ##         formatC((1:n_runs) + rn.start, digits = 3, format = "d", 
    ##             flag = "0"), ".txt")))
    ## }
    ## 
    ## [[3]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[4]]
    ## [[4]]$value
    ## function (folder, Parameters, SubPop = NULL, sample_matrix = NULL, 
    ##     xxx = 1, max_xxx = 1, min_xxx = 1, base.name = "rSPACEx", 
    ##     results.file = "sim_results.txt", n_runs = NULL, FPCind = TRUE, 
    ##     skipConfirm = F, overwrite = T, add = F, randomize = T) 
    ## {
    ##     list.files("./2.Code/2.Functions/23_test_replicates", full.names = TRUE) %>% 
    ##         grep("CopyOf", ., value = TRUE, invert = TRUE) %>% lapply(., 
    ##         source) %>% invisible()
    ##     if (!skipConfirm) {
    ##         askConfirm <- ("" == readline(prompt = "\n rSPACE creates text files.\n        If you're ok with this, press ENTER to continue.\n        Typing anything else will exit.\n"))
    ##         if (!askConfirm) {
    ##             message("Exiting function")
    ##             return(0)
    ##         }
    ##     }
    ##     cat("\nSetting up...\n")
    ##     time1 <- proc.time()[3]
    ##     n_visits <- Parameters$n_visits
    ##     n_yrs <- Parameters$n_yrs
    ##     output_files <- dir(folder, full.names = T, pattern = paste0("^", 
    ##         paste(base.name, collapse = "|")))
    ##     folder <- paste0(folder, "/output")
    ##     if (!file.exists(folder)) {
    ##         dir.create(folder)
    ##     }
    ##     if ((3 %in% Parameters$alt_model) | (4 %in% Parameters$alt_model) | 
    ##         (5 %in% Parameters$alt_model) | (6 %in% Parameters$alt_model)) {
    ##         samp_mat_folder <- paste0(folder, "/", gsub("sim_results.txt", 
    ##             "", results.file), "variable_sampling_matrices")
    ##         if (!dir.exists(samp_mat_folder)) {
    ##             dir.create(samp_mat_folder)
    ##         }
    ##         else if (dir.exists(samp_mat_folder)) {
    ##             askConfirm <- ("" == readline(prompt = "\n Sampling matrices folder already exists, \n  rerunning the analysis will rewrite these matrices\n  If you're ok with this, press ENTER to continue.\n  Typing anything else will exit.\n"))
    ##             if (!askConfirm) {
    ##                 message("Exiting function")
    ##                 return(0)
    ##             }
    ##         }
    ##     }
    ##     results_file <- paste(folder, results.file, sep = "/")
    ##     if (file.exists(results_file) & !overwrite & !add) {
    ##         stop(paste0("'", results.file, "' already exists; use overwrite = TRUE or add = TRUE or specify a new filename using results.file"))
    ##     }
    ##     if (!file.exists(results_file)) {
    ##         add <- FALSE
    ##     }
    ##     if (add) {
    ##         message("Appending to previous results. If you've changed simulation parameters, this is a BAD IDEA!")
    ##         sim_results <- read.table(results_file, header = T)
    ##         DF <- ddply(sim_results, .(rn), nrow)
    ##         if (nrow(DF) == 1) {
    ##             message("Only one previous scenario (unknown completion); restarting results file")
    ##             add <- FALSE
    ##         }
    ##         tab <- unique(tabulate(DF$V1))
    ##         tab <- tab[tab != 0]
    ##         drop.files <- c()
    ##         if (length(DF) == 1) {
    ##             drop.files <- unique(paste(DF$rn))
    ##         }
    ##         if (length(tab) > 2) {
    ##             stop("Inconsistent number of subsets in results; specify files to analyze using base.name argument")
    ##         }
    ##         if (length(tab) == 2 | length(tab) == 1) {
    ##             drop.files <- unique(paste(DF$rn[DF$V1 == max(DF$V1)]))
    ##             if (length(tab) == 2) {
    ##                 sim_results <- sim_results[sim_results$rn != 
    ##                   paste(DF$rn[DF$V1 == min(DF$V1)]), ]
    ##             }
    ##         }
    ##         output_files <- output_files[!(file_label(output_files) %in% 
    ##             drop.files)]
    ##         rm(tab, drop.files)
    ##     }
    ##     if (!add) {
    ##         sim_results <- run_analysis(n_yrs)
    ##         cat(c(names(sim_results), "n_grid", "n_visits", "detP", 
    ##             "alt_model", "loc_per", "grid_min", "grid_max", "rn", 
    ##             "\n"), file = results_file)
    ##     }
    ##     if (!is.null(sample_matrix)) {
    ##         if (ncol(sample_matrix) != Parameters$n_yrs) {
    ##             message("Number of years in sample_matrix does not match n_yrs.  Resampling from sample_matrix")
    ##             sample_matrix <- sample_matrix[, sample(ncol(sample_matrix), 
    ##                 Parameters$n_yrs, replace = T)]
    ##         }
    ##     }
    ##     if (is.null(Parameters$n_visit_test)) {
    ##         Parameters$n_visit_test <- 2:Parameters$n_visits
    ##     }
    ##     if (is.null(Parameters$detP_test)) {
    ##         Parameters$detP_test <- c(1, 0.8, 0.2)
    ##     }
    ##     if (is.null(Parameters$grid_sample)) {
    ##         Parameters$grid_sample <- c(0.05, 0.15, 0.25, 0.35, 0.45, 
    ##             0.55, 0.75, 0.95)
    ##     }
    ##     if (is.null(Parameters$alt_model)) {
    ##         Parameters$alt_model <- c(0, 1)
    ##     }
    ##     GRDuse <- set_grid(filetest = output_files[1], SubPop)
    ##     gridTotal <- length(GRDuse)
    ##     n_grid_sample <- round(Parameters$grid_sample * gridTotal)
    ##     detP_test <- adjust_det_p(detP_test = Parameters$detP_test)
    ##     if (is.null(n_runs)) {
    ##         n_runs <- length(output_files)
    ##     }
    ##     index <- rep(min_xxx:max_xxx, length.out = n_runs)
    ##     if (sum(grep(4, Parameters$alt_model, invert = TRUE)) > 0) {
    ##         if (max(Parameters$spatial_percents) < 1) {
    ##             Parameters$spatial_percents <- c(Parameters$spatial_percents, 
    ##                 1)
    ##         }
    ##         if (min(Parameters$spatial_percents) > 0) {
    ##             Parameters$spatial_percents <- c(0, Parameters$spatial_percents)
    ##         }
    ##     }
    ##     pb_run <- progress_bar$new(format = "Testing replicates [:bar] :percent eta: :eta", 
    ##         total = max(n_runs), clear = FALSE)
    ##     for (rn in (1:n_runs)[index == xxx]) {
    ##         cat("\n", rn, " ", file_label(output_files[rn]), " ")
    ##         flush.console()
    ##         test <- read_input(output_files[rn])
    ##         GRD <- test$GridID
    ##         test <- test$ch
    ##         use <- sample(match(GRDuse, GRD), gridTotal)
    ##         if (!is.null(randomize)) {
    ##             if (!randomize) {
    ##                 use <- 1:gridTotal
    ##             }
    ##         }
    ##         if ((3 %in% Parameters$alt_model) | (4 %in% Parameters$alt_model) | 
    ##             (5 %in% Parameters$alt_model) | (6 %in% Parameters$alt_model)) {
    ##             run_num <- substring(file_label(output_files[rn]), 
    ##                 nchar(file_label(output_files[rn])) - 3)
    ##             run_mat_folder <- paste0(samp_mat_folder, "/rn_", 
    ##                 formatC(run_num, flag = 0, digits = 4, format = "d"))
    ##             if (!dir.exists(run_mat_folder)) {
    ##                 dir.create(run_mat_folder)
    ##             }
    ##         }
    ##         detPhold <- 1
    ##         for (detPt in detP_test) {
    ##             test <- drop_det_p(ch = test, detP = detPt)
    ##             detPhold <- detPhold * detPt
    ##             for (altM in Parameters$alt_model) {
    ##                 for (n_grid in n_grid_sample) {
    ##                   if (n_grid != max(n_grid_sample) & (altM == 
    ##                     5 | altM == 6)) {
    ##                     next
    ##                   }
    ##                   suppressWarnings(rm("ch1"))
    ##                   if (altM == 3 | altM == 4 | altM == 5 | altM == 
    ##                     6) {
    ##                     ch1 <- test
    ##                     grd1 <- GRD
    ##                   }
    ##                   else {
    ##                     ch1 <- test[use[1:n_grid]]
    ##                     grd1 <- GRD[use[1:n_grid]]
    ##                   }
    ##                   fpc <- ifelse(FPCind, finite_pop_corr(n_grid, 
    ##                     gridTotal), 1)
    ##                   for (n_visit in Parameters$n_visit_test) {
    ##                     suppressWarnings(rm("ch"))
    ##                     ch <- drop_visits(ch = ch1, n_visits, n_yrs, 
    ##                       n_visit)
    ##                     for (loc_percent in Parameters$spatial_percents) {
    ##                       if (altM == 4) {
    ##                         loc_per_folder <- paste0(run_mat_folder, 
    ##                           "/sp_percent_", formatC(loc_percent * 
    ##                             100, flag = 0, digits = 1, format = "d"))
    ##                         if (!dir.exists(loc_per_folder)) {
    ##                           dir.create(loc_per_folder)
    ##                         }
    ##                         enc_hist_folder <- paste0(run_mat_folder, 
    ##                           "/sp_percent_", formatC(loc_percent * 
    ##                             100, flag = 0, digits = 1, format = "d"), 
    ##                           "/encounter_histories")
    ##                         if (!dir.exists(enc_hist_folder)) {
    ##                           dir.create(enc_hist_folder)
    ##                         }
    ##                       }
    ##                       else if (altM != 4 & loc_percent != max(Parameters$spatial_percents)) {
    ##                         next
    ##                       }
    ##                       sim_results <- run_analysis(n_yrs, ch, 
    ##                         n_visit, sample_yr = altM, FPC = ifelse(altM < 
    ##                           4, fpc, 1), sample_matrix = sample_matrix, 
    ##                         n_grid = n_grid, loc_percent = loc_percent, 
    ##                         grid_min = Parameters$grid_min, grid_max = Parameters$grid_max, 
    ##                         alpha = Parameters$alpha, beta = Parameters$beta, 
    ##                         base = Parameters$base)
    ##                       if (altM == 3 | altM == 5 | altM == 6) {
    ##                         sim_mat_file <- paste0(run_mat_folder, 
    ##                           "/", detPt * 100, "detP_", n_grid, 
    ##                           "cells_", n_visit, "visits.txt")
    ##                         write.table(sim_results$samp_mat, file = sim_mat_file, 
    ##                           row.names = FALSE, col.names = FALSE)
    ##                         for (i in 1:nrow(sim_results$sim_results)) {
    ##                           cat(c(unlist(sim_results$sim_results[i, 
    ##                             ]), n_grid, n_visit, detPhold, altM, 
    ##                             loc_percent, Parameters$grid_min, 
    ##                             Parameters$grid_max, file_label(output_files[rn]), 
    ##                             "\n"), file = results_file, append = T)
    ##                         }
    ##                       }
    ##                       else if (altM == 4) {
    ##                         sim_mat_file <- paste0(loc_per_folder, 
    ##                           "/", detPt * 100, "detP_", n_grid, 
    ##                           "cells_", n_visit, "visits.txt")
    ##                         write.table(sim_results$samp_mat, file = sim_mat_file, 
    ##                           row.names = FALSE, col.names = FALSE)
    ##                         enc_hist_file <- paste0(enc_hist_folder, 
    ##                           "/", detPt * 100, "detP_", n_grid, 
    ##                           "cells_", n_visit, "visits.txt")
    ##                         write.table(sim_results$enc_hist, file = enc_hist_file, 
    ##                           row.names = FALSE, col.names = FALSE)
    ##                         for (i in 1:nrow(sim_results$sim_results)) {
    ##                           cat(c(unlist(sim_results$sim_results[i, 
    ##                             ]), n_grid, n_visit, detPhold, altM, 
    ##                             loc_percent, Parameters$grid_min, 
    ##                             Parameters$grid_max, file_label(output_files[rn]), 
    ##                             "\n"), file = results_file, append = T)
    ##                         }
    ##                       }
    ##                       else {
    ##                         for (i in 1:nrow(sim_results)) {
    ##                           cat(c(unlist(sim_results[i, ]), n_grid, 
    ##                             n_visit, detPhold, altM, loc_percent, 
    ##                             Parameters$grid_min, Parameters$grid_max, 
    ##                             file_label(output_files[rn]), "\n"), 
    ##                             file = results_file, append = T)
    ##                         }
    ##                       }
    ##                     }
    ##                   }
    ##                 }
    ##             }
    ##         }
    ##         pb_run$tick()
    ##     }
    ##     cat("\n")
    ##     return(proc.time()[3] - time1)
    ## }
    ## 
    ## [[4]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[5]]
    ## [[5]]$value
    ## function (folder, sing_max = 5, CI = 0.95, returnData = 1, plot = T) 
    ## {
    ##     list.files("./2.Code/2.Functions/24_get_results", full.names = TRUE) %>% 
    ##         lapply(., source) %>% invisible()
    ##     dta <- get_data(folder = folder, sing_max = sing_max, CI = CI)
    ##     dtaS <- sum_data(dta)
    ##     count <- n_runs <- n_grid <- n_visits <- detP <- alt_model <- loc_per <- NULL
    ##     if (plot) {
    ##         print(ggplot(subset(dtaS, detP < 1 & alt_model < 4), 
    ##             aes(x = n_grid, y = (count/n_runs), group = interaction(n_visits, 
    ##                 detP, alt_model, loc_per))) + geom_line(aes(colour = n_visits, 
    ##             linetype = factor(detP)), linewidth = 1.25) + scale_colour_gradient(name = "# visits", 
    ##             guide = "legend") + scale_linetype_discrete(name = expression(p["sim"])) + 
    ##             scale_y_continuous(limits = c(0, 1)) + labs(x = "Number of cells sampled", 
    ##             y = "Detected trend/Number of replicates", title = basename(folder)) + 
    ##             facet_wrap(~alt_model))
    ##     }
    ##     if (returnData == 1) {
    ##         return(dta)
    ##     }
    ##     if (returnData == 2) {
    ##         return(dtaS)
    ##     }
    ## }
    ## 
    ## [[5]]$visible
    ## [1] FALSE
    ## 
    ## 
    ## [[6]]
    ## [[6]]$value
    ## function (folder, data, CI = 0.95, pwr = 0.8, n_grid = NULL) 
    ## {
    ##     tryWARN <- function(xx) {
    ##         suppressWarnings(tryCatch(withCallingHandlers(xx, warning = function(w) {
    ##             message("Smoothed estimates may not be reliable.\nTry increasing number of data points or number of simulations\n")
    ##         }), error = function(x) {
    ##             return(NA)
    ##         }))
    ##     }
    ##     tryNA <- function(xx) {
    ##         tryCatch(xx, error = function(x) {
    ##             return(NA)
    ##         })
    ##     }
    ##     if (missing(folder) & missing(data)) {
    ##         stop("Must supply either a folder or data")
    ##     }
    ##     if (missing(data)) {
    ##         dta <- get_data(folder, CI)
    ##         dtaS <- sum_data(dta)
    ##     }
    ##     else {
    ##         dtaS <- data
    ##     }
    ##     dtaS$percent <- dtaS$count/dtaS$total
    ##     extra.var <- which(!(names(dtaS) %in% c("n_grid", "total", 
    ##         "n_runs", "count", "percent")) & unname(apply(dtaS, 2, 
    ##         function(x) length(unique(x)) > 1)))
    ##     if (is.null(n_grid)) {
    ##         fit_loess_pwr <- function(DF, percent) {
    ##             fit <- tryWARN(loess(n_grid ~ percent, data = DF))
    ##             fit <- tryNA(as.numeric(predict(fit, data.frame(percent = percent))))
    ##             return(data.frame(n_grid = fit))
    ##         }
    ##         fit <- ddply(dtaS, names(dtaS)[extra.var], fit_loess_pwr, 
    ##             percent = pwr)
    ##     }
    ##     else {
    ##         fit_loess_grid <- function(DF, n_grid) {
    ##             fit <- tryWARN(loess(percent ~ n_grid, data = DF))
    ##             fit <- tryNA(as.numeric(predict(fit, data.frame(n_grid = n_grid))))
    ##             return(data.frame(pwr = fit))
    ##         }
    ##         fit <- ddply(dtaS, names(dtaS)[extra.var], fit_loess_grid, 
    ##             n_grid = n_grid)
    ##     }
    ##     if (all(is.na(fit[, ncol(fit)]))) {
    ##         stop("Smoothing function failed")
    ##     }
    ##     else if (any(is.na(fit[, ncol(fit)]))) {
    ##         message("Tested values out of range, NAs returned\n")
    ##     }
    ##     return(fit)
    ## }
    ## 
    ## [[6]]$visible
    ## [1] FALSE

``` r
dyn.load("2.Code/2.C_Scripts/SPACE.dll")
```

# Data

# Establishes the location of the output folder for saving results to

output_folder \<- here::here(“3.Outputs”)

## Setup
