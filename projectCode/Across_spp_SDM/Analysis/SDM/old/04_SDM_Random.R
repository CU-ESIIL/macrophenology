# SDM Modeling 
#Author: Lizbeth Amador 
#This script creates ensembled models for phenology and range, two time periods, across species 
#Applies pseudo absences to all models 
#point based ensemble 

#########
#Presets 
#########
#1) Kill native over-threading that causes crashes
Sys.setenv(OPENBLAS_NUM_THREADS="1", OMP_NUM_THREADS="1", MKL_NUM_THREADS="1", GDAL_CACHEMAX="2048")
#2) Unique tempdir per process to avoid collisions
tmp_root <- file.path(Sys.getenv("HOME"), "r_tmp"); dir.create(tmp_root, TRUE)
tmp_dir  <- file.path(tmp_root, paste0("biomod_", Sys.getpid())); dir.create(tmp_dir, TRUE)
#3) raster (not terra) IO limits; BIOMOD2 is raster-centric
suppressPackageStartupMessages(library(raster))
rasterOptions(tmpdir=tmp_dir, chunksize=5e7, maxmemory=5e8, progress="text")
#4) Force no forked parallelism anywhere
options(mc.cores = 1)

# Load necessary libraries
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")}; library(tidyverse)
#terra - spatial package 
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")}; require(terra)
#biomod2 - ensemble modeling
if (!requireNamespace("biomod2", quietly = TRUE)) {
  install.packages("biomod2")}; require(biomod2)
#dismo - ensemble modeling
if (!requireNamespace("dismo", quietly = TRUE)) {
  install.packages("dismo")}; require(dismo)
#randomForest - ensemble modeling - For RF model
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")}; require(randomForest)
#ggtext - for biomod2 plotting 
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")}; require(ggtext)
#tools -
if (!requireNamespace("tools", quietly = TRUE)) {
  install.packages("tools")}; require(tools)
#parallel - for loops with parallelization 
if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")}; require(parallel)
# #doParallel - for loops with parallelization 
# if (!requireNamespace("doParallel", quietly = TRUE)) {
#   install.packages("doParallel")}; require(doParallel)
# #foreach - faster for loops 
# if (!requireNamespace("foreach", quietly = TRUE)) {
#   install.packages("foreach")}; require(foreach)


#############
#Directories
#############
##Main working directory
mwd = "G:/Shared drives/MSB_Phenology/Amador/Amador, Liz-Phenology Dissertation 2023/Chapters/Chapter4/Data"
# mwd = "/iplant/home/esokol/esiil_macrophenology"
# mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
# L2 = file.path(mwd, "L2/Across_spp_SDM") #data 
# L2.clim = file.path(L2, "SDM/clim_data_unzipped/sp_ranges_clim")
L2.clim = file.path(L2, "sp_ranges_clim") #using personal folder for climate data
#output directory -- important to set wd here for biomod2 to pull dependencies
em = file.path("D:/Career_Journey/Collaborations/ESIIL_Macrophenology/Across_spp_SDM/Data/L2/SDM/SDM_Uncertainty") #output file for results

#Ensure the directory exists!
if (!dir.exists(em)) {
  dir.create(em, recursive = TRUE)
  message("Created directory at: ", em)
} else {
  message("Using existing directory at: ", em)
}

######
#Data
######
#load objects 
load(file.path(L2, "phenology_timeperiods_thinned_cleaned_focalspecies.RData"), verbose = TRUE)
load(file.path(L2, "range_timeperiods_thinned_cleaned_focalspecies.RData"), verbose = TRUE)
#see if there are different lengths 
sp = intersect(names(phe.h[3:length(phe.h)]), names(phe.c[3:length(phe.c)]))

#Climate rasters 
#1. List file pathways
tif_files_h = list.files(file.path(L2, "sp_ranges_clim"), pattern = "^ClimRastS_H_cropped_.*\\.tif$", full.names = TRUE)
#2. Read them as rasters
rasters_h <- lapply(tif_files_h, rast)
#3. Pull species names from file pathways 
species_names <- tools::file_path_sans_ext(basename(tif_files_h))
species_names <- gsub("ClimRastS_H_cropped_", "", species_names)  # adjust to your naming
species_names <- gsub("_", " ", species_names)
#4. Set list labels to species names 
sp.clim.h <- setNames(lapply(tif_files_h, rast), species_names)
#---
#1. List file pathways
tif_files_c = list.files(file.path(L2, "sp_ranges_clim"), pattern = "^ClimRastS_C_cropped_.*\\.tif$", full.names = TRUE)
#2. Read them as rasters
rasters_c <- lapply(tif_files_c, rast)
#3. Pull species names from file pathways
species_names <- tools::file_path_sans_ext(basename(tif_files_c))
species_names <- gsub("ClimRastS_C_cropped_", "", species_names)  # adjust to your naming
species_names <- gsub("_", " ", species_names)
#4. Set list labels to species names
sp.clim.c <- setNames(lapply(tif_files_c, rast), species_names)

#For testing
# df = phe.h; sp.clim = sp.clim.h; sp = sp_names; time = "H"; type = "Phenology"

#############
# Function 
#############
#Function inputs 
#df: phe.h, phe.c, rang.h, rang.c
#sp: species
#time: H, C
#type: Phenology, Range
#e.g., df = phe.h; time = "H"; type= "Phenology"; sp = some character of species 
em.fun = function(df, sp.clim, sp, time, type){
  
  #Setting working directory to where all initial SDM outputs are saved
  #We need to set this because biomod2 will automatically look for supporting files that are in the folder
  setwd(em) 
  
  # sp.clim = list(sp.clim.rasters.h)
  class(sp.clim)

    
    ###########################
    ## i. `biomod2` formatting 
    ###########################
    #Select species name
    myRespName = sp
    # Get corresponding P data
    myResp = df[, myRespName]
    # Making sure there are no 0s
    myResp = ifelse(myResp == 1, 1, NA)
    #Get corresponding coordinates
    myRespXY = as.data.frame(df[, c('longitude', 'latitude')]) #Make sure long goes first!
    
    #Check if we have at least one presence
    if(sum(myResp == 1, na.rm = TRUE) == 0) {
      message("\nSkipping", myRespName, "- no presence data.\n")
      return(NULL)
      # next
    }
    
    #check if species raster exists 
    if(is.null(sp.clim[[myRespName]])) {
      message("Skipping ", myRespName, ": no raster found.")
      return(NULL)
      # next 
    }
    
    #Conditional sample size of presences
    nb.p = sum(myResp == 1, na.rm = TRUE)
    # Set PA.nb.rep conditionally -- (Barbet-Massin et al. 2012; Methods Ecol. Evol.)
    if(nb.p < 1000) {
      pa.nb.rep <- 10
    } else {
      pa.nb.rep <- 2
    }
    
    #Format data for biomod
    bmdat = BIOMOD_FormatingData(
      resp.name = myRespName, #species name
      resp.var = myResp, #presences-absences
      resp.xy = myRespXY, #lat/lon
      expl.var = sp.clim[[myRespName]], #raster stack
      PA.nb.rep = pa.nb.rep,
      PA.nb.absences = nb.p, #same number as presences (Barbet-Massin et al. 2012; Methods Ecol. Evol.)
      PA.strategy = 'random' #method
    )
    
    #Extract the data frame of presences + pseudo-absences into csv
    bmdat.df <- dplyr::bind_cols(
      longitude = bmdat@coord[, 1],
      latitude = bmdat@coord[, 2],
      status = bmdat@data.species,
      bmdat@PA.table
    )
    #Save biomodout file
    out <- file.path(em, paste0(gsub("\\.", "_", bmdat@sp.name), "_Biomod_bmdat_", time, "_", type, ".csv"))
    print(out)
    write.csv(bmdat.df, file = out, row.names = FALSE)
  
    
    ###########################
    ## ii. Single models
    ###########################
    #For modeling options see: https://biomodhub.github.io/biomod2/reference/ModelsTable.html
    # Model single models
    biomod.sm <- BIOMOD_Modeling(bm.format = bmdat, #formatted biomod data
                                 models = "RF",
                                 CV.strategy = 'kfold', #cross-validation strategy
                                 CV.nb.rep = 3, #cross-val repititions
                                 CV.k = 5, #cross-val partitions
                                 OPT.strategy = 'bigboss', #model param selection
                                 var.import = 3, #number of permutations to est var importance
                                 seed.val = 1, #to keep same results when rerunning
                                 nb.cpu = 1, #computing resources
                                 metric.eval = c('TSS','ROC')) #evaluation metrics
    
    ##save biomodout file
    out <- file.path(em, paste0(gsub("\\.", "_", biomod.sm@sp.name), "_Biomod_SM_", time, "_", type, ".rds"))
    print(out)
    saveRDS(biomod.sm, file = out)
    
    
    ###########################
    ## iii. Pull best models
    ###########################
    ##CALIBRATION: choose high scoring models
    modeleval <- biomod.sm@models.evaluation
    #Create a new data frame with stuff
    modelevaldataset <- data.frame(modeleval@val[["full.name"]], modeleval@val[["algo"]],
                                   modeleval@val[["metric.eval"]], modeleval@val[["validation"]],
                                   modeleval@val[["calibration"]])
    #Filter based on a TSS threshold
    bestmodelscal <- modelevaldataset %>%
      filter(modeleval.val...metric.eval...== "TSS") %>%
      filter(modeleval.val...calibration... >= 0.6) # select models that had TSS over 0.6, done by Carroll et al. 2024; Ecol. App.
    ##VALIDATION: choose high scoring models
    #Filter based on a TSS threshold
    bestmodelsval <- modelevaldataset %>%
      filter(modeleval.val...metric.eval...== "TSS") %>%
      filter(modeleval.val...validation... > 0.6)
    ##best models
    bestmods <- c(unique(bestmodelscal$modeleval.val...algo...), unique(bestmodelsval$modeleval.val...algo...))
    #calibration
    filt.cal<- bestmodelscal %>%
      filter(modeleval.val...algo...%in% bestmods)
    #validation
    filt.val <- bestmodelsval %>%
      filter(modeleval.val...algo...%in% bestmods)
    #full names of best models
    bestmodsfullnames = c(filt.cal$modeleval.val...full.name..., filt.val$modeleval.val...full.name...)
    
    #check if any best models
    if(length(bestmodsfullnames) < 1) {
      message("\nSkipping", myRespName, "- no best models selected\n")
      return(NULL)
      # next
    }
    
    
    ###########################
    ## iv. Ensemble model
    ###########################
    #Model ensemble models
    biomod.em <- try(biomod2::BIOMOD_EnsembleModeling(bm.mod = biomod.sm, #singles model output
                                                      models.chosen = bestmodsfullnames, #vector of best models
                                                      em.by = 'all', #what models will be combined to ensemble
                                                      em.algo = c('EMcv', 'EMmean'), #types of ensembles models to be computed (coef. pf variation & mean)
                                                      metric.select = c('TSS'),
                                                      metric.eval = c('TSS', 'ROC'), #evaluation metrics to filter models
                                                      var.import = 3, #num permutationsto est var importance
                                                      EMci.alpha = 0.05, #significance level
                                                      EMwmean.decay = 'proportional'), #relative importance of weights
                     
                     silent = TRUE)
    #Skip if ensemble modeling failed
    if (inherits(biomod.em, "try-error")) {
      message("Skipping ", myRespName, ": ensemble modeling failed.")
      return(NULL)
      # next
    }
    
    #save biomodout file
    out <- file.path(em, paste0(gsub("\\.", "_", biomod.em@sp.name), "_Biomod_EM_", time, "_", type, ".rds"))
    print(out)
    saveRDS(biomod.em, file = out)
    
    message("Initial Ensemble done")
    
    
    ###########################
    ## Ensemble Projections
    ###########################
    mod <- biomod.em@em.computed   # full names
    print(mod)
    proj = paste0(time, "_", type)
    
    em.proj <- tryCatch(
      biomod2::BIOMOD_EnsembleForecasting(
        bm.em = biomod.em,
        proj.name = proj,      # name of folder
        new.env = raster::stack(sp.clim[[myRespName]]), #sp.clim[[myRespName]] # still pass env in case biomod2 needs it
        new.env.xy = myRespXY,
        models.chosen = "all",
        # na.rm = TRUE, 
        # do.stack = FALSE, #avoid writing big stacked files
        # keep.in.memory = TRUE, #keep all projections in memory or only point to link in hard drive
        # compress = FALSE,
        on_0_1000 = FALSE,
        nb.cpu = 1 # <- makes sure only one core allocated -- no threading
      ))
    # ),
    # error = function(e) { #This overrides the error message
    #   message("Projection error for ", myRespName, ": ", conditionMessage(e))
    #   return(NULL)
    #   })
    
    #Save biomod ensemble file
    out <- file.path(em, paste0(gsub("\\.", "_", em.proj@sp.name), "_Biomod_EMPRJ_", time, "_", type, ".rds"))
    print(out)
    saveRDS(em.proj, file = out)
    message("End of Projections")
    
    
    message("Raster extraction: ", myRespName)
    #path to raster
    r.path = em.proj@proj.out@link #note: raster includes all ensemble model algorithms (layers)
    #read raster
    r = try(terra::rast(r.path))
    #save raster to em with new name
    terra::writeRaster(r, filename = file.path(em, paste0(
      gsub("\\.", "_", em.proj@sp.name), "_cv_mean_", time, "_", type, ".tif")), overwrite = TRUE)
    
    message("Done with ", myRespName)
    
    #give some time to avoid overloading server
    Sys.sleep(1)
  # }#END of for loop
  
} #END of function


#######################
# Function per species
#######################
#Identifty the number of available cores 
detectCores()#set the range of species to be modeled

sp_names = sp[2:length(sp)]

#Function to run all time-type combos for one species
em.fun.one.sp = function(sp1){
  system.time({
    message("\n=== Starting species: ", sp1, " ===")
    
    res = list()
    res$phe_h <- tryCatch(
      em.fun(df = phe.h, sp.clim = sp.clim.h, sp = sp1, time = "H", type = "Phenology"),
      error = function(e) { message("Error in phe_h: ", conditionMessage(e)); NULL }
    )
    res$phe_c <- tryCatch(
      em.fun(df = phe.c, sp.clim = sp.clim.c, sp = sp1, time = "C", type = "Phenology"),
      error = function(e) { message("Error in phe_c: ", conditionMessage(e)); NULL }
    )
    res$rang_h <- tryCatch(
      em.fun(df = rang.h, sp.clim = sp.clim.h, sp = sp1, time = "H", type = "Range"),
      error = function(e) { message("Error in rang_h: ", conditionMessage(e)); NULL }
    )
    res$rang_c <- tryCatch(
      em.fun(df = rang.c, sp.clim = sp.clim.c, sp = sp1, time = "C", type = "Range"),
      error = function(e) { message("Error in rang_c: ", conditionMessage(e)); NULL }
    )
    
    res
  })
}


#sp_names is a vector of species to be processed in parallel 
x = lapply(sp_names, FUN = em.fun.one.sp)

#Sum elapsed time across list items 
total_elapsed <- sum(sapply(x, function(i) i[["elapsed"]]))
#Convert to minutes if desired
min <- round(total_elapsed / 60, 2)
#readable output 
if(min > 59){
  hr = round(min/60, 2)
  message("Total run time: ", hr, "hr")
} else{
  message("Total run time: ", min, "min")
}
