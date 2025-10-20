# SDM Modeling 
#Author: Lizbeth Amador 
#This script creates ensembled models for phenology and range, two time periods, across species 

#########
#Presets 
#########
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

#foreach - for loops with parallelization 
if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")}; require(parallel)


#############
#Directories
#############
##Data origin 
#getwd() #curious what it looks like 
# mwd = "/iplant/home/esokol/esiil_macrophenology" 
mwd = "~/data-store/home/lamador/Data"
# L2 = file.path(mwd, "L2/Across_spp_SDM")
L2 = file.path(mwd, "L2")
# em = file.path(mwd, "SDM/SDM_Uncertainty") #output file for results 
em = file.path(L2, "SDM/SDM_Uncertainty")

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

########### read climate rasters ############
# 1. Save rasters into a list  
#^: starts with, .*:  matches any characters, including spaces, after the underscore, \\.tif$: must end with .tif  
tif_files_h = list.files(file.path(L2, "sp_ranges_clim"), 
                         pattern = "^ClimRastS_H_cropped_.*\\.tif$", full.names = TRUE)
tif_files_c = list.files(file.path(L2, "sp_ranges_clim"), 
                         pattern = "^ClimRastS_C_cropped_.*\\.tif$", full.names = TRUE)
# 2. Read each raster as a SpatRaster and store in a list
rasters_h <- lapply(tif_files_h, rast)
rasters_c <- lapply(tif_files_c, rast)
# 3. Extract species names from file names (e.g., from "ClimRastS_H_cropped_Acer glabrum.tif")
species_names <- tools::file_path_sans_ext(basename(tif_files_h))
species_names <- gsub("ClimRastS_H_cropped_", "", species_names)  # adjust to your naming
species_names <- gsub("_", " ", species_names)
# 4. Load rasters into a named list 
sp.clim.rasters.h <- setNames(lapply(tif_files_h, rast), species_names)
sp.clim.rasters.c <- setNames(lapply(tif_files_c, rast), species_names)

sp = intersect(names(phe.h[3:length(phe.h)]), names(phe.c[3:length(phe.c)]))

sp_names = sp[c(3, 92)]

#############
# Function 
#############
#Function inputs 
#df: phe.h, phe.c, rang.h, rang.c
#sp: species
#time: H, C
#type: Phenology, Range
# df = phe.h; time = "H"; type="Phenology"; sp = "Acer glabrum"
em.fun = function(df, sp, time, type){
  
  #Setting working directory to where all initial SDM outputs are saved
  #We need to set this because biomod2 will automatically look for supporting files that are in the folder
  setwd(em) 
  
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
  if (sum(myResp == 1, na.rm = TRUE) == 0) {
    message("\nSkipping", sp, "- no presence data.\n")
    next
    }
  
  #To pull time specific climate rasters 
  if(time == "H"){
    sp.clim = list(sp.clim.rasters.h)
  } else {
    sp.clim = list(sp.clim.rasters.c)
  }
  # sp.clim = sp.clim[[sp]]
  #check if species raster exists 
  if(is.null(sp.clim[[1]][[myRespName]])) {
    message("Skipping ", myRespName, ": no raster found.")
    next
  }
  
  #sample size of presences 
  nb.p = sum(myResp == 1, na.rm = TRUE)
  # Set PA.nb.rep conditionally -- (Barbet-Massin et al. 2012; Methods Ecol. Evol.)
  if (nb.p < 1000) {
    pa.nb.rep <- 10
  } else {
    pa.nb.rep <- 2
  }
  
  #Format data for biomod 
  bmdat = BIOMOD_FormatingData(
    resp.name = myRespName, #species name
    resp.var = myResp, #presences-absences
    resp.xy = myRespXY, #lat/lon
    expl.var = sp.clim[[1]][[myRespName]], #raster stack 
    PA.nb.rep = pa.nb.rep,
    PA.nb.absences = nb.p, #same number as presences (Barbet-Massin et al. 2012; Methods Ecol. Evol.)  
    PA.strategy = 'random' #method 
  )
  
  
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
                                        # nb.cpu = 8), #computing resources
                                        metric.eval = c('TSS','ROC')) #evaluation metrics 
  
  ##save biomodout file
  saveRDS(biomod.sm, 
          file = file.path(em, paste0(gsub("\\.", "_", biomod.sm@sp.name),
                                     "_Biomod_SM_", time, "_", type, ".rds")))
  
  
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
    filter(modeleval.val...calibration... >= 0.6) # select models that had TSS over 0.6, done by Carroll et al. 
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
  if (length(bestmodsfullnames) < 1) {
    message("\nSkipping", i, "- no best models selected\n")
    next #<-- SKIPS the current species 
  }
  
  
  ###########################
  ## iv. Ensemble model
  ###########################
  #Model ensemble models
  biomod.em <- biomod2::BIOMOD_EnsembleModeling(bm.mod = biomod.sm, #singles model output
                                                 models.chosen = bestmodsfullnames, #vector of best models
                                                 em.by = 'all', #what models will be combined to ensemble
                                                 em.algo = c('EMcv', 'EMmean'), #types of ensembles models to be computed 
                                                 metric.select = c('TSS'),
                                                 metric.eval = c('TSS', 'ROC'), #evaluation metrics to filter models
                                                 var.import = 3, #num permutationsto est var importance
                                                 EMci.alpha = 0.05, #significance level 
                                                 EMwmean.decay = 'proportional') #relative importance of weights 
  
  ##save biomodout file
  saveRDS(biomod.em, 
          file = file.path(em, paste0(gsub("\\.", "_", biomod.em@sp.name),
                                                  "_BiomodEM_", time, "_", type, ".rds")))
  
  ###########################
  ## v. Ensemble Projections  
  ###########################
  # Projecting across space using initial ensemble outputs and pre-modeling data. 
  mod = paste0(biomod.em@em.computed) 
  
  for(i in mod){ #START of for loop
    #Project species - will pick best one
    em.proj <- tryCatch({biomod2::BIOMOD_EnsembleForecasting(biomod.em, #output from ensemble 
                                                         proj.name = myRespName, #species name
                                                         new.env = sp.clim[[1]][[myRespName]], #enviro matrix
                                                         new.env.xy = myRespXY,
                                                         models.chosen = mod[i]) #change back to i
      }, error = function(e) {
        message("Ensemble Forecasting failed for ", myRespName, ": ", e$message)
        return(NULL)
      })
    ##save projection file
    saveRDS(em.proj, 
            file = file.path(em, paste0(gsub("\\.", "_", em.proj@sp.name),
                                                     "_EMproj_", time, "_", type, ".rds")))
    
    print("Projection output file link:")
    print(em.proj@proj.out@link)
    #read raster output
    r = try(rast(em.proj@proj.out@link))
    
    #Ensuring CRS 
    if (inherits(r, "SpatRaster") && (is.na(crs(r)) || crs(r) == "")) {
      message("Ensemble projection raster has no CRS. Assigning EPSG:4326 manually.")
      crs(r) <- "EPSG:4326"}
    
    #write ouput with new name -- will be overwritten by the current timeperiod model since the species share the same name 
    try(writeRaster(r, 
                    file = file.path(em, paste0(gsub("\\.", "_", models[i]), 
                                                   "_EMproj_", time, "_", type, ".tif")), overwrite = TRUE))
    
    #Adding a small delay to avoid overwhelming the server
    Sys.sleep(1)
  } #END of ensemble raster for loop
  
 } #END of function 


#######################
# Function per species
#######################
#Function to run all time-type combos for one species 
em.fun.one.sp = function(sp1){
  system.time({
  x1 = em.fun(df = phe.h, sp = sp1, time = "H", type = "Phenology")
  x2 = em.fun(df = phe.c, sp = sp1, time = "C", type = "Phenology")
  x3 = em.fun(df = rang.h, sp = sp1, time = "H", type = "Range")
  x4 = em.fun(df = rang.c, sp = sp1, time = "C", type = "Range")
  })#END of timer
  }

#sp_names is a vector of species to be processed
x = parallel::mclapply(sp_names, FUN = em.fun.one.sp, mc.cores = 2) #core = num species to run in parallel 





    
    