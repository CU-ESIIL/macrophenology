# Modeling Phenology 
#Author: Lizbeth Amador 
#This script creates ensembled models for different species, using a for loop  
#and for pseudo-absences 

#########
#Presets 
#########
# Load necessary libraries
#if statement to automatically install libraries if absent in r library
#tidyverse - mainly for data wrangling & plotting/mapping
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

#terra - spatial package 
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}
require(terra)

#biomod2 - ensemble modeling
if (!requireNamespace("biomod2", quietly = TRUE)) {
  install.packages("biomod2")
}
require(biomod2)

#dismo - ensemble modeling
if (!requireNamespace("dismo", quietly = TRUE)) {
  install.packages("dismo")
}
require(dismo)

#randomForest - ensemble modeling - For RF model
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
require(randomForest)

#ggtext - for biomod2 plotting 
if (!requireNamespace("ggtext", quietly = TRUE)) {
  install.packages("ggtext")
}
require(ggtext)

#tools -  
if (!requireNamespace("tools", quietly = TRUE)) {
  install.packages("tools")
}
require(tools)

#foreach - for loops with parallelization 
if (!requireNamespace("foreach", quietly = TRUE)) {
  install.packages("foreach")
}
require(foreach)


#Directories
##Data origin 
getwd() #curious what it looks like 
#ensemble folder for phenology - output folder
#dir.create("Phenology") #create directory 
L2.em = file.path(getwd(), "Phenology")

#Ensure the directory exists!
if (!dir.exists(L2.em)) {
  dir.create(L2.em, recursive = TRUE)
message("Created directory at: ", L2.em)
} else {
  message("Using existing directory at: ", L2.em)
}


#Data
#load objects 
load("phenology_timeperiods_thinned_cleaned_focalspecies.RData", verbose = TRUE)

#list of species with enough true absence observations (n>=10)
sp0 = read.csv(file = "Focal_Species_List_AbsenceThreshold_TimeComparisions.csv", header = TRUE)
#pull species list 
sp0 = unique(sp0$species)
#remove columns that are in the species list -- want to test those with  insufficient true absence observations
phe.c = phe.c[, setdiff(names(phe.c), sp0)]


#read climate rasters 
# 1. Save rasters into a list  
#^: starts with, .*:  matches any characters, including spaces, after the underscore, \\.tif$: must end with .tif  
tif_files_c = list.files(file.path(getwd()), 
                         pattern = "^ClimRastS_C_cropped_.*\\.tif$", full.names = TRUE)
# 2. Read each raster as a SpatRaster and store in a list
rasters_c <- lapply(tif_files_c, rast)
# 3. Extract species names from file names (e.g., from "ClimRastS_H_cropped_Acer glabrum.tif")
species_names <- tools::file_path_sans_ext(basename(tif_files_c))
species_names <- gsub("ClimRastS_C_cropped_", "", species_names)  # adjust to your naming
species_names <- gsub("_", " ", species_names)
# 4. Load rasters into a named list 
sp.clim.rasters.c <- setNames(lapply(tif_files_c, rast), species_names)


#set working directory to where you want biomod outputs to save
setwd("Phenology")

#for loop preset - species list 
sp = names(phe.c[138:length(phe.c)])
head(sp)

##################################
# Current  
##################################
system.time({
  for(i in sp) { #START of foreach loop
    
    #Message to keep track of species being processed
    message(paste("Processing", i, "..."))
    
    ###########################
    ## i. `biomod2` formatting 
    ###########################
    #Select species name
    myRespName.c = i
    # Get corresponding P data
    myResp.c = phe.c[, myRespName.c]
    # Making sure there are no 0s
    myResp.c = ifelse(myResp.c == 1, 1, NA)
    #Get corresponding coordinates
    myRespXY.c = as.data.frame(phe.c[, c('longitude', 'latitude')]) #Make sure long goes first!
    
    # Check if we have at least one presence
    if (sum(myResp.c == 1, na.rm = TRUE) == 0) {
      message("\nSkipping", i, "- no presence data.\n")
      next
    }

    #Skip species with missing raster data
    raster_input <- sp.clim.rasters.c[[myRespName.c]]
    if (is.null(raster_input)) {
      message("Skipping ", myRespName.c, ": no raster found.")
      next # <--- SKIPS the rest of the code inside the loop for this species
    }
    if (!inherits(raster_input, c("SpatRaster", "RasterLayer", "RasterStack"))) {
      message("Skipping ", myRespName.c, ": invalid raster class: ", paste(class(raster_input), collapse = ", "))
      next # <--- SKIPS the rest of the code inside the loop for this species
    }
    
    #sample size of presences 
    nb.p <- sum(myResp.c == 1, na.rm = TRUE)
    # Check if we have at least one presence
    if (nb.p < 1) {
        message("\nSkipping", myRespName.c, "due to no presence data\n")
        next
    }

    #Format data for biomod 
    bmdat.c = biomod2::BIOMOD_FormatingData(
      resp.name = myRespName.c, #species name
      resp.var = myResp.c, #presences-absences
      resp.xy = myRespXY.c, #lat/lon
      expl.var = sp.clim.rasters.c[[myRespName.c]], #raster stack 
      PA.nb.rep = 10,
      PA.nb.absences = nb.p, # same number as presences (Barbet-Massin et al. 2012; Methods Ecol. Evol.) 
      PA.strategy = 'random' #method 
    )
    
    #get today's date -  uncomment to save 
    today = format(Sys.Date(), format = "%Y%m%d")
    ##save biomodout file
    saveRDS(bmdat.c, file = file.path(L2.em,
                                      paste0(gsub("\\.", "_",
                                                  bmdat.c@sp.name),
                                             "_bmdat_c_Phenology_", today, ".rds")))                 
    
    ## function to get PA dataset // https://www.rpubs.com/dgeorges/416446 
    d <- dplyr::bind_cols(
      longitude = bmdat.c@coord[, 1],
      latitude = bmdat.c@coord[, 2],
      status = bmdat.c@data.species,
      bmdat.c@PA.table
    )
    #save
    write.csv(d, file = file.path(L2.em, paste0(gsub("\\.", "_", bmdat.c@sp.name), 
                                                "_bmdat_c_Phenology_PPA.csv")),
              row.names = FALSE)
    
    ## Cross validation k-fold selection
    cv.k.c <- bm_CrossValidation(bm.format = bmdat.c, #Formatted biomod data
                                 strategy = "kfold", #validation strategy - various
                                 nb.rep = 3, #number of repitions
                                 k = 5) # number of split datasets of equivalent sizes
    
    #save object 
    # save(bmdat.c, file = file.path(L2, "bmdat_h.RData"))
    pdf(file = file.path(L2.em, 
                         paste0(gsub("\\.", "_", bmdat.c@sp.name), 
                                "_bmdat_c.pdf")))
    try(plot(bmdat.c))
    try(plot(bmdat.c, calib.lines = cv.k.c))
    dev.off()
    
    
    ###########################
    ## ii. Single models
    ###########################
    #For modeling options see: https://biomodhub.github.io/biomod2/reference/ModelsTable.html
    # Model single models
    myBiomodModelOut.c <- biomod2::BIOMOD_Modeling(bm.format = bmdat.c, #formatted biomod data
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
    saveRDS(myBiomodModelOut.c, file = file.path(L2.em,
                                                 paste0(gsub("\\.", "_",
                                                             myBiomodModelOut.c@sp.name),
                                                        "_myBiomodModSM_c_", today, ".rds")))
    
    #Save outputs & plots 
    pdf(file = file.path(L2.em,
                         paste0(gsub("\\.", "_", myBiomodModelOut.c@sp.name),
                                "_myBiomodModSM_c",
                                "_output_", today, ".pdf")))
    # Represent evaluation scores & variables importance
    bm_PlotEvalMean(bm.out = myBiomodModelOut.c)
    bm_PlotEvalBoxplot(bm.out = myBiomodModelOut.c, group.by = c('algo', 'algo'))
    bm_PlotEvalBoxplot(bm.out = myBiomodModelOut.c, group.by = c('algo', 'run'))
    bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.c, group.by = c('expl.var', 'algo', 'algo'))
    bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.c, group.by = c('expl.var', 'algo', 'run'))
    # bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.c, group.by = c('algo', 'expl.var', 'run'))
    # Represent response curves
    bm_PlotResponseCurves(bm.out = myBiomodModelOut.c, 
                          models.chosen = get_built_models(myBiomodModelOut.c)[c(1:3, 12:14)], 
                          fixed.var = 'median')
    bm_PlotResponseCurves(bm.out = myBiomodModelOut.c, 
                          models.chosen = get_built_models(myBiomodModelOut.c)[c(1:3, 12:14)], 
                          fixed.var = 'min')
    #Response Curves heat map 
    bm_PlotResponseCurves(bm.out = myBiomodModelOut.c, 
                          models.chosen = get_built_models(myBiomodModelOut.c)[3],
                          fixed.var = 'median',
                          do.bivariate = TRUE)
    dev.off()
    
    #saving output to a text file 
    sink(file = file.path(L2.em, paste0(gsub("\\.", "_", myBiomodModelOut.c@sp.name),
                                        "_myBiomodModSM_c","_output_", today, ".txt")))
    print(gsub("\\.", "_", myBiomodModelOut.c@sp.name))
    # Get evaluation scores & variables importance 
    try(print(get_evaluations(myBiomodModelOut.c)))
    try(print(get_variables_importance(myBiomodModelOut.c)))
    sink() 
    
    
    ###########################
    ## iii. Pull best models 
    ###########################
    ##CALIBRATION: choose high scoring models
    modeleval<-myBiomodModelOut.c@models.evaluation
    #Create a new data frame with stuff 
    modelevaldataset<- data.frame(modeleval@val[["full.name"]], modeleval@val[["algo"]], 
                                  modeleval@val[["metric.eval"]], modeleval@val[["validation"]], 
                                  modeleval@val[["calibration"]])
    #Filter based on a TSS threshold 
    bestmodelscal <- modelevaldataset %>% 
      filter(modeleval.val...metric.eval...== "TSS") %>%  
      filter(modeleval.val...calibration... >= 0.6) # select models that had TSS over 0.6, done by Carroll et al. 
    #Plot 
    # ggplot(bestmodelscal)+
    #   geom_col(mapping = aes(x = modeleval.val...algo..., y = modeleval.val...calibration...))
    
    ##VALIDATION: choose high scoring models
    #Filter based on a TSS threshold 
    bestmodelsval <- modelevaldataset %>% 
      filter(modeleval.val...metric.eval...== "TSS") %>%
      filter(modeleval.val...validation... > 0.6) 
    #Plot 
    # ggplot(bestmodelsval)+ 
    #   geom_col(mapping = aes(x = modeleval.val...algo..., y = modeleval.val...calibration...))
    
    ##best models
    bestmods <- c(unique(bestmodelscal$modeleval.val...algo...), 
                  unique(bestmodelsval$modeleval.val...algo...))
    #calibration 
    filt.cal<- bestmodelscal %>% 
      filter(modeleval.val...algo...%in% bestmods)
    #validation
    filt.val <- bestmodelsval %>% 
      filter(modeleval.val...algo...%in% bestmods)
    #full names of best models 
    bestmodsfullnames.c = c(filt.cal$modeleval.val...full.name..., filt.val$modeleval.val...full.name...)
    
    #remove any invalid names 
    bestmodsfullnames.c <- bestmodsfullnames.c[!is.na(bestmodsfullnames.c) & bestmodsfullnames.c != ""]
    #skip if no valid models remain 
    if (length(bestmodsfullnames.c) < 1) {
      message("\nSkipping", i, "- no valid models selected after NA check\n")
      next
    }   
    
    ###########################
    ## iv. Ensemble model: Testing
    ###########################
    #Model ensemble models
    myBiomodEM.c <- try(biomod2::BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut.c, #singles model output
                                                     models.chosen = bestmodsfullnames.c, #vector of best models
                                                     em.by = 'all', #what models will be combined to ensemble
                                                     em.algo = 'EMwmean', #types of ensembles models to be computed 
                                                     metric.select = c('TSS'),
                                                     metric.eval = c('TSS', 'ROC'), #evaluation metrics to filter models
                                                     var.import = 3, #num permutationsto est var importance
                                                     EMci.alpha = 0.05, #significance level 
                                                     EMwmean.decay = 'proportional'), #relative importance of weights 
    			silent = TRUE)
    #Skip if ensemble modeling failed
    if (inherits(myBiomodEM.c, "try-error")) {
      message("Skipping ", i, ": ensemble modeling failed.")
      next
    }
    
    
    ##save biomodout file
    saveRDS(myBiomodEM.c, file = file.path(L2.em,
                                           paste0(gsub("\\.", "_", myBiomodEM.c@sp.name),
                                                  "_myBiomodEM_c_", today, ".rds")))
    
    
    #Save outputs & plots
    pdf(file = file.path(L2.em,
                         paste0(gsub("\\.", "_", myBiomodEM.c@sp.name),
                                "_myBiomodEM_c", "_output_", today, ".pdf")))
    # Represent evaluation scores & variables importance
    bm_PlotEvalMean(bm.out = myBiomodEM.c, group.by = 'full.name')
    bm_PlotEvalBoxplot(bm.out = myBiomodEM.c, group.by = c('full.name', 'full.name'))
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM.c, group.by = c('expl.var', 'full.name', 'full.name'))
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM.c, group.by = c('expl.var', 'algo', 'merged.by.run'))
    bm_PlotVarImpBoxplot(bm.out = myBiomodEM.c, group.by = c('algo', 'expl.var', 'merged.by.run'))
    #Response cirves heat map
    bm_PlotResponseCurves(bm.out = myBiomodEM.c,
                          models.chosen = get_built_models(myBiomodEM.c),
                          fixed.var = 'median',
                          do.bivariate = TRUE)
    # Represent response curves
    bm_PlotResponseCurves(bm.out = myBiomodEM.c,
                          models.chosen = get_built_models(myBiomodEM.c),
                          fixed.var = 'median')
    bm_PlotResponseCurves(bm.out = myBiomodEM.c,
                          models.chosen = get_built_models(myBiomodEM.c),
                          fixed.var = 'min')
    dev.off()
    
    #saving output to a text file 
    sink(file = file.path(L2.em, paste0(gsub("\\.", "_", myBiomodEM.c@sp.name),
                                        "_myBiomodEM_c", "_output_", today, ".txt")))
    print(gsub("\\.", "_", myBiomodEM.c@sp.name))
    #Get evaluation scores & variables importance - can save these as csv
    try(print(get_evaluations(myBiomodEM.c)))
    try(print(get_variables_importance(myBiomodEM.c)))
    #Extract kept models in the ensemble
    try(print(get_kept_models(myBiomodEM.c)))
    sink()                    
    
    
    ###########################
    ## v. Ensemble Projections: Final  
    ###########################
    # Projecting across space using initial ensemble outputs and pre-moeling data. 
    mod = paste0(myBiomodEM.c@sp.name, "_EMwmeanByTSS_mergedData_mergedRun_mergedAlgo")
    
    # project species - will pick best one
    sp_projection.c <- try(biomod2::BIOMOD_EnsembleForecasting(myBiomodEM.c, #output from ensemble 
                                                           proj.name = myRespName.c, #species name
                                                           new.env = sp.clim.rasters.c[[myRespName.c]], #enviro matrix
                                                           new.env.xy = myRespXY.c,
                                                           models.chosen = mod),
			silent = TRUE)
    #Skip if ensemble modeling failed
    if (inherits(sp_projection.c, "try-error")) {
      message("Skipping ", i, ": ensemble modeling failed.")
      next
    }
    
    ##save projection file
    saveRDS(sp_projection.c, file = file.path(L2.em,
                                              paste0(gsub("\\.", "_", sp_projection.c@sp.name),
                                                     "_EMprojection_c_", today, ".rds")))
    
    print("Projection output file link:")
    print(sp_projection.c@proj.out@link)
    # #read raster output
    r = rast(sp_projection.c@proj.out@link)
    #write ouput with new name -- will be overwritten by the current timeperiod model since the species share the same name
    writeRaster(r,
                file = file.path(L2.em, paste0(gsub("\\.", "_", sp_projection.c@models.projected),
                                               "_EMproj_C_Phenology.tif")), overwrite = TRUE)
    
    
    # #convert raw data into spatvector points
    d.v = vect(d, geom = c("longitude", "latitude"), keep = TRUE, crs= crs(r))
    #extract raster information using the raw data
    r.d = extract(r, d.v, bind = TRUE)
    #convert back into data frame
    df.r = as.data.frame(r.d)
    #save
    write.csv(df.r, file = file.path(L2.em, paste0(gsub("\\.", "_", sp_projection.c@models.projected),
                                                   "_EMproj_c_Phenology_PPA.csv")),
              row.names = FALSE)
    
    
    
    # Optional: Adding a small delay to avoid overwhelming the server
    Sys.sleep(1) # 1 second delay
    
  }#END of for loop 
  
})


