# Modeling Phenology 
#Author: Lizbeth Amador 
#This script creates ensemble models for the species with sufficient true-absences  

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

#earth - ensemble modeling - For MARS model
if (!requireNamespace("earth", quietly = TRUE)) {
  install.packages("earth")
}
require(earth)

#randomForest - ensemble modeling - For RF model
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
require(randomForest)

#dismo - ensemble modeling
if (!requireNamespace("dismo", quietly = TRUE)) {
  install.packages("dismo")
}
require(dismo)

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


#Directories
## Data origin 
getwd() #curious what it looks like 
#ensemble folder for phenology - output folder
#dir.create("Phenology") #create directory 
L2.em = file.path(getwd(), "Phenology")

#ensure the directory exists!
if (!dir.exists(L2.em)) {
  dir.create(L2.em, recursive = TRUE)
message("Created directory at: ", L2.em)
} else {
  message("Using existing directory at: ", L2.em)
}




#Data
#load biomod objects 
load("phenology_timeperiods_thinned_cleaned_focalspecies.RData", verbose = TRUE)

#list of species with enough true absence observations (n>=10)
sp0 = read.csv(file = "Focal_Species_List_AbsenceThreshold_TimeComparisions.csv", header = TRUE)
#pull species list 
sp0 = unique(sp0$species)
#remove columns that are in the species list -- want to test those with  insufficient true absence observations
phe.h = cbind(phe.h[, 1:2], phe.h[, sp0])
phe.c = cbind(phe.c[, 1:2], phe.c[, sp0])

# Group rasters
#1. Save rasters into a list
#^: starts with, .*:  matches any characters, including spaces, after the underscore, /\.tif$: must end with .tif
tif_files_h = list.files(file.path(getwd()),
                         pattern = "^ClimRastS_H_cropped_.*\\.tif$", full.names = TRUE)
tif_files_c = list.files(file.path(getwd()),
                         pattern = "^ClimRastS_C_cropped_.*\\.tif$", full.names = TRUE)
## 2. Read each raster as a SpatRaster and store in a list
rasters_h <- lapply(tif_files_h, rast)
rasters_c <- lapply(tif_files_c, rast)
# 3. Extract species names from file names (e.g., from "ClimRastS_H_cropped_Acer glabrum.tif")
species_names <- tools::file_path_sans_ext(basename(tif_files_h))
species_names <- gsub("ClimRastS_H_cropped_", "", species_names)  # adjust to your naming
species_names <- gsub("_" , " ", species_names)
# 4. Load rasters into a named list - Ensure proj 
sp.clim.rasters.h <- setNames(lapply(tif_files_h, rast), species_names)
sp.clim.rasters.c <- setNames(lapply(tif_files_c, rast), species_names)

#set working directory to where you want biomod outputs to save
setwd("Phenology") 


#for loop preset - species list 
# sp = names(phe.h[3:length(phe.h)])
sp = names(phe.h[3:length(phe.h)]) #testing


##################################
# Historic  
##################################
system.time({
  for(i in sp) { #START of foreach loop
                                  
          
          ###########################
          ## i. `biomod2` formatting 
          ###########################
          #Select species name
          myRespName.h = i
          # Get corresponding P/A data
          myResp.h = phe.h[, myRespName.h]
          #Get corresponding coordinates
          myRespXY.h = as.data.frame(phe.h[, c('longitude', 'latitude')]) #Make sure long goes first!
        
  
          #Message to keep track of species being processed 
	  message(paste("Processing", i, "..."))
	  #Skip species with missing raster data
	  raster_input <- sp.clim.rasters.h[[myRespName.h]]
	  if (is.null(raster_input)) {
	    message("Skipping ", myRespName.h, ": no raster found.")
	    next # <--- SKIPS the rest of the code inside the loop for this species
	  }
	  if (!inherits(raster_input, c("SpatRaster", "RasterLayer", "RasterStack"))) {
	    message("Skipping ", myRespName.h, ": invalid raster class: ", paste(class(raster_input), collapse = ", "))
	    next # <--- SKIPS the rest of the code inside the loop for this species
	  }


       	 #Ensure there is a crs proj
       	 if (is.na(crs(sp.clim.rasters.h[[myRespName.h]])) || crs(sp.clim.rasters.h[[myRespName.h]]) == "") {
	  message(paste("No usable CRS detected for", myRespName.h, "- assigning EPSG:4326"))
	  crs(sp.clim.rasters.h[[myRespName.h]]) <- "EPSG:4326"
	} else {
	  message(paste("CRS already set for", myRespName.h, ":", crs(sp.clim.rasters.h[[myRespName.h]])))
	}


	
          #Format data for biomod 
          bmdat.h = biomod2::BIOMOD_FormatingData(
            resp.name = myRespName.h, #species name
            resp.var = myResp.h, #presences-absences
            resp.xy = myRespXY.h, #lat/lon
            expl.var = sp.clim.rasters.h[[myRespName.h]] #raster stack 
          )
          
          
          ## Cross validation k-fold selection
          cv.k.h <- biomod2::bm_CrossValidation(bm.format = bmdat.h, #Formatted biomod data
                                                strategy = "kfold", #validation strategy - various
                                                nb.rep = 3, #number of repitions
                                                k = 5) # number of split datasets of equivalent sizes
          
          #save object 
          # save(bmdat.h, file = file.path(L2, "bmdat_h.RData"))
          pdf(file = file.path(L2.em, 
                               paste0(gsub("\\.", "_", bmdat.h@sp.name), 
                                      "_bmdat_h.pdf")))
          try(plot(bmdat.h))
          try(plot(bmdat.h, calib.lines = cv.k.h))
          dev.off()
          
          
          ###########################
          ## ii. Single models
          ###########################
          #For modeling options see: https://biomodhub.github.io/biomod2/reference/ModelsTable.html
          mods = c("CTA", "GBM", "GLM", "MARS", "RF", "SRE")
          # Model single models
          myBiomodModelOut.h <- biomod2::BIOMOD_Modeling(bm.format = bmdat.h, #formatted biomod data
                                                         models = mods, 
                                                         CV.strategy = 'kfold', #cross-validation strategy
                                                         CV.nb.rep = 3, #cross-val repititions
                                                         CV.k = 5, #cross-val partitions
                                                         OPT.strategy = 'bigboss', #model param selection
                                                         var.import = 3, #number of permutations to est var importance  
                                                         seed.val = 1, #to keep same results when rerunning
                                                         # nb.cpu = 8), #computing resources
                                                         metric.eval = c('TSS','ROC')) #evaluation metrics 
          
          
          
          #get today's date -  uncomment to save 
          today = format(Sys.Date(), format = "%Y%m%d")
          ##save biomodout file
          saveRDS(myBiomodModelOut.h, file = file.path(L2.em,
                                                       paste0(gsub("\\.", "_",
                                                                   myBiomodModelOut.h@sp.name),
                                                              "_myBiomodModSM_h_", today, ".rds")))
          
          #Save outputs & plots 
          pdf(file = file.path(L2.em,
                               paste0(gsub("\\.", "_", myBiomodModelOut.h@sp.name),
                                      "_myBiomodModSM_h",
                                      "_output_", today, ".pdf")))
          # Represent evaluation scores & variables importance
          bm_PlotEvalMean(bm.out = myBiomodModelOut.h)
          bm_PlotEvalBoxplot(bm.out = myBiomodModelOut.h, group.by = c('algo', 'algo'))
          bm_PlotEvalBoxplot(bm.out = myBiomodModelOut.h, group.by = c('algo', 'run'))
          bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.h, group.by = c('expl.var', 'algo', 'algo'))
          bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.h, group.by = c('expl.var', 'algo', 'run'))
          bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.h, group.by = c('algo', 'expl.var', 'run'))
          # Represent response curves
          bm_PlotResponseCurves(bm.out = myBiomodModelOut.h, 
                                models.chosen = get_built_models(myBiomodModelOut.h)[c(1:3, 12:14)], 
                                fixed.var = 'median')
          bm_PlotResponseCurves(bm.out = myBiomodModelOut.h, 
                                models.chosen = get_built_models(myBiomodModelOut.h)[c(1:3, 12:14)], 
                                fixed.var = 'min')
          #Response Curves heat map 
          bm_PlotResponseCurves(bm.out = myBiomodModelOut.h, 
                                models.chosen = get_built_models(myBiomodModelOut.h)[3],
                                fixed.var = 'median',
                                do.bivariate = TRUE)
          dev.off()
          
          #saving output to a text file 
          sink(file = file.path(L2.em, paste0(gsub("\\.", "_", myBiomodModelOut.h@sp.name),
                                              "_myBiomodModSM_h","_output_", today, ".txt")))
          print(gsub("\\.", "_", myBiomodModelOut.h@sp.name))
          # Get evaluation scores & variables importance 
          try(print(get_evaluations(myBiomodModelOut.h)))
          try(print(get_variables_importance(myBiomodModelOut.h)))
          sink() 
          
          
          
          ###########################
          ## iii. Pull best models 
          ###########################
          ##CALIBRATION: choose high scoring models
          modeleval<-myBiomodModelOut.h@models.evaluation
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
          bestmodsfullnames.h = c(filt.cal$modeleval.val...full.name..., filt.val$modeleval.val...full.name...)
          
          
          # Optional: Adding a small delay to avoid overwhelming the server
          Sys.sleep(1) # 1 second delay
          
        }#END of for loop 
  
})





#for loop preset - species list 
# sp = names(phe.c[3:length(phe.c)])
sp = names(phe.h[3:length(phe.h)]) #testing

##################################
# Current  
##################################
system.time(
  foreach(i = sp, .packages = c("biomod2", "terra", "tools", 
                                "randomForest", "terra", 
                                "tidyverse")) %do% { #START of foreach loop
                
          ###########################
          ## i. `biomod2` formatting 
          ###########################
          #Select species name
          myRespName.c = i
          # Get corresponding P/A data
          myResp.c = phe.c[, myRespName.c]
          #Get corresponding coordinates
          myRespXY.c = as.data.frame(phe.c[, c('longitude', 'latitude')]) #Make sure long goes first!
          
	
	  #Message to keep track of species being processed 
	  message(paste("Processing", i, "..."))
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
	
    
	  #Ensure there is a crs proj
          raster_input <- sp.clim.rasters.c[[myRespName.c]]
          if (is.na(crs(raster_input)) || crs(raster_input) == "") {
            message(paste("No usable CRS detected for", myRespName.c, "- assigning EPSG:4326"))
            crs(raster_input) <- "EPSG:4326"
            sp.clim.rasters.c[[myRespName.c]] <- raster_input
          } else {
            message(paste("CRS already set for", myRespName.c, ":", crs(raster_input)))
          }

          
          #Format data for biomod 
          bmdat.c = biomod2::BIOMOD_FormatingData(
            resp.name = myRespName.c, #species name
            resp.var = myResp.c, #presences-absences
            resp.xy = myRespXY.c, #lat/lon
            expl.var = sp.clim.rasters.c[[myRespName.c]] #raster stack 
          )
          
          
          ## Cross validation k-fold selection
          cv.k.c <- biomod2::bm_CrossValidation(bm.format = bmdat.c, #Formatted biomod data
                                                strategy = "kfold", #validation strategy - various
                                                nb.rep = 2, #number of repitions
                                                k = 5) # number of split datasets of equivalent sizes
          
          #save object 
          # save(bmdat.c, file = file.path(L2, "bmdat_c.RData"))
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
          mods = c("CTA", "GBM", "GLM", "MARS", "RF", "SRE")
          # Model single models
          myBiomodModelOut.c <- biomod2::BIOMOD_Modeling(bm.format = bmdat.c, #formatted biomod data
                                                         models = mods, #for simulation set
                                                         CV.strategy = 'kfold', #cross-validation strategy
                                                         CV.nb.rep = 2, #cross-val repititions
                                                         CV.k = 5, #cross-val partitions
                                                         OPT.strategy = 'bigboss', #model param selection
                                                         var.import = 3, #number of permutations to est var importance  
                                                         seed.val = 1, #to keep same results when rerunning
                                                         # nb.cpu = 8), #computing resources
                                                         metric.eval = c('TSS','ROC')) #evaluation metrics 
          
          
          
          #get today's date -  uncomment to save 
          today = format(Sys.Date(), format = "%Y%m%d")
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
          bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut.c, group.by = c('algo', 'expl.var', 'run'))
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
          
          
          # Optional: Adding a small delay to avoid overwhelming the server
          Sys.sleep(1) # 1 second delay
          
        }#END of for loop 
  
)










