**ModelImportance**: This folder contains the script for testing  ensemble model importance for species with sufficient true-absences (**ModImport_Phe.R**) and ensembled models for different species, using a for loop for random pseudo-absences (ModImport_phe_rand.R) for phenology data only.

**1. 02a_Modeling_Phenology.R**: This script creates ensemble models for the species with sufficient true-absence.

**2. 02b_Modeling_Phenology_Random_c.R**: This script creates ensembled species phenology models for different species for the current time period, using a for loop and for the random pseudo-absences. 

**3. 02b_Modeling_Phenology_Random_h.R**: This script creates ensembled species phenology models for different species for the historic time period, using a for loop for the random pseudo-absences. 

**4. 03_Modeling_Range_Random_c.R**: This script creates ensembled species range models for different species for the current time period, using a for loop with random pseudo-absences.

**5. 03_Modeling_Range_Random_h.R**: This script creates ensembled species range models for different species for the historic time period, using a for loop with random pseudo-absences.

**6. 04_SDM_Uncertainty_Rasters.R**: This script is used to extract the CV and CI rasters from all ensemble models. 

**05a_Calc_Differences_Phenology.R**: This script finds the best threshold for each species and time period to binarize the raster SDM outputs and compute the difference between time periods for phenology data.

**05b_Calc_Differences_Range.R**: This script finds the best threshold for each species and time period to binarize the raster SDM outputs and compute the difference between time periods for range data. This script also bases its working directories on the supercomputing directories. 
