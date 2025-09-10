#table making 
#Author: Lizbeth G Amador 

#This script outputs tables from model outputs 

#model type and run # settings 
m = "4"
run = "4"

###########
# Presets
###########
# Load necessary libraries, installing if missing
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}; library(tidyverse)

if (!requireNamespace("brms", quietly = TRUE)) {
  install.packages("brms")
}; library(brms)

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}; library(stringr)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}; library(readr)   # for CSV export


if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}; library(openxlsx)   # for xlsx export

##############
# Directories
##############
getwd()  # Print current working directory
# graphs <- "/users/PUOM0017/lamad/esiil_macrophenology/analysis/4-regression/Graphs"
graphs <- file.path(getwd(), "Graphs")

if (!dir.exists(graphs)) {
  dir.create(graphs, recursive = TRUE)
  message("Created directory at: ", graphs)
} else {
  message("Using existing directory at: ", graphs)
}

#########
# Data
#########
fit = readRDS(file = paste0("fitb_", m, "_training_R", run, ".rds"))

####################
#Est table function
####################
save_key_params <- function(fit, save_path) {
  #full summary
  summ <- as.data.frame(posterior_summary(fit))
  summ$Parameter <- rownames(summ)
  
  #diagnostics
  rhat_vals <- round(rhat(fit), 2)
  bulk <- round(neff_ratio(fit, type = "bulk"), 2)
  tail <- round(neff_ratio(fit, type = "tail"), 2)
  ndraws_total <- round(posterior::ndraws(as_draws_array(fit)), 2)
  
  summ$Rhat <- round((rhat_vals[summ$Parameter]), 2)
  summ$Bulk_ESS <- round((bulk[summ$Parameter] * ndraws_total), 2)
  summ$Tail_ESS <- round((tail[summ$Parameter] * ndraws_total), 2)
  
  #keep only CAR, species RE, and coefficients
  keep <- grepl("^car$|^sdcar$|^sd_species__Intercept$|^b_", summ$Parameter)
  out <- summ[keep, c("Parameter","Estimate","Est.Error","Q2.5","Q97.5",
                      "Rhat","Bulk_ESS","Tail_ESS")]
  
  #clean names
  out$Parameter <- out$Parameter %>%
    str_replace("^b_", "") %>%
    str_replace("^sd_species__Intercept$", "Species (SD)") %>%
    str_replace("^sdcar$", "CAR (SD)") %>%
    str_replace("^car$", "CAR correlation")
  
  rownames(out) <- NULL
  
  # export based on file extension
  ext <- tools::file_ext(save_path)
  if (ext == "csv") {
    write_csv(out, save_path)
  } else if (ext %in% c("xlsx","xls")) {
    openxlsx::write.xlsx(out, save_path)
  } else if (ext == "tsv") {
    write_tsv(out, save_path)
  } else {
    stop("Unsupported file type: use .csv, .xlsx, or .tsv")
  }
  
  message("Table saved to: ", save_path)
  invisible(NULL)
}

# Example usage
save_key_params(fit,
                     save_path = file.path(graphs, paste0("Mod", m, "_R", run, "_param_summary.csv")))
  

