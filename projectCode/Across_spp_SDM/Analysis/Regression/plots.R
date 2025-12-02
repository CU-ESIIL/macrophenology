#Model outputs
#Author: Lizbeth G Amador 


#This script plots brms outputs 

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

if (!requireNamespace("bayesplot", quietly = TRUE)) {
  install.packages("bayesplot")
}; library(bayesplot)

if (!requireNamespace("tidybayes", quietly = TRUE)) {
  install.packages("tidybayes")
}; library(tidybayes)

if (!requireNamespace("marginaleffects", quietly = TRUE)) {
  install.packages("marginaleffects")
}; library(marginaleffects)

if (!requireNamespace("collapse", quietly = TRUE)) {
  install.packages("collapse")
}; library(collapse)

if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}; library(parallel)


##############
# Directories
##############
mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
graphs = file.path(L2, "Graphs")

if (!dir.exists(graphs)) {
  dir.create(graphs, recursive = TRUE)
  message("Created directory at: ", graphs)
} else {
  message("Using existing directory at: ", graphs)
}


#########
# Data
#########
# load(file = file.path("/users/PUOM0017/lamad/esiil_macrophenology/Data/L2/Subset_Data_ModelData_NoOutliers.RData"), verbose = TRUE)
load(file = file.path(L2, "Subset_ModelData_NoOutliers.RData"), verbose = TRUE)


#function to plot both training & testing models
#type: nativity or nah
#group: training or testing 
#run: r number
fit.plot <- function(run, group, type){
  message("Plotting!")
  #backing up name
  fit = readRDS(file = file.path(L2, paste0("brms_fit_Run", run, "_", group, "_", type, ".rds")))
  #sample for re-predictions 
  n = 10
  
  ####################
  # 1. Model Convergence
  ####################
  pdf(file = file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type, "_Convergence.pdf")))
  #plot dist
  p = plot(fit)
  p
  # Posterior means + intervals
  p = mcmc_plot(fit, type = "areas", prob = 0.95)
  p
  dev.off()
  message("1) Plotted & saved brms convergence plots!")
  
  
  #############
  # 2. Box plots
  #############
  #relationships with pdiff
  #pdiff ~ nativity
  p = plot(conditional_effects(fit, effects = "nativity"))[[1]]
  p + theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          # axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type, "_Boxplots_pdiff_nativity.png")), last_plot(),
         width = 18, height = 11, units = "in", dpi = 600)
  
  #pdiff ~ dispersal
  p = plot(conditional_effects(fit, effects = "dispersal"))[[1]]
  p + theme_minimal() +
    theme(
      plot.background = element_rect(fill = "gray99", color = NA),
      axis.text = element_text(size = 22),
      axis.text.x = element_text(angle = 45, hjust = .9, vjust = .9),
      axis.title = element_text(size = 22),
      plot.title = element_text(size = 22, hjust = 0.5),
      legend.text = element_text(size = 22),
      legend.title = element_text(size = 22)
    )
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_Boxplots_pdiff_dispersal.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600)
  
  message("2) Plotted & saved brms boxplots plots!")
  
  
  ########################### 
  # 3. Coefficient plot 
  ###########################
  #version 1
  mcmc_plot(fit, type = "intervals", prob = 0.95, point_size = 5, # make points larger 
            point_color = "black", size = 1.5) + # make interval lines thicker 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
    theme(text = element_text(size = 22))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_Coefficients_V1_poster.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600)
  
  
  #Extract and tidy posterior summaries
  coef_df <- as.data.frame(posterior_summary(fit))
  coef_df$Term <- rownames(coef_df)
  #Filter only fixed effects
  fixef_df <- coef_df %>%
    filter(grepl("^b_", Term)) %>%
    mutate(
      #Term = gsub("^b_", "", Term),
      Sig = ifelse(`Q2.5` > 0 | `Q97.5` < 0, "Significant", "Not Significant"))
  #Plot with ggplot
  ggplot(fixef_df, aes(x = reorder(Term, Estimate), y = Estimate, color = Sig)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.7) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = `Q2.5`, ymax = `Q97.5`), width = 0.5) +
    coord_flip() +
    labs(x = "", y = "Estimate", title = "") +
    scale_color_manual(values = c("Significant" = "#40B0A6", "Not Significant" = "gray50")) +
    scale_x_discrete(labels = c(
      "b_Intercept" = "Intercept",
      "b_rdiff" = "Change in Range",
      "b_nativityI" = "nativity(Invasive)",
      "b_latitude" = "Latitude",
      "b_rdiff:nativityI" = "Change in Range × nativity(Invasive)",
      "b_rdiff:latitude" = "Change in Range × Latitude",
      "b_functional_typeEvergreenbroadleaf" = "Evergreen Broadleaf",
      "b_functional_typeSemiMevergreenforb" = "Semi-evergreen Forb", 
      "b_functional_typeSemiMevergreenbroadleaf" = "Semi-evergreen Broadleaf",
      "b_functional_typeEvergreenforb" = "Evergreen Forb",
      "b_functional_typeForb" = "Forb",
      "b_functional_typeDroughtdeciduousbroadleaf" = "Drought-deciduous Broadleaf",
      "b_functional_typeSemievergreenforb" = "Semi-evergreen Forb",
      "b_functional_typeSemievergreenbroadleaf" = "Semi-evergreen Broadleaf")) + 
    theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = 0, hjust = .4, vjust = 1, size = 22), #est
          axis.text.y = element_text(angle = 0, hjust = .9, vjust = 0.7, size = 22), #terms 
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 1, hjust = 0.5),  # Centered
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 0),
          legend.box.background = element_rect(fill = "white", color = "grey99"),
          legend.position = c(0.75, 0.05),   # inside, nudged left & up
          legend.justification = c("left", "bottom"),
          plot.margin = margin(10, 70, 10, 10))  # add a bit of right margin so legend isn’t clipped
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_Coefficients_V2_poster.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600, limitsize=FALSE)
  
  rm(coef_df, fixef_df); gc()
  message("3) Plotted & saved coefficient plot!")
  
  
  ################################# 
  # 4) Simple regression line
  ################################# 
  # newdata_grid <- sub.data %>%
  #   select(rdiff, dispersal, latitude, nativity, species, domain_id) %>%
  #   distinct() %>%
  #   slice_sample(n = 1000)
  newdata_grid <- sub.data %>%
    select(rdiff, dispersal, latitude, nativity, species, domain_id, rsd, psd) %>%
    distinct() %>%
    slice_sample(n = 1000)
  # Match factor levels exactly
  newdata_grid$species <- factor(newdata_grid$species,
                                 levels = unique(fit$data$species))
  newdata_grid$domain_id <- factor(newdata_grid$domain_id,
                                   levels = unique(fit$data$domain_id))
  
  preds_simple <- add_predicted_draws(fit, newdata = newdata_grid, allow_new_levels = TRUE, ndraws = n)
  
  # Plot with posterior predictive intervals
  model_fit <- preds_simple %>%
    ggplot(aes(x = rdiff, y = .prediction)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 0.5, colour = "black") +
    scale_fill_brewer(palette = "Greys") +
    labs(y = "Change in Phenology\n", x = "\nChange in Range",
         color = "CI",
         fill = "CI") +
    theme_bw() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          legend.position = c(0.92, 0.1),
          legend.box.background = element_rect(fill = "white", color = "grey99"))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_LinearPlot_pdiff_rdiff_poster.png")), model_fit,
         width = 16, height = 11, units = "in", dpi = 600)
  
  rm(model_fit, newdata_grid, preds_simple); gc()
  message("4) Plotted & saved simple regression plot!")
  
  
  ##################################################  
  # 5) Interactions grouped by latitude region 
  ##################################################  
  sample_data <- sub.data %>%
    mutate(
      lat_region = case_when(
        latitude < 30 ~ "Low (<30°)",
        latitude >= 30 & latitude <= 60 ~ "Mid (30°–60°)",
        latitude > 60 ~ "High (>60°)"))
  sample_data$lat_region <- factor(sample_data$lat_region,
                                   levels = c("Low (<30°)", "Mid (30°–60°)", "High (>60°)"))
  #Add predicted values per lat_region
  preds_lat_region <- sample_data %>%
    group_by(lat_region) %>%
    add_epred_draws(fit, allow_new_levels = TRUE, ndraws = n)  #reduce ndraws if needed for speed
  #plot
  ggplot(preds_lat_region, aes(x = rdiff, y = .epred, color = lat_region, fill = lat_region)) +
    stat_lineribbon(.width = c(0.95, 0.8), alpha = 0.25) +
    geom_line(stat = "summary", fun = mean, linewidth = 3) +
    labs(x = "Range Difference (rdiff)", y = "Predicted Phenology Difference (pdiff)",
         color = "Latitude Region",
         fill = "Latitude Region") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          legend.position = c(0.92, 0.1),
          legend.box.background = element_rect(fill = "white", color = "grey99"))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type, "_Interaction_latitude_cat_poster.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600)
  
  rm(preds_lat_region); gc()
  message("5) Plotted & saved latitude interaction plot!")
  
  
  ######################################## 
  # 6) Using marginaleffects package 
  ########################################
  update_geom_defaults("line", list(linewidth = 3))
  
  #pdiff ~ rdiff*nativity
  p = plot_predictions(fit, 
                       condition = c("rdiff", "nativity"),  # x1 on x-axis, lines per x2
                       allow_new_levels = TRUE, ndraws = n)
  p + theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          legend.position = c(0.92, 0.1),
          legend.box.background = element_rect(fill = "white", color = "grey99"))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_Interaction_nativity_poster.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600)
  
  #pdiff ~ rdiff*latitude
  p = plot_predictions(fit, 
                       condition = c("rdiff", "latitude"),  # x1 on x-axis, lines per x2
                       allow_new_levels = TRUE, ndraws = n)
  p + theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          legend.position = c(0.92, 0.1),
          legend.box.background = element_rect(fill = "white", color = "grey99"))
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_Interaction_latitude_cont_poster.png")), last_plot(), 
         width = 16, height = 11, units = "in", dpi = 600)
  
  #pdiff ~ rdiff*dispersal
  p = plot_predictions(fit, 
                       condition = c("rdiff", "dispersal"),  # x1 on x-axis, lines per x2
                       allow_new_levels = TRUE, ndraws = n)
  p + theme_minimal() +
    theme(plot.background = element_rect(fill = "gray99", color = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1),
          axis.title = element_text(size = 22),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.text = element_text(size = 22),
          legend.title = element_text(size = 22),
          legend.position = c(0.80, 0.2),
          legend.box.background = element_rect(fill = "white", color = "grey99")) 
  
  ggsave(file.path(graphs, paste0("brms_plot_", "Run", run, "_", group, "_", type,  "_LinearPlot_dipersal_poster.png")), last_plot(),
         width = 16, height = 11, units = "in", dpi = 600)
  
  message("6) Plotted & saved marginal error plots!")
}


#run function
fit.plot(run = "1", group = "training", type = "nativity")




# # fit.plot(fitb, mod, type = "training"); fit.plot(fitt, mod, type = "testing")
# #Match SLURM allocation
# n.cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", 2))  
# #apply function -- in parallel 
# mclapply(
#   list(
#     list(fitb, "training"),
#     list(fitt, "testing")),
#   function(args) fit.plot(args[[1]], mod, type = args[[2]], run),
# mc.cores = n.cores #number of cores requested 
#   )





