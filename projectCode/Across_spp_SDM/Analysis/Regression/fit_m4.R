# Linear regression 
#Author: Lizbeth G Amador 

#This script takes the difference data of phenology and range and fits it to a 
#linear regression with random effects of species and space.

run ="1"
##########
# Presets 
##########
# Load necessary libraries, installing if missing
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")}; library(tidyverse)

if (!requireNamespace("brms", quietly = TRUE)) {
  install.packages("brms")}; library(brms)

if (!requireNamespace("Metrics", quietly = TRUE)) {
  install.packages("Metrics")}; library(Metrics)


#Set Stan cores to max
options(mc.cores = parallel::detectCores())
cat("Using", options("mc.cores")[[1]], "cores for Stan parallelization.\n")


##############
# Directories
##############
graphs <- "/users/PUOM0017/lamad/esiil_macrophenology/analysis/4-regression/Graphs"
getwd()  # Print current working directory
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
# load(file = file.path("/users/PUOM0017/lamad/esiil_macrophenology/Data/L2/Subset_Data_ModelData_NoOutliers.RData"), verbose = TRUE)
load(file = file.path("Subset_Data_ModelData_NoOutliers.RData"), verbose = TRUE)

#Important to read as a matrix!
# dom.mat <- as.matrix(read.csv(file.path("/users/PUOM0017/lamad/esiil_macrophenology/Data/L2/neon_adjacency_matrix.csv"),header = TRUE, row.names = 1, check.names = FALSE))
dom.mat <- as.matrix(read.csv(file.path("neon_adjacency_matrix.csv"),header = TRUE, row.names = 1, check.names = FALSE))
#making sure it's numeric
mode(dom.mat) <- "numeric"

######################
# Model calibration 
######################
#training & testing groups -- for model validation    
set.seed(run)   # Set a seed value for reproducability purposes in this document
# Randomly select 70% of the data (rows)
index <- sample(1:nrow(sub.data), size=floor(.70*nrow(sub.data)))
train <- sub.data %>% 
  filter(row_number() %in% index)
test  <- sub.data %>% 
  filter(!row_number() %in% index)

#save space
rm(sub.data); gc()

##############
# Assumptions
##############
#Fit linear model
mod <- lm(pdiff ~ rdiff + latitude + status + functional_type +
           rdiff*latitude + rdiff*status, data = train)
pdf(file = file.path(graphs, paste0("Mod4", "_R", run, "_Linear_Assumptions_lm_R1.pdf")))
#1. Linearity & Homoscedasticity
par(mfrow = c(2,2))
plot(mod)
#2. Normality of residuals
#Histogram
hist(resid(mod), breaks = 100, main = "Histogram of Residuals", xlab = "Residuals")
#3. Q-Q plot
qqnorm(resid(mod))
qqline(resid(mod), col = "red")
par(mfrow = c(1,1))
dev.off()

rm(mod); gc()

######################
# Model selection 
######################
message("Model Selection/Assumptions for M4 same as M3 ...")

best.mod = "pdiff ~ rdiff + latitude + status + functional_type +
                 rdiff*latitude + rdiff*status + 
                 (1|species) + car(dom.mat, gr = domain_id)"

#################
# brms modeling 
#################
system.time({ #START timer
  m4 <- best.mod

  message("Modeling: ", m4)
  
  fitb.4 <- brm(
    formula = m4,
    family = gaussian(),
    data = train,
    data2 = list(dom.mat=dom.mat), #adjacency matrix
    chains = 4,     #parallel chains
    cores = 4,      #ideally equals the number of chains
    threads = threading(28),  #threads per chain (th/ch); thxch for core number  
    iter = 2500, #Iterations
    warmup = 1500, #the burn or warmup -- when predictions are recorded
    seed = 123
  )
  
  
  message("Modeling complete! ... Saving model")
  # Save the model object
  saveRDS(fitb.4, file = file.path(graphs, paste0("fitb_4_training_R", run, ".rds")))
  
  #predict testing data using the model 
  fitb.t <- predict(fitb.4, newdata=test)
  saveRDS(fitb.t, file = file.path(graphs, paste0("fitb_4_testing_R", run, ".rds")))

})#END timer 

###############
# Model Output
###############
message("Saving model output")
# Save model summary output
sink(file = file.path(graphs, paste0("Mod4_brms_Summary_Output-R", run, ".txt")))
cat("Training\n")
cat("Summary:\n")
print(summary(fitb.4))
cat("\nPrior summary:\n")
print(prior_summary(fitb.4))

cat("Testing\n")
cat("Summary:\n")
print(summary(fitb.t))


#Posterior Mean predictions
#Posterior expected value per observation
train_fitted <- fitted(fitb.4)  # matrix with Estimate, Est.Error, Q2.5, Q97.5
# Posterior mean predictions
train_mean <- train_fitted[, "Estimate"]
test_mean <- fitb.t[, "Estimate"]  # if predict() returns a matrix with Estimate

cat("\nRMSE\n")
rmse_train <- sqrt(mean((train$pdiff - train_mean)^2))
rmse_test <- sqrt(mean((test$pdiff - test_mean)^2))
cat("training: ", round(rmse_train, 4))
cat("\ntesting: ", round(rmse_test, 4))

cat("\nR^2\n")
r2_train <- cor(train$pdiff, train_mean)^2
r2_test  <- cor(test$pdiff, test_mean)^2
cat("training: ", round(r2_train,4))
cat("\ntesting: ", round(r2_test,4))

sink()


# 
# fitb.t = readRDS("/users/PUOM0017/lamad/esiil_macrophenology/analysis/4-regression/Graphs/fitb_4_testing.rds")
# fitb.4 = readRDS("/users/PUOM0017/lamad/esiil_macrophenology/analysis/4-regression/Graphs/fitb_4_training.rds")
