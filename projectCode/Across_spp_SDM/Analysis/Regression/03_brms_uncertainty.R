## BRMS linear regression with uncertainty
## Authors: Tong Qiu, Lizbeth G Amador


#Libraries
suppressPackageStartupMessages({
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse"); library(tidyverse)
  if (!requireNamespace("brms", quietly = TRUE)) install.packages("brms"); library(brms)
  if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret"); library(caret)
})

#########
run = 1
#########
#######
# Data
#######
#Working directories
mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
graphs = file.path(L2, "Graphs")

#Model data
load(file = file.path(L2, "Subset_ModelData_NoOutliers.RData"), verbose = TRUE) # variables of importance: pdiff, psd, rdiff, rsd, domain_id, funct_tpe, nativity, dispersal
dim(sub.data)
#NEON Domain adjaceny table -- Important to read as a matrix!
dom.mat <- as.matrix(read.csv(file.path(L2, "neon_adjacency_matrix.csv"), header = TRUE, row.names = 1, check.names = FALSE))
#Making sure it's numeric
mode(dom.mat) <- "numeric"


######################################
# Model calibration/validation groups
######################################
message("1) Create calibration and validation groups")
#Dummy data for testing code [remove slicing for actual runs]
# sub.data <- sub.data %>%
#   slice_sample(n = 1000)   # keep exactly n rows
#Training & testing groups -- for model validation
set.seed(run)   # Set a seed value for reproducability purposes in this document
#Randomly select 70% of the data (rows)
index <- sample(1:nrow(sub.data), size=floor(.70*nrow(sub.data)))
#Training group (initial model)
train.dat <- sub.data[index, ]
#Testing group
test.dat  <- sub.data[-index, ]
dim(train.dat); dim(test.dat)
#save space
rm(sub.data)
#Training data without woody species (for second model)
train.dat1 = train.dat[train.dat$funct_type == "non-woody", ]
test.dat1 = test.dat[test.dat$funct_type == "non-woody", ]


#####################
# Linear assumptions
#####################
#message("Exporting linear assumptions")
#Model with functional type (No nativity)
#pdf(file = file.path(L2, "Graphs", paste0("brms_fit_LinearAssumptions_Run", run, "_funct_type.pdf")))
#mod <- lm(pdiff ~ rdiff + latitude + funct_type + dispersal + rdiff*latitude, data = train.dat)
#1. Linearity & Homoscedasticity
#par(mfrow = c(2,2))
#plot(mod)
#2. Normality of residuals
#hist(resid(mod), breaks = 100, main = "Histogram of Residuals", xlab = "Residuals")
#3. Q-Q plots
#qqnorm(resid(mod)); qqline(resid(mod), col = "red")
#dev.off()

#Model with nativity (No functional type)
pdf(file = file.path(L2, paste0("brms_fit_LinearAssumptions_Run", run, "_nativity.pdf")))
mod <- lm(pdiff ~ rdiff + latitude + nativity + dispersal + rdiff*latitude, data = train.dat1)
#1. Linearity & Homoscedasticity
par(mfrow = c(2,2))
plot(mod)
#2. Normality of residuals
hist(resid(mod), breaks = 100, main = "Histogram of Residuals", xlab = "Residuals")
#3. Q-Q plot
qqnorm(resid(mod)); qqline(resid(mod), col = "red")
dev.off()
#save space
rm(mod); gc()


x.time = system.time({ #START timer
  ###################
  ## Fit BRMS model:
  ###################
  ##   - Gaussian identity
  ##   - Response measurement error via  | se(phen_sd (pdiff), sigma = TRUE)
  ##   - Predictor measurement error via me(range_mean (rdiff), range_sd (rsd))
  ##   - Random intercept by species (can easily change to site or other factors)
  # message("2) Begin first brms model -- All woody & non-woody, w/o nativity")
  # #----------- All woody & non-woody (w/o nativity status) -----------
  # #Not including nativity status since only non-woody functional types are invasive
  # #Formua
  # form <- bf(
  #   pdiff | se(psd, sigma = TRUE) ~
  #     me(rdiff, rsd) + latitude + funct_type + dispersal + rdiff*latitude +
  #     (1 | species) + #random effect
  #     car(dom.mat, gr = domain_id)) #adjacency matrix
  # message(form)
  # #Setting priors
  # priors <- c(
  #   set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  #   set_prior("normal(0, 1)",         class = "b"),      # includes slope for me()
  #   set_prior("exponential(1)",       class = "sd"),     # RE SD
  #   set_prior("student_t(3, 0, 2.5)", class = "sigma")   # residual SD (beyond phen_sd)
  # )
  # #Fitting model
  # fit <- brm(
  #   formula = form,
  #   data    = train.dat,
  #   data2 = list(dom.mat=dom.mat), # adjacency matrix data
  #   family  = gaussian(),           # identity link on [-1, 1]
  #   prior   = priors,
  #   chains  = 4, iter = 3000, warmup = 1000, cores = 4,
  #   #threads = threading(16),
  #   seed    = 20250909,
  #   backend = "cmdstanr", #to avoid TBB error in Cyverse HPC environment
  #   control = list(adapt_delta = 0.95, max_treedepth = 13))
  # message("Modeling complete! ... Saving model fit")
  # saveRDS(fit, file = file.path(L2, paste0("brms_fit_training_Run", run, "_funct_type.rds")))
  # message("Predicting using the testing dataset")
  # #predict testing data using the training model
  # fit.t <- predict(fit, newdata = test.dat)
  # saveRDS(fit.t, file = file.path(L2, paste0("brms_fit_testing_Run", run, "_funct_type.rds")))
  #
  #
  # #--- save output ---
  # message("saving output")
  # sink(file.path(graphs, paste0("brms_fit_Run", run, "_funct_type_output.txt")))
  # cat("Training\n"); cat("Summary:\n")
  # print(summary(fit))
  # cat("\nPrior summary:\n")
  # print(prior_summary(fit))
  # cat("\nLook for the coefficient named b_me... — that's the slope for the latent RANGE.\n")
  # print(fit)
  # cat("\nTesting\n"); cat("Summary:\n")
  # print(summary(fit.t))
  # print(fit.t)
  # #Posterior Mean predictions
  # #Posterior expected value per observation
  # train_fitted <- fitted(fit)  # matrix with Estimate, Est.Error, Q2.5, Q97.5
  # # Posterior mean predictions
  # train_mean <- train_fitted[, "Estimate"]; test_mean <- fit.t[, "Estimate"]  # if predict() returns a matrix with Estimate
  # cat("\nRMSE\n")
  # rmse_train <- sqrt(mean((train.dat$pdiff - train_mean)^2)); rmse_test <- sqrt(mean((test.dat$pdiff - test_mean)^2))
  # cat("training: ", round(rmse_train, 4)); cat("\ntesting: ", round(rmse_test, 4))
  # cat("\nR^2\n")
  # r2_train <- cor(train.dat$pdiff, train_mean)^2; r2_test  <- cor(test.dat$pdiff, test_mean)^2
  # cat("training: ", round(r2_train, 4)); cat("\ntesting: ", round(r2_test, 4))
  # sink()
  #-------------------
  
  
  #----------- All non-woody (w/ nativity status) -----------
  message("3) Begin second brms model -- All non-woody, w/ nativity")
  #Removing woody species (and therefor function type) from the model (only non-woody invasive spp.)
  #Formula
  form <- bf(
    pdiff | se(psd, sigma = TRUE) ~
      me(rdiff, rsd) + latitude + nativity + dispersal + rdiff*latitude +
      (1 | species) + #random effect
      car(dom.mat, gr = domain_id)) #adjacency matrix
  message(form)
  #Setting priors
  priors <- c(
    set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
    set_prior("normal(0, 1)",         class = "b"),      # includes slope for me()
    set_prior("exponential(1)",       class = "sd"),     # RE SD
    set_prior("student_t(3, 0, 2.5)", class = "sigma")   # residual SD (beyond phen_sd)
  )
  #Fitt model
  fit <- brm(
    formula = form,
    data    = train.dat1,
    data2 = list(dom.mat=dom.mat), # adjacency matrix data
    family  = gaussian(),           # identity link on [-1, 1]
    prior   = priors,
    chains  = 4, iter = 3000, warmup = 1000, cores = 4,
    #threads = threading(16),
    seed    = 20250909,
    control = list(adapt_delta = 0.95, max_treedepth = 13))
  message(cat("\tModeling complete! ... Saving model fit"))
  saveRDS(fit, file = file.path(L2, paste0("brms_fit_Run", run, "_training_nativity.rds")))
  message(cat("\tPredicting using the testing dataset"))
  #predict testing data using the training model
  fit.t <- predict(fit, newdata = test.dat1)
  saveRDS(fit.t, file = file.path(L2, paste0("brms_fit_Run", run, "_testing_nativity.rds")))
  
}) #END timer

sink(file = file.path(graphs, paste0("brms_fit_Run", run, "_nativity_TotalRunTime.txt")))
min = round(x.time[3]/60)
if(min > 59){
  hr = round(min/60)
  message("Total run time: ", hr, " hours")
  if(hr > 23){
    day = round(hr/24)
    message("Total run time: ", day, " days")
  }
} else{
  message("Total run time: ", min, " minutes")
}
sink()


#--- save output ---
message(cat("\tsaving output"))
#Posterior Mean predictions
#Posterior expected value per observation
train_fitted <- fitted(fit)  # matrix with Estimate, Est.Error, Q2.5, Q97.5
# Posterior mean predictions
# train_mean <- posterior_predict(fit, ndraws = 200)
train_mean <- train_fitted[, "Estimate"]
test_mean <- fit.t[, "Estimate"]  # if predict() returns a matrix with Estimate
rmse_train <- sqrt(mean((train.dat1$pdiff - train_mean)^2)); rmse_test <- sqrt(mean((test.dat1$pdiff - test_mean)^2))
r2.train <- cor(train.dat1$pdiff, train_mean)^2; r2.test  <- cor(test.dat1$pdiff, test_mean)^2
# r2.train = bayes_R2(fit); r2.test = bayes_R2(fit.t)

sink(file.path(graphs, paste0("brms_fit_Run", run, "_nativity_output.txt")))
cat("\nRMSE\n")
cat("training: ", round(rmse_train, 4)); cat("\ntesting: ", round(rmse_test, 4))
cat("\nR^2\n")
cat("\ntraining: ", r2.train); cat("\ntesting: ", r2.test)
cat("\n")
cat("\nTraining\n")
cat("Summary:\n"); print(summary(fit))
cat("\nPrior summary:\n"); print(prior_summary(fit))
cat("\nLook for the coefficient named b_me... — that's the slope for the latent RANGE.\n")
print(fit)
cat("\nTesting\n")
cat("Summary:\n"); print(summary(fit.t))
print(fit.t)
sink()
#-------------------