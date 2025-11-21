## ---------- Monte Carlo null for non-independence ----------
## Author: Tong Qiu
## Inputs:
##   pdiff, psd : vectors (phenology mean & SD, length n)
##   rdiff, rsd : vectors (range mean & SD, length n)
##   B            : number of Monte Carlo replicates (e.g., 2000)
##   blocks       : optional factor for restricted permutation (e.g., region/site)
##   use_slope    : if TRUE, test slope of lm(y ~ x + covars); otherwise test cor(y, x)
##   covars       : optional data.frame of additional covariates (fixed across permutations)

mc_null_test <- function(y_mean, y_sd, x_mean, x_sd,
                         B = 2000, blocks = NULL, use_slope = FALSE, covars = NULL,
                         seed = 123, clamp = TRUE)
{ #START of function
  stopifnot(length(y_mean) == length(x_mean),
            length(y_sd)   == length(x_sd),
            length(y_mean) == length(y_sd))
  set.seed(seed)
  n <- length(y_mean)

  clamp11 <- function(v) pmin(pmax(v, -1 + 1e-6), 1 - 1e-6)

  ## 1) One “observed” draw from your prediction distributions
  y_obs <- rnorm(n, y_mean, y_sd)
  x_obs <- rnorm(n, x_mean, x_sd)
  if (clamp) { y_obs <- clamp11(y_obs); x_obs <- clamp11(x_obs) }

  ## helper to compute statistic
  get_stat <- function(y, x) {
    if (!use_slope) {
      return(stats::cor(y, x, use = "pairwise.complete.obs"))
    } else {
      if (is.null(covars)) {
        return(coef(lm(y ~ x))[2])
      } else {
        df <- data.frame(y = y, x = x, covars)
        return(coef(lm(y ~ x + ., data = df))[2])
      }
    }
  }

  stat_obs <- get_stat(y_obs, x_obs)

  ## 2) Null: redraw x and y from their own uncertainty, but
  ##    break their pairing by permuting x within blocks (or globally)
  permute_within_blocks <- function(z, f) {
    if (is.null(f)) return(sample.int(length(z)))
    idx <- seq_along(z)
    split_idx <- split(idx, f)
    new_idx <- unlist(lapply(split_idx, sample), use.names = FALSE)
    return(new_idx)
  }

  null_stats <- numeric(B)
  for (b in seq_len(B)) {
    y_b <- rnorm(n, y_mean, y_sd)
    x_b <- rnorm(n, x_mean, x_sd)
    if (clamp) { y_b <- clamp11(y_b); x_b <- clamp11(x_b) }

    idx <- permute_within_blocks(x_b, blocks)
    x_b_perm <- x_b[idx]

    null_stats[b] <- get_stat(y_b, x_b_perm)
  }

  ## two-sided Monte Carlo p
  p_mc <- mean(abs(null_stats) >= abs(stat_obs))

  list(
    observed_stat      = stat_obs,
    null_mean          = mean(null_stats),
    null_sd            = sd(null_stats),
    p_value_mc         = p_mc,
    excess_effect      = stat_obs - mean(null_stats),  # “beyond spurious expectation”
    null_stats         = null_stats
  )
} #END of function



#######
# Data
#######
#working directories
mwd = "~/data-store/home/lamador/Data"
L2 = file.path(mwd, "L2")
grahs = file.path(L2, "Graphs")
load(file = file.path(L2, "Subset_ModelData_NoOutliers.RData"), verbose = TRUE)
dat = sub.data
## variables of importance: pdiff, psd, rdiff, rsd, domain_id, funct_tpe, nativity, dispersal

x.time = system.time({ #START timer
  ###########################################
  # 1) Correlation test (global permutation)
  ###########################################
  message("1) Correlation test (global permutations)")
  ## Suppose your data.frame is `dat` with columns:
  ## phen_mean, phen_sd, range_mean, range_sd, region, (and maybe traits)
  #out_cor <- mc_null_test(
  # y_mean = dat$phen_y, y_sd = dat$phen_sd,
  # x_mean = dat$range_mean, x_sd = dat$range_sd,
  # B = 4000, blocks = NULL, use_slope = FALSE)
  out_cor <- mc_null_test(
    y_mean = dat$pdiff, y_sd = dat$psd,
    x_mean = dat$rdiff, x_sd = dat$rsd,
    B = 4000, blocks = NULL, use_slope = FALSE
  )
  out_cor$p_value_mc
  out_cor$excess_effect   # observed r minus expected spurious r
  message(print("\tCorrelation test completed"))

  ###########################################################
  # 2) Slope test with traits and region-blocked permutation
  ###########################################################
  message("2) Slope test with traits and region-blocked permutation")
  covars <- data.frame(funct_type = dat$funct_type, dispersal = dat$dispersal, nativity = dat$nativity)
  out_slope <- mc_null_test(
    y_mean = dat$pdiff, y_sd = dat$psd,
    x_mean = dat$rdiff, x_sd = dat$rsd,
    B = 4000, blocks = dat$domain_id, use_slope = TRUE, covars = covars
  )
  out_slope$p_value_mc
  out_slope$excess_effect # observed β_x minus expected spurious β_x
  message(print("\tSlope test completed"))
}) #END timer

message("3) Saving output")
#save output
sink(file.path(graphs, "MC_correlation_check.txt"))
print(paste0("Total run time: ", round(x.time[3]/60), " minutes"))
cat("\n1) Correlation test (global permutations)....\n")
cat("Monte Carlo p-value \n")
out_cor$p_value_mc
cat("\nMonteCarlo excess effect (observed r minus expected spurious r)\n")
out_cor$excess_effect   # observed r minus expected spurious r

cat("\n2) Slope test with traits and region-blocked permutation\n")
cat("Monte Carlo p-value \n")
out_slope$p_value_mc
cat("\nMonteCarlo excess effect (observed β_x minus expected spurious β_x)\n")
out_slope$excess_effect # observed β_x minus expected spurious β_x
sink()
