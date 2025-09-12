library(brms)

set.seed(20250909)

clamp11 <- function(x) pmin(pmax(x, -1 + 1e-6), 1 - 1e-6)  # keep inside (-1, 1)

## ---------- Simulation settings ----------
n_species <- 15
n_rep     <- 30
N         <- n_species * n_rep

species_levels <- paste0("sp", seq_len(n_species))

## traits (simple):
leaf_levels      <- c("woody","non-woody")      # 2-level factor
dispersal_levels <- c("wind","animal","ballistic")  # 3-level factor

## Assign species-level traits
sp_leaf      <- factor(sample(leaf_levels, n_species, TRUE), levels = leaf_levels)
sp_dispersal <- factor(sample(dispersal_levels, n_species, TRUE), levels = dispersal_levels)

## Design species and site
design <- data.frame(
  species = rep(species_levels, each = n_rep),
  stringsAsFactors = FALSE
)
# attach traits by species
design$leaf      <- sp_leaf[match(design$species, species_levels)]
design$dispersal <- sp_dispersal[match(design$species, species_levels)]
design$species   <- factor(design$species,   levels = species_levels)
design$leaf      <- factor(design$leaf,      levels = leaf_levels)
design$dispersal <- factor(design$dispersal, levels = dispersal_levels)

## ---------- Random intercepts (species) ----------
u_species_range <- rnorm(n_species, 0, 0.20); names(u_species_range) <- species_levels
u_species_phen  <- rnorm(n_species, 0, 0.25); names(u_species_phen)  <- species_levels

## ---------- True coefficients on [-1, 1] scale ----------
b0_r   <-  0.00                      # intercept for RANGE (latent)
b_leaf <-  0.25                      # evergreen vs deciduous on RANGE
b_disp <- c(wind = 0.00, animal = 0.15, ballistic = -0.15)

a0_p    <-  0.05                     # intercept for PHENO
a1_prng <-  0.80                     # slope for RANGE -> PHENO (target to recover)
a_leaf  <- -0.10
a_disp  <- c(wind = 0.00, animal = 0.10, ballistic = -0.05)

cat(sprintf("TRUE slope (range -> phenology): %.2f\n", a1_prng))

## ---------- Simulate latent truths in (-1, 1) ----------
## We build linear predictors and squash with tanh to respect [-1, 1]
tanh_squash <- function(x) tanh(x)  # maps R -> (-1,1)

eta_range <- with(design,
                  b0_r +
                    b_leaf * as.numeric(leaf == "evergreen") +
                    b_disp[as.character(dispersal)] +
                    u_species_range[as.character(species)] +
                    rnorm(N, 0, 0.35)
)
range_true <- clamp11(tanh_squash(eta_range))

eta_phen <- with(design,
                 a0_p +
                   a1_prng * range_true +
                   a_leaf * as.numeric(leaf == "evergreen") +
                   a_disp[as.character(dispersal)] +
                   u_species_phen[as.character(species)] +
                   rnorm(N, 0, 0.30)
)
phen_true <- clamp11(tanh_squash(eta_phen))


## Simulate SD and mean

range_sd  <- runif(N, 0.05, 0.15)                     # predictor SDs
range_mean <- clamp11(range_true + rnorm(N, 0, range_sd))

phen_sd   <- runif(N, 0.06, 0.18)                     # response SDs
phen_mean <- clamp11(phen_true + rnorm(N, 0, phen_sd))

## Final data frame for analysis
dat <- data.frame(
  phen_y        = phen_mean,   # observed response mean
  phen_sd       = phen_sd,     # per-row SE/SD for response measurement error
  range_mean    = range_mean,  # observed predictor mean
  range_sd      = range_sd,    # per-row SD for predictor ME
  leaf          = design$leaf,
  dispersal     = design$dispersal,
  species       = design$species
)


## Fit BRMS model:
##   - Gaussian identity
##   - Response measurement error via  | se(phen_sd, sigma = TRUE)
##   - Predictor measurement error via me(range_mean, range_sd)
##   - Random intercept by species (can easily change to site or other factors)

form <- bf(
  phen_y | se(phen_sd, sigma = TRUE) ~
    me(range_mean, range_sd) + leaf + dispersal + (1 | species)
)

priors <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
  set_prior("normal(0, 1)",         class = "b"),      # includes slope for me()
  set_prior("exponential(1)",       class = "sd"),     # RE SD
  set_prior("student_t(3, 0, 2.5)", class = "sigma")   # residual SD (beyond phen_sd)
)

fit <- brm(
  formula = form,
  data    = dat,
  family  = gaussian(),           # identity link on [-1, 1]
  prior   = priors,
  chains  = 4, iter = 3000, warmup = 1000, cores = 4,
  seed    = 20250909,
  control = list(adapt_delta = 0.95, max_treedepth = 13)
)

print(fit)
cat("\nLook for the coefficient named b_me... — that's the slope for the latent RANGE.\n")
