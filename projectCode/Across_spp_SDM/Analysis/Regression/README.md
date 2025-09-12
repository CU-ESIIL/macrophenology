**1. 01_Prep_Regression_Data.R**: This script merges the ensemble projection differences data for phenology and range. Descriptive information is added, and outliers are removed. 

**2. fit_m4.R**: This script takes the difference data of phenology and range and fits it to a linear regression with random effects of species and space.

**3. plots.R**: This script plots brms (Bayesian Regression Model Stan) outputs. 

**4. tables.R**: This script outputs tables from model outputs.

**5. traitDB_dispersal_wrangling.R**:  This script analyzes dispersal traits across species.

**6. uncertaintyHandling.R**: This script examines uncertainty associated with SDM predictions.

**7. MC_simulation.R**: The Monte Carlo null for non-independence compares the covariance between range and phenology
