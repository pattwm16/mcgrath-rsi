# run analysis

# include custom helpers
source("scripts/helpers.R")

# load in dataset
source("scripts/data-load.r")

# evaluate dataset
source("scripts/eda.R")

# assess balance in randomization using absolute standardized difference
source("scripts/imbalance.R") # output saved under tbls/balance

# modelling for primary outcomes
source("scripts/primary_outcome.R")

# create plots for secondary outcomes (unadjusted)
source("scripts/secondary_outcomes.R")

# create plots for safety outcomes (unadjusted)
source("scripts/safety_outcomes.R")
