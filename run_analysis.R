# run analysis

# include custom helpers
source("scripts/helpers.R")

# load in dataset
source("scripts/data-load.r")

# evaluate dataset
source("scripts/eda.R")

# assess balance in randomization using absolute standardized difference
source("scripts/imbalance.R") # output saved under tbls/balance

# create plots for secondary outcomes (unadjusted)

# create plots for safety outcomes (unadjusted)
source("scripts/safety_outcomes.R")

# modelling for primary outcomes
source("scripts/modelling.R")


# testing for 
