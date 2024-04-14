# examine imbalance in the covariates between the two groups
library(tableone)
library(kableExtra)

source("scripts/helpers.R")
load("data/cleaned-data.RData")

# create a table 1 for the potential covariates
(tabUnmatched <- tableone::CreateTableOne(vars = adj_vars, 
                               strata = "randomized_to", 
                               data = data,
                               test = FALSE))

# using Austin 2009
imbal_thresh <- 1.96*sqrt(2/(nrow(data)/2))

# extract the SMD values and save to pdf
stddiffs <- data.frame(ExtractSmd(tabUnmatched)) %>%
  rownames_to_column("var") %>%
  arrange(desc(X1.vs.2)) %>%
  mutate(
    adjust_0.1 = X1.vs.2 >= 0.1,
    adjust_derived = X1.vs.2 >= imbal_thresh,
    ASD = round(X1.vs.2, 3), 
    .keep = "unused"
    ) %>%
  select(var, ASD, adjust_0.1, adjust_derived)

stddiffs %>%
  mutate(var = recode(var,  !!!data_labels)) %>%
  kableExtra::kbl(
    caption = "Standardized mean differences for potential confounders",
    booktabs = T, 
    escape = F,
    col.names = c("Covariate", "ASD", "Adjust? (0.1)", "Adjust? (Austin 2009)")
      ) %>%
  kableExtra::kable_classic(full_width = FALSE, position = "center", 
                            html_font = "Cambria") %>%
kableExtra::save_kable("tbls/balance/table1_potential_confounders.pdf")

# which variables aren't balanced?
unbalanced_vars <- stddiffs %>%
  filter(adjust_0.1 == TRUE) %>%
  select(var) %>%
  as.list()
save(unbalanced_vars, file = "data/unbalanced_vars.RData")

# 2/22 - OLD CODE FOR ASD THAT MIRRORED STATA. SAME RESULTS WITH TABLEONE.
# # what vars should be adjusted for?
# ## calculate asd for predictors by randomized_to group
# # using https://support.sas.com/resources/papers/proceedings12/335-2012.pdf
# stddiffs <- calculate_stddiff(data, randomized_to) %>%
#   rownames_to_column("name")
# 
# 
# 
# # save the asd table to a pdf for inspection
# stddiffs %>%
#   arrange(desc(stddiff)) %>%
#   filter(name %in% adj_vars) %>%
#   mutate(adj_austin = stddiff >= imbal_thresh,
#          adj_0.1    = stddiff >= 0.1) %>%
#   kbl(caption = "Absolute standardized mean differences between treatment groups",
#       booktabs = TRUE) %>%
#   kable_classic() %>%
#   save_kable("tbls/balance/asd_potential_confounders.pdf")
# 
# sink("tbls/balance/table1_potential_confounders.txt") # Open a connection to a .txt file
# print(tabUnmatched, smd = TRUE) # Print the output with smd
# sink() # Close the connection