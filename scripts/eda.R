# data exploration
# load in dataset
source("scripts/helpers.R")
load("data/cleaned-data.RData")

# randomization ----
data %>%
  janitor::tabyl(randomized_to)

# TODO: missing the randomization for 11 patients
data %>% 
  filter(is.na(randomized_to)) %>%
  select(uid) %>%
  write_csv("probs/missing-randomization.csv")

# Specify variables, labels, and titles
variables <- data %>% 
  select(where(is.numeric)) %>%
  colnames()

# create density plots for each continuous variable in the dataset
#plots <- 
pdf("figs/eda/density-plots.pdf")
print(lapply(variables, create_density_plot, 
             data = data, fill_var = randomized_to))
dev.off()

# save correlation plots of data
GGally::ggpairs(data, 
                columns = {{adj_vars}},
                ggplot2::aes(color=as.factor(randomized_to), alpha = 0.5)) %>%
  ggsave(plot = ., "figs/eda/ggpairs.png", width = 30, height = 30, bg = "white") 

# use spearman \rho statistic to estimate rank-based association
testRes <- corrplot::cor.mtest(data %>% select(where(is.numeric)), conf.level = 0.95)
png(height=500, width=500, file="figs/eda/corrplot-significant.png", type = "quartz")
corrplot::corrplot(cor(data %>% select(where(is.numeric)), use = 'complete.obs'), 
                   p.mat = testRes$p, method = 'circle', 
                   type = 'lower', insig='blank', addCoef.col = 'black',
                   number.cex = 0.8, order = 'AOE', diag = FALSE,
                   title = "Significant Spearman correlation coefficients (p < 0.05)")
dev.off()

# Evaluate missing data ----
# peek at dataset
visdat::vis_dat(data, facet = randomized_to) %>% 
  ggsave(file = "figs/missing/missing_data.png", 
         width = 10, height = 10, bg = "white")

# missing data
naniar::miss_var_summary(data) %>% 
  mutate(pct_missing = round(pct_miss, 2), .keep = "unused") %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")

# are outcome variables missing?
data %>%
  select(randomized_to, intubation_result, cl_grade, intubation_attempts, teeth_injury_yn, teeth_injury_specify,
         cough_yn, cough_severity, sore_throat_yn, sore_throat_severity, hoarseness_yn, hoarseness_severity) %>%
  naniar::vis_miss(., facet = randomized_to)
