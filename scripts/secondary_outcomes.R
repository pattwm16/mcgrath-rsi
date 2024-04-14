# secondary outcomes
source("scripts/helpers.R")
load("data/cleaned-data.RData")

# intubation failure
file_path <- 'figs/secondary/'

## INTUBATION FAILURE - secondary outcome
# visualize data
data %>% 
  tabyl(intub_success, randomized_to) %>%
  adorn_totals('row') %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title()

# NB: protocol stated chi-squared, but not appropriate (see expected counts)
data %>%
  tabyl(intub_success, randomized_to) %>%
  chisq.test(simulate.p.value = TRUE) %>%
  .$expected # < 5 in 50% of cells -> so use fisher's exact test

# fisher's exact test
data %>%
  tabyl(intub_success, randomized_to) %>%
  fisher.test(conf.level = 0.975) # RR b/c this is cohort study
# p = 0.36, RR = 0.32 (0.002, 5.36)

# median values
data %>%
  group_by(randomized_to) %>%
  summarise(med_attempts = median(as.numeric(intubation_attempts), na.rm = TRUE))

## NUMBER OF INTUBATIONS - secondary outcome
# first, change from character to numeric
data <- data %>%
  mutate(intubation_attempts = as.numeric(intubation_attempts))

# tabyl
data %>% 
  tabyl(intubation_attempts, randomized_to) %>%
  adorn_totals('row') %>%
  adorn_percentages('col') %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title()

# distribution of patients in each glottis visualization class
data %>%
  filter(!is.na(intubation_attempts)) %>%
  ggplot(aes(x = intubation_attempts, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(#title = "Distribution of intubation attempts by randomization group",
       caption = paste0("Missing data for ", data %>% filter(is.na(intubation_attempts)) %>% nrow(), " patients"),
       x = "Number of intubation attempts",
       y = "Count") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  )
ggsave(paste0(file_path, "intubation_attempts_distribution.png"), 
       width = 8, height = 6, bg = "white")

# unadjusted analysis of mann-whitney u test for intubation attempts
data %>%
  filter(!is.na(intubation_attempts)) %>% # remove missing data
  mutate(intubation_attempts = as.numeric(intubation_attempts)) %>%
  rstatix::wilcox_test(intubation_attempts ~ randomized_to, data = .,
                       paired = FALSE)
# p-value 0.87, can't reject null of no difference

# median number of attempts
data %>%
  group_by(randomized_to) %>%
  summarise(med_attempts = median(as.numeric(intubation_attempts), na.rm = TRUE))

# negative binomial
data %>%
  MASS::glm.nb(intubation_attempts ~ randomized_to + provider + center, 
               init.theta = 0.005,
               trace = 2,
               #family = poisson,
            data = .) %>%
  tbl_regression(exponentiate = TRUE,
                 conf.level = 0.975,
                 include = c(randomized_to),
                 label = data_labels)
