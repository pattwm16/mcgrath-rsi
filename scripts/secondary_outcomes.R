# secondary outcomes
source("scripts/helpers.R")
load("data/cleaned-data.RData")

# intubation failure
file_path <- 'figs/secondary/'

# distribution of patients in each glottis visualization class
data %>%
  filter(!is.na(intubation_attempts)) %>%
  ggplot(aes(x = intubation_attempts, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(title = "Distribution of intubation attempts by randomization group",
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

# unadjusted analysis of mann-whitney u test for cl_grade
data %>%
  filter(!is.na(intubation_attempts)) %>%
  mutate(intubation_attempts = as.numeric(intubation_attempts)) %>%
  wilcox_test(intubation_attempts ~ randomized_to, data = .)
# p-value 0.87, can't reject null of no difference

data %>%
  mutate(intubation_attempts = as.numeric(intubation_attempts) - 1) %>%
  mutate(
    across(c(mallampati_score, mobility_cervical_spine, upper_lip_bite_test_class, 
             mandibular_protrusion_test, teeth_status), 
           ~ factor(., ordered = FALSE))
  ) %>%
  MASS::glm.nb(intubation_attempts ~ randomized_to + teeth_status + upper_lip_bite_test_class +
                 mallampati_score + gender + mouth_opening_cm + bmi + thyromental_height_cm +
                 thyromental_distance_cm + mandibular_protrusion_test + mobility_cervical_spine, data = .) %>%
  tbl_regression(exponentiate = TRUE)
# no significant differences in intubation attempts