# load libraries
source("scripts/helpers.R")
load("data/cleaned-data.RData")

file_path <- 'figs/primary/'

# distribution of patients in each glottis visualization class
data %>%
  filter(!is.na(cl_grade)) %>%
  ggplot(aes(x = cl_grade, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(title = "Distribution of glottis visualization by randomization group",
       caption = paste0("Missing data for ", data %>% filter(is.na(cl_grade)) %>% nrow(), " patients"),
       x = "Cormack and Lehane classification",
       y = "Count") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.05, .95),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  )
ggsave(paste0(file_path, "cl_grade_distribution.png"), 
       width = 8, height = 6, bg = "white")

# unadjusted analysis of mann-whitney u test for cl_grade
data %>%
  filter(!is.na(cl_grade)) %>%
  mutate(cl_grade = as.numeric(cl_grade)) %>%
  wilcox_test(cl_grade ~ randomized_to, data = .)
# p-value 0.36, can't reject null of no difference

# raw test
data <- data %>%
  select(cl_grade, randomized_to, teeth_status, upper_lip_bite_test_class,
         mallampati_score, gender, mouth_opening_cm, bmi, thyromental_height_cm,
         thyromental_distance_cm, mandibular_protrusion_test, 
         mobility_cervical_spine) %>%
  mutate(
    across(c(mallampati_score, mobility_cervical_spine, upper_lip_bite_test_class, 
             mandibular_protrusion_test, teeth_status), 
           ~ factor(., ordered = FALSE))
  ) 
glm.fit.saturated <- 
  MASS::polr(cl_grade ~ randomized_to + teeth_status + upper_lip_bite_test_class +
               mallampati_score + gender + mouth_opening_cm + bmi + thyromental_height_cm +
               thyromental_distance_cm + mandibular_protrusion_test + mobility_cervical_spine,
             data = data, Hess = TRUE)

# test for multicollinearity
car::vif(glm.fit.saturated)

# brant test
brant::brant(model = glm.fit.saturated, by.var = TRUE)

glm.fit.saturated %>%
  tbl_regression(exponentiate = TRUE) %>%
  add_global_p()  %>%
  modify_caption("Proportional odds logistic regression for CL grade")

# difficult airway is mostly no, so excluded for rank-deficiency
thresh <- "2a"
glm.fit <- data %>%
  mutate(
    across(c(mallampati_score, mobility_cervical_spine, upper_lip_bite_test_class, 
             mandibular_protrusion_test, teeth_status), 
           ~ factor(., ordered = FALSE))
  ) %>%
  glm((cl_grade == thresh) ~ randomized_to + teeth_status + upper_lip_bite_test_class +
               mallampati_score + gender + mouth_opening_cm + bmi + thyromental_height_cm +
               thyromental_distance_cm + mandibular_protrusion_test + mobility_cervical_spine, 
      data = ., family = binomial(link = "logit"))

glm.fit %>%
  gtsummary::tbl_regression(exponentiate = TRUE) %>%
  gtsummary::add_global_p() %>%
  modify_caption(paste0("Logistic regression (threshold at CL grade ", thresh, ")"))
