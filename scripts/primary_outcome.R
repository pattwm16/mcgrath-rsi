# load libraries
source("scripts/helpers.R")
load("data/cleaned-data.RData")

file_path <- 'figs/primary/' # path for figures

### PRIMARY OUTCOME: Cormack and Lehane grade visualization ----
# distribution of patients in each glottis visualization class
data %>%
  filter(!is.na(cl_grade)) %>%
  ggplot(aes(x = cl_grade, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(#title = "Distribution of glottis visualization by randomization group",
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

# calculate median visualization
# NB: need to change to numeric, so that median can be calculated (4 = 2a)
data %>%
  group_by(randomized_to) %>%
  summarise(median_cl_grade = median(as.numeric(cl_grade)))

# unadjusted analysis of mann-whitney u test for cl_grade
data %>%
  filter(!is.na(cl_grade)) %>%
  mutate(cl_grade = as.numeric(cl_grade)) %>%
  rstatix::wilcox_test(cl_grade ~ randomized_to, data = .) # MWU test
# p-value 0.29, can't reject null of no difference

# unadjusted proportional odds logistic regression, since no ASD > threshold
polr.fit.unadjusted <- 
  MASS::polr(cl_grade ~ randomized_to + provider + center,
             data = data, Hess = TRUE)

# brant test for proportional odds assumption
# NB: p > 0.05 -> parallel assumption holds
# brant::brant(model = polr.fit.unadjusted)

# visually inspect for all thresholds
pomcheckr::pomcheck(cl_grade ~ randomized_to + provider + center, data = data)


# surrogate residuals using sure package
# NB: https://koalaverse.github.io/sure/articles/sure.html
(qq <- sure::autoplot.polr(polr.fit.unadjusted))
(fitted <- sure::autoplot.polr(polr.fit.unadjusted, what = "fitted"))
ggsave(plot = gridExtra::grid.arrange(qq, fitted, ncol = 2), 
       filename = paste0(file_path, "cl_grade_polr_residuals.png"), 
         width = 12, height = 6, bg = "white")

(fit1 <- polr.fit.unadjusted %>%
  tbl_regression(exponentiate = TRUE,
                 label = data_labels,
                 include = randomized_to,
                 pvalue_fun = purrr::partial(style_sigfig, digits = 3)) %>%
  add_global_p()  %>%
  modify_caption("Proportional odds logistic regression for CL grade"))

gt::gtsave(as_gt(fit1), file = paste0(file_path, "cl_grade_polr.png"))

data %>% 
  tabyl(randomized_to, cl_grade) %>% 
  adorn_totals("col") %>%
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()

# does accounting for center-level effects change the results?
t1 <- MASS::polr(cl_grade ~ randomized_to, data = data, Hess = TRUE) %>%
  tbl_regression(exponentiate = TRUE,
                 label = data_labels
                 ) %>%
  add_global_p()  %>%
  modify_caption("Proportional odds logistic regression for CL grade\n(unadjusted)")

t2 <- MASS::polr(cl_grade ~ randomized_to + provider + center, 
                 data = data, Hess = TRUE) %>%
  tbl_regression(exponentiate = TRUE,
                 label = data_labels
                 ) %>%
  add_global_p()  %>%
  modify_caption("Proportional odds logistic regression for CL grade\n(adjusted for center and provider)")

t3 <- MASS::polr(cl_grade ~ randomized_to * center + provider,
             data = data, Hess = TRUE) %>%
  tbl_regression(exponentiate = TRUE,
                 label = data_labels
                 ) %>%
  add_global_p()  %>%
  modify_caption("Proportional odds logistic regression for CL grade\n(test interaction between center and provider)")

tbl_merge(tbls = list(t1, t2, t3), 
          tab_spanner = c("**Unadjusted**", "**No interaction**", "**Interaction**")
          ) %>%
  modify_caption("Model comparison for CL grade using proportional odds logistic regression")

# full model with 0.1 ASD threshold
# NB: per Austin (2009), don't need to adjust for anything < 0.1973 -- no vars
#     greater than that value 
# reg_data <- data %>%
#   select(cl_grade, randomized_to, teeth_status, upper_lip_bite_test_class,
#          mallampati_score, gender, mouth_opening_cm, bmi, thyromental_height_cm,
#          thyromental_distance_cm, mandibular_protrusion_test, 
#          mobility_cervical_spine) %>%
#   mutate(
#     across(c(mallampati_score, mobility_cervical_spine, upper_lip_bite_test_class, 
#              mandibular_protrusion_test, teeth_status), 
#            ~ factor(., ordered = FALSE))
#   ) 
# polr.fit.saturated <- 
#   MASS::polr(cl_grade ~ randomized_to + teeth_status + upper_lip_bite_test_class +
#                mallampati_score + gender + mouth_opening_cm + bmi + thyromental_height_cm +
#                thyromental_distance_cm + mandibular_protrusion_test + mobility_cervical_spine,
#              data = reg_data, Hess = TRUE)
#
# test for multicollinearity
# NB: not needed for unadjusted analysis
# car::vif(polr.fit.saturated)