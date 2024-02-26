# load libraries
library(lme4)
library(tidyverse)

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
  gtsummary::tbl_regression(exponentiate = TRUE) %>%
  gtsummary::add_global_p()  %>%
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
