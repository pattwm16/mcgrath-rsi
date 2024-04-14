# table 1, request from mehmet

source("scripts/helpers.R")
load("data/cleaned-data.RData")

# create table 1

# age, cervical mobility, mouth opening, interincisor gap, 
# thyro-mental and sterno-mental distances, upper lip bite test, Mallampati score, 
# and dental status, mandibular protrusion test, and neck circumference. 

library(table1)
library(labelled)
library(tidyverse)



(table1 <- CreateTableOne(vars = c("age", "mobility_cervical_spine", "mouth_opening_cm", "inter_incisor_gap_cm", 
                                   "thyromental_distance_cm", "sternomental_distance_cm", "upper_lip_bite_test_class", 
                                   "mallampati_score", "teeth_status", "mandibular_protrusion_test", 
                                   "neck_circumference_cm"), 
                         strata = "randomized_to",
                         data = data, 
                         smd = TRUE,
                         addOverall = TRUE,
                         #labels = data_labels,
                         factorVars = c("teeth_status", 
                                        "upper_lip_bite_test_class", 
                                        "mallampati_score", 
                                        "mandibular_protrusion_test")))

# create table 1

# custom renderer
rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  Q1 <- quantile(x, 0.25, na.rm = T)
  Q3 <- quantile(x, 0.75, na.rm = T)
  what <- switch(name,
                 age = "Median [Q1, Q3]",
                 bmi = "Median [Q1, Q3]",
                 mouth_opening_cm  = "Mean (SD)",
                 inter_incisor_gap_cm = "Mean (SD)",
                 thyromental_distance_cm = "Mean (SD)",
                 thyromental_height_cm = "Mean (SD)",
                 sternomental_distance_cm = "Mean (SD)",
                 neck_circumference_cm = "Median [Q1, Q3]")
  parse.abbrev.render.code(c("", what))(x)
}

label(data$center) <- "Study center"
label(data$age) <- "Age"
label(data$gender) <- "Sex"
label(data$bmi) <- "BMI"
label(data$asa_class) <- "ASA physical status"
label(data$osa_yn) <- "History of OSA"
label(data$difficult_airway_hx_yn) <- "History of difficult airway"
label(data$snoring_yn) <- "History of snoring"
label(data$cpap_use_yn) <- "CPAP use"
label(data$mobility_cervical_spine) <- "Mobility of the cervical spine"
label(data$mouth_opening_cm) <- "Mouth opening"
label(data$inter_incisor_gap_cm) <- "Inter-incisor gap"
label(data$thyromental_distance_cm) <- "Thyromental distance"
label(data$thyromental_height_cm) <- "Thyromental height"
label(data$sternomental_distance_cm) <- "Sternomental distance"
label(data$upper_lip_bite_test_class) <- "Upper lip bite test class"
label(data$mallampati_score) <- "Mallampati score"
label(data$teeth_status) <- "Teeth status"
label(data$mandibular_protrusion_test) <- "Mandibular protrusion test"
label(data$neck_circumference_cm) <- "Neck circumference"

units(data$age) <- "years"
units(data$mobility_cervical_spine) <- "degrees"
units(data$mouth_opening_cm) <- "cm"
units(data$inter_incisor_gap_cm) <- "cm"
units(data$thyromental_distance_cm) <- "cm"
units(data$sternomental_distance_cm) <- "cm"
units(data$neck_circumference_cm) <- "cm"

(tab1 <- table1(~ center +age + gender + bmi + asa_class + osa_yn + snoring_yn + cpap_use_yn + difficult_airway_hx_yn + mobility_cervical_spine + mouth_opening_cm + inter_incisor_gap_cm + 
         thyromental_distance_cm + thyromental_height_cm + sternomental_distance_cm + upper_lip_bite_test_class + 
         mallampati_score + teeth_status + mandibular_protrusion_test + neck_circumference_cm | randomized_to,
       data = data,
       render = rndr))

# (tab1 <- table1(~ age + mobility_cervical_spine + mouth_opening_cm + inter_incisor_gap_cm + 
#                   thyromental_distance_cm + sternomental_distance_cm + upper_lip_bite_test_class + 
#                   mallampati_score + teeth_status + mandibular_protrusion_test + neck_circumference_cm | randomized_to,
#                 data = data,
#                 render = rndr))


# save table 1
t1flex(tab1) %>%
  flextable::save_as_docx(path = "tbls/table1.docx")

t1flex(tab1) %>%
  flextable::bg(bg = "white", part = "all") %>%
  flextable::save_as_image(path = "tbls/table1.png")
