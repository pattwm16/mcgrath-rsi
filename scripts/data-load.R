# load packages
library(tableone)
library(stddiff)
library(kableExtra)

# load in dataset (downloaded 2-16-24)
file_path <- "scripts/import-r-data.r" # from REDCap
source(file_path)
source("scripts/helpers.R")

# add counts for intubation failure reasons
counts <- data %>% 
  select(!contains("factor")) %>%
  mutate(intub_fail_reason_n = rowSums(select(., starts_with("intubation_failure___"))),
         teeth_injury_n = rowSums(select(., starts_with("teeth_injury_specify___")))) %>%
  select(uid, intub_fail_reason_n, teeth_injury_n)

# severity ordering
severity_grades <- c("None", "Mild (less than a common cold)", 
                     "Moderate (similar to a cold)", 
                     "Severe (more than a common cold)")

hoarsness_grades <- c("None", "Noticed by the patient only", 
                     "Aparent to an observer", 
                     "Aphonia")

teeth_status_levels <- c("Edentulous", "Missing frontal teeth", "Full dentition") 

# join counts into data
data <- data %>% 
  left_join(counts, by = "uid")

# select only the columns we need
data <- data %>%
  select(
    # identifiers
    uid, # is there a center identifier?
    # demographics
    age, gender = gender.factor, height_cm, weight_kg, bmi, asa_class = asa, 
    # preop airway eval
    #preoperative_skip, # was this filled out
    mallampati_score = mallampati_score.factor, 
    upper_lip_bite_test_class = upper_lip_bite_test_class.factor,
    mobility_cervical_spine = mobility_cervical_spine.factor, 
    mandibular_protrusion_test = mandibular_protrusion_test.factor, 
    teeth_status = edentulous.factor, 
    osa = obstructive_sleep_apnea,
    snoring = snoring, 
    cpap_use = cpap_use, 
    difficult_airway_hx = difficult_airway, 
    inter_incisor_gap_cm = inter_incisor_gap, 
    mouth_opening_cm = mouth_opening,
    thyromental_distance_cm = thyromental_distance, 
    thyromental_height_cm = thyromental_height, 
    sternomental_distance_cm = sternomental_distance, 
    neck_circumference_cm = neck_circumference, 
    # intubation
    #intubation_skip, # was this filled out
    start_of_case_date, 
    randomized_to = randomization.factor, 
    blade_size,
    provider = provider.factor, 
    endotracheal_tube_number, 
    stylette_use, burp, sellick, 
    cl_grade = cormacklehane_grading.factor, 
    pogo_score = pogo_score.factor, 
    intubation_attempts = intubation_attempts.factor, 
    time_to_intubation,
    ett_pressure_cmh2o = endotracheal_pressure, 
    ease_of_intubation = ease_of_intubation.factor, 
    intubation_result = intubation_failure.categorical,
    intub_fail_reason_n,
    # postop complications
    #complications_skip, # was this filled out
    teeth_injury, 
    teeth_injury_specify = teeth_injury_specify.categorical, teeth_injury_n,
    cough, cough_severity = cough_specify.factor, 
    sore_throat, sore_throat_severity = sore_throat_specify.factor,
    hoarseness, hoarseness_severity = hoarseness_specify.factor) %>%
  mutate(across(where(is.character), as.factor),
         uid = as.character(uid),
         teeth_injury_specify = as.character(teeth_injury_specify),
         cough_severity = as.character(cough_severity),
         sore_throat_severity = as.character(sore_throat_severity),
         hoarseness_severity = as.character(hoarseness_severity),
         blade_size = factor(blade_size)) %>%
  mutate(
    teeth_injury_specify = factor(replace_na(teeth_injury_specify, "No injury")),
    cough_severity       = factor(replace_na(cough_severity, "None")),
    sore_throat_severity = factor(replace_na(sore_throat_severity, "None")),
    hoarseness_severity  = factor(replace_na(hoarseness_severity, "None")),
  ) %>%
  ## TODO: should teeth_status be ordinal? full denture wasn't an option in the plan
  mutate(teeth_status_ord = case_when(
    teeth_status == "Edentulous" ~ "Edentulous",
    teeth_status == "Missing frontal teeth" ~ "Missing frontal teeth",
    teeth_status %in% c("Full denture", "No missing teeth" ) ~ "No missing teeth"
  )) %>%
  ## END TODO
  mutate(
    cl_grade                = fct_rev(factor(cl_grade, ordered = TRUE)),
    pogo_score              = factor(pogo_score, ordered = TRUE),
    intubation_attempts     = factor(intubation_attempts, ordered = TRUE),
    ease_of_intubation      = factor(ease_of_intubation, ordered = TRUE),
    asa_class               = factor(asa_class, ordered = TRUE),
    mallampati_score        = factor(mallampati_score, ordered = TRUE),
    upper_lip_bite_test_class = factor(upper_lip_bite_test_class, ordered = TRUE),
    teeth_status_ord        = factor(teeth_status_ord, ordered = TRUE,
                                     levels = teeth_status_levels),
    mobility_cervical_spine = fct_rev(factor(mobility_cervical_spine, ordered = TRUE,
                                             levels = c("0", "15", "30", "45"))),
    mandibular_protrusion_test = factor(mandibular_protrusion_test, ordered = TRUE),
    cough_severity          = factor(cough_severity, ordered = TRUE, 
                                     levels = severity_grades),
    sore_throat_severity    = factor(sore_throat_severity, ordered = TRUE, 
                                     levels = severity_grades),
    hoarseness_severity     = factor(hoarseness_severity, ordered = TRUE, 
                                     levels = hoarsness_grades)
  ) %>%
  mutate(
    osa_yn                 = as.logical(osa),
    snoring_yn             = as.logical(snoring),
    cpap_use_yn            = as.logical(cpap_use),
    difficult_airway_hx_yn = as.logical(difficult_airway_hx),
    stylette_use_yn        = as.logical(stylette_use),
    burp_yn                = as.logical(burp),
    sellick_yn             = as.logical(sellick),
    teeth_injury_yn        = as.logical(teeth_injury),
    cough_yn               = as.logical(cough),
    sore_throat_yn         = as.logical(sore_throat),
    hoarseness_yn          = as.logical(hoarseness),
    .keep = "unused" # remove old non-logical cols from this mutate call
  ) %>%
  mutate(start_of_case_date = as.Date(start_of_case_date)) %>%
  ## TODO: inclusion criteria specify less than 45 bmi, two subjects have bmi > 45
  filter(!is.na(randomized_to))

adj_vars <- c("gender", "height_cm", "weight_kg", "bmi", "age", "asa_class", 
          "mallampati_score", "mobility_cervical_spine", "upper_lip_bite_test_class", 
          "mandibular_protrusion_test", "teeth_status", "osa_yn", "snoring_yn", "cpap_use_yn", 
          "difficult_airway_hx_yn", "inter_incisor_gap_cm", "mouth_opening_cm", 
          "thyromental_distance_cm", "thyromental_height_cm", "sternomental_distance_cm", 
          "neck_circumference_cm")

# save cleaned dataset
save(data, adj_vars, file = "data/cleaned-data.RData")