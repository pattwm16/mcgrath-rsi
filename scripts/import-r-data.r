#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
library(tidyverse)
#Read Data
data=read.csv('data/raw-data_2-19-24.csv')
#Setting Labels

label(data$uid)="uid"
label(data$redcap_event_name)="Event Name"
label(data$username_for_eligibility)="username_for_eligibility"
label(data$dag)="DAG MOVE this field  MUST be in second form!! the dag isnt created until after the record is saved so this cant be in the first form or it will be 0 **Note to me - this field will have the DAG# so I can use branching logic based on sitesUse HIDDED action tag on this field"
label(data$eligible)="1- Patient eligible"
label(data$consented)="2- Patient consented and enrolled"
label(data$consent_date)="Date and time of consent"
label(data$eligibility_and_enrollment_complete)="Complete?"
label(data$username_for_demographics)="username_for_demographics"
label(data$gender)="Sex"
label(data$height_cm)="Height (cm)"
label(data$age)="Age"
label(data$asa)="ASA"
label(data$weight_kg)="Weight (kg)"
label(data$bmi)="BMI"
label(data$race___8)="Race:  (choice=White)"
label(data$race___2)="Race:  (choice=Black)"
label(data$race___3)="Race:  (choice=Hispanic)"
label(data$race___99)="Race:  (choice=Other)"
label(data$race___9999)="Race:  (choice=Missing/Not asked)"
label(data$race___8888)="Race:  (choice=Not Applicable)"
label(data$race_other)="Other race"
label(data$demographics_complete)="Complete?"
label(data$username_for_preoperative)="username_for_preoperative"
label(data$preoperative_skip)="If form(s) can not be completed please select an item from the class reasons in the dropdown list."
label(data$mallampati_score)="Mallampati score:"
label(data$mobility_cervical_spine)="Mobility of cervical spine(degrees):"
label(data$upper_lip_bite_test_class)="Upper lip bite test class: "
label(data$mandibular_protrusion_test)="Mandibular protrusion test: "
label(data$edentulous)="Teeth status:  "
label(data$obstructive_sleep_apnea)="History of obstructive sleep apnea"
label(data$snoring)="History of snoring"
label(data$cpap_use)="History of CPAP use"
label(data$difficult_airway)="History of difficult airway"
label(data$inter_incisor_gap)="Inter-incisor gap "
label(data$mouth_opening)="Mouth opening "
label(data$thyromental_distance)="Thyromental distance "
label(data$thyromental_height)="Thyromental height "
label(data$sternomental_distance)="Sternomental distance "
label(data$neck_circumference)="Neck circumference "
label(data$preoperative_airway_examination_complete)="Complete?"
label(data$username_for_intubation)="username_for_intubation"
label(data$start_of_case_date)="Date of Surgery: "
label(data$intubation_skip)="If form(s) can not be completed please select an item from the class reasons in the dropdown list."
label(data$randomization)="Randomization: "
label(data$randomization_date)="Date of Randomization: "
label(data$blade_size)="Blade size: "
label(data$provider)="Provider"
label(data$endotracheal_tube_number)="Endotracheal tube number:"
label(data$stylette_use)="Stylette use"
label(data$burp)="BURP:"
label(data$sellick)="Sellick:"
label(data$cormacklehane_grading)="Cormack-Lehane grading:"
label(data$pogo_score)="Portion of glottis opening score (POGO)"
label(data$intubation_attempts)="Intubation attempts:"
label(data$time_to_intubation)="Time to intubation: __ __ __ sec  (Started with laryngoscope introduced into oral cavity and cuff insufflation, and ended when first appearance of end-tidal CO2)"
label(data$endotracheal_pressure)="Endotracheal tube cuff pressure: _______cm H2O"
label(data$ease_of_intubation)="Ease of intubation: "
label(data$intubation_failure___1)="Intubation failure:  (choice=Grade IV visualization)"
label(data$intubation_failure___2)="Intubation failure:  (choice=Failure to intubate within 3 intubation attempts)"
label(data$intubation_failure___3)="Intubation failure:  (choice=Need to switch providers or intubation device)"
label(data$intubation_failure___4)="Intubation failure:  (choice=Need to stop study per anesthesiologists discretion.)"
label(data$intubation_failure___98)="Intubation failure:  (choice=Successful)"
label(data$intubation_failure___9999)="Intubation failure:  (choice=Missing/Not asked)"
label(data$intubation_failure___8888)="Intubation failure:  (choice=Not Applicable)"
label(data$intubation_complete)="Complete?"
label(data$username_for_complications)="username_for_complications"
label(data$complications_skip)="If form(s) can not be completed please select an item from the class reasons in the dropdown list."
label(data$teeth_injury)="1- Any obvious airway and teeth injury (If yes, check all that apply)"
label(data$teeth_injury_specify___1)="Specify Teeth injury (check all that apply) (choice=Cut lip)"
label(data$teeth_injury_specify___2)="Specify Teeth injury (check all that apply) (choice=Bleeding)"
label(data$teeth_injury_specify___3)="Specify Teeth injury (check all that apply) (choice=Airway trauma)"
label(data$teeth_injury_specify___4)="Specify Teeth injury (check all that apply) (choice=Bronchospasm)"
label(data$teeth_injury_specify___9999)="Specify Teeth injury (check all that apply) (choice=Missing/Not asked)"
label(data$teeth_injury_specify___8888)="Specify Teeth injury (check all that apply) (choice=Not Applicable)"
label(data$cough)="2- Cough (if yes, specify)"
label(data$cough_specify)="Specify Cough "
label(data$sore_throat)="3- Sore throat (if yes, specify) "
label(data$sore_throat_specify)="Specify Sore throat"
label(data$hoarseness)=" 4- Hoarseness (if yes, specify)"
label(data$hoarseness_specify)="Specify Hoarseness"
label(data$postoperative_complications_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$redcap_event_name.factor = factor(data$redcap_event_name,levels=c("pod_0_arm_1","not_time_dependent_arm_1"))
data$eligible.factor = factor(data$eligible,levels=c("1","0"))
data$consented.factor = factor(data$consented,levels=c("1","0"))
data$eligibility_and_enrollment_complete.factor = factor(data$eligibility_and_enrollment_complete,levels=c("0","1","2"))
data$gender.factor = factor(data$gender,levels=c("1","0"))
data$asa.factor = factor(data$asa,levels=c("1","2","3"))
data$race___8.factor = factor(data$race___8,levels=c("0","1"))
data$race___2.factor = factor(data$race___2,levels=c("0","1"))
data$race___3.factor = factor(data$race___3,levels=c("0","1"))
data$race___99.factor = factor(data$race___99,levels=c("0","1"))
data$race___9999.factor = factor(data$race___9999,levels=c("0","1"))
data$race___8888.factor = factor(data$race___8888,levels=c("0","1"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))
data$preoperative_skip.factor = factor(data$preoperative_skip,levels=c("1","2","3","4","5"))
data$mallampati_score.factor = factor(data$mallampati_score,levels=c("1","2","3","4"))
data$mobility_cervical_spine.factor = factor(data$mobility_cervical_spine,levels=c("0","15","30","45"))
data$upper_lip_bite_test_class.factor = factor(data$upper_lip_bite_test_class,levels=c("1","2","3"))
data$mandibular_protrusion_test.factor = factor(data$mandibular_protrusion_test,levels=c("1","2","3"))
data$edentulous.factor = factor(data$edentulous,levels=c("96","0","1","2"))
data$obstructive_sleep_apnea.factor = factor(data$obstructive_sleep_apnea,levels=c("1","0"))
data$snoring.factor = factor(data$snoring,levels=c("1","0"))
data$cpap_use.factor = factor(data$cpap_use,levels=c("1","0"))
data$difficult_airway.factor = factor(data$difficult_airway,levels=c("1","0"))
data$preoperative_airway_examination_complete.factor = factor(data$preoperative_airway_examination_complete,levels=c("0","1","2"))
data$intubation_skip.factor = factor(data$intubation_skip,levels=c("1","2","3","4","5"))
data$randomization.factor = factor(data$randomization,levels=c("1","0"))
data$provider.factor = factor(data$provider,levels=c("1","2"))
data$stylette_use.factor = factor(data$stylette_use,levels=c("1","0"))
data$burp.factor = factor(data$burp,levels=c("1","0"))
data$sellick.factor = factor(data$sellick,levels=c("1","0"))
data$cormacklehane_grading.factor = factor(data$cormacklehane_grading,levels=c("1","2.1","2.2","3","4"))
data$pogo_score.factor = factor(data$pogo_score,levels=c("0","25","50","75","100"))
data$intubation_attempts.factor = factor(data$intubation_attempts,levels=c("1","2","3","4"))
data$ease_of_intubation.factor = factor(data$ease_of_intubation,levels=c("1","2","3","4","5"))
data$intubation_failure___1.factor = factor(data$intubation_failure___1,levels=c("0","1"))
data$intubation_failure___2.factor = factor(data$intubation_failure___2,levels=c("0","1"))
data$intubation_failure___3.factor = factor(data$intubation_failure___3,levels=c("0","1"))
data$intubation_failure___4.factor = factor(data$intubation_failure___4,levels=c("0","1"))
data$intubation_failure___98.factor = factor(data$intubation_failure___98,levels=c("0","1"))
data$intubation_failure___9999.factor = factor(data$intubation_failure___9999,levels=c("0","1"))
data$intubation_failure___8888.factor = factor(data$intubation_failure___8888,levels=c("0","1"))
data$intubation_complete.factor = factor(data$intubation_complete,levels=c("0","1","2"))
data$complications_skip.factor = factor(data$complications_skip,levels=c("1","2","3","4","5"))
data$teeth_injury.factor = factor(data$teeth_injury,levels=c("1","0"))
data$teeth_injury_specify___1.factor = factor(data$teeth_injury_specify___1,levels=c("0","1"))
data$teeth_injury_specify___2.factor = factor(data$teeth_injury_specify___2,levels=c("0","1"))
data$teeth_injury_specify___3.factor = factor(data$teeth_injury_specify___3,levels=c("0","1"))
data$teeth_injury_specify___4.factor = factor(data$teeth_injury_specify___4,levels=c("0","1"))
data$teeth_injury_specify___9999.factor = factor(data$teeth_injury_specify___9999,levels=c("0","1"))
data$teeth_injury_specify___8888.factor = factor(data$teeth_injury_specify___8888,levels=c("0","1"))
data$cough.factor = factor(data$cough,levels=c("1","0"))
data$cough_specify.factor = factor(data$cough_specify,levels=c("1","2","3"))
data$sore_throat.factor = factor(data$sore_throat,levels=c("1","0"))
data$sore_throat_specify.factor = factor(data$sore_throat_specify,levels=c("1","2","3"))
data$hoarseness.factor = factor(data$hoarseness,levels=c("1","0"))
data$hoarseness_specify.factor = factor(data$hoarseness_specify,levels=c("1","2","3"))
data$postoperative_complications_complete.factor = factor(data$postoperative_complications_complete,levels=c("0","1","2"))

levels(data$redcap_event_name.factor)=c("POD 0","Not time dependent")
levels(data$eligible.factor)=c("Yes","No")
levels(data$consented.factor)=c("Yes","No")
levels(data$eligibility_and_enrollment_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$gender.factor)=c("Female","Male")
levels(data$asa.factor)=c("I","II","III")
levels(data$race___8.factor)=c("Unchecked","Checked")
levels(data$race___2.factor)=c("Unchecked","Checked")
levels(data$race___3.factor)=c("Unchecked","Checked")
levels(data$race___99.factor)=c("Unchecked","Checked")
levels(data$race___9999.factor)=c("Unchecked","Checked")
levels(data$race___8888.factor)=c("Unchecked","Checked")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$preoperative_skip.factor)=c("This form did not get completed today.","Not done because patient refused","The patient was discharged.","The patient withdrew.","Not applicable or not available for this patient.")
levels(data$mallampati_score.factor)=c("I","II","III","IV")
levels(data$mobility_cervical_spine.factor)=c("0","15","30","45")
levels(data$upper_lip_bite_test_class.factor)=c("I","II","III")
levels(data$mandibular_protrusion_test.factor)=c("A","B","C")
levels(data$edentulous.factor)=c("No missing teeth","Edentulous","Missing frontal teeth","Full denture")
levels(data$obstructive_sleep_apnea.factor)=c("Yes","No")
levels(data$snoring.factor)=c("Yes","No")
levels(data$cpap_use.factor)=c("Yes","No")
levels(data$difficult_airway.factor)=c("Yes","No")
levels(data$preoperative_airway_examination_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$intubation_skip.factor)=c("This form did not get completed today.","Not done because patient refused","The patient was discharged.","The patient withdrew.","Not applicable or not available for this patient.")
levels(data$randomization.factor)=c("Direct laryngoscopy","McGrath videolaryngoscope")
levels(data$provider.factor)=c("Specialist, Attending","Trainees")
levels(data$stylette_use.factor)=c("Yes","No")
levels(data$burp.factor)=c("Yes","No")
levels(data$sellick.factor)=c("Yes","No")
levels(data$cormacklehane_grading.factor)=c("1","2a","2b","3","4")
levels(data$pogo_score.factor)=c("0%","25%","50%","75%","100%")
levels(data$intubation_attempts.factor)=c("1","2","3","4 or more")
levels(data$ease_of_intubation.factor)=c("Very easy","Easy","Moderate","Difficult","Impossible")
levels(data$intubation_failure___1.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___2.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___3.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___4.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___98.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___9999.factor)=c("Unchecked","Checked")
levels(data$intubation_failure___8888.factor)=c("Unchecked","Checked")
levels(data$intubation_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$complications_skip.factor)=c("This form did not get completed today.","Not done because patient refused","The patient was discharged.","The patient withdrew.","Not applicable or not available for this patient.")
levels(data$teeth_injury.factor)=c("Yes","No")
levels(data$teeth_injury_specify___1.factor)=c("Unchecked","Checked")
levels(data$teeth_injury_specify___2.factor)=c("Unchecked","Checked")
levels(data$teeth_injury_specify___3.factor)=c("Unchecked","Checked")
levels(data$teeth_injury_specify___4.factor)=c("Unchecked","Checked")
levels(data$teeth_injury_specify___9999.factor)=c("Unchecked","Checked")
levels(data$teeth_injury_specify___8888.factor)=c("Unchecked","Checked")
levels(data$cough.factor)=c("Yes","No")
levels(data$cough_specify.factor)=c("Mild (less than a common cold)","Moderate (similar to a cold)","Severe (more than a common cold)")
levels(data$sore_throat.factor)=c("Yes","No")
levels(data$sore_throat_specify.factor)=c("Mild (less than a common cold)","Moderate (similar to a cold)","Severe (more than a common cold)")
levels(data$hoarseness.factor)=c("Yes","No")
levels(data$hoarseness_specify.factor)=c("Noticed by the patient only","Apparent to an observer","Aphonia")
levels(data$postoperative_complications_complete.factor)=c("Incomplete","Unverified","Complete")

data <- data %>%
  mutate(race.categorical = case_when(
    as.logical(race___8) ~ "White",
    as.logical(race___2) ~ "Black",
    as.logical(race___3) ~ "Hispanic",
    as.logical(race___99) ~ "Other",
    as.logical(race___9999) ~ "Missing or not asked",
    as.logical(race___8888) ~ "Not applicable")) %>%
  mutate(intubation_failure.categorical = case_when(
    as.logical(intubation_failure___1) ~ "Grade IV visualization",
    as.logical(intubation_failure___2) ~ "Failure to intubate after 3 attempts",
    as.logical(intubation_failure___3) ~ "Need to switch providers or intubation device",
    as.logical(intubation_failure___4) ~ "Need to stop study (at anesthesiologist's discretion)",
    as.logical(intubation_failure___98) ~ "Intubation successful",
    as.logical(intubation_failure___9999) ~ "Missing/not asked",
    as.logical(intubation_failure___8888) ~ "Not applicable")) %>%
  mutate(teeth_injury_specify.categorical = case_when(
    as.logical(teeth_injury_specify___1) ~ "Cut lip",
    as.logical(teeth_injury_specify___2) ~ "Bleeding",
    as.logical(teeth_injury_specify___3) ~ "Airway trauma",
    as.logical(teeth_injury_specify___4) ~ "Bronchospasm",
    as.logical(teeth_injury_specify___9999) ~ "Missing/not asked",
    as.logical(teeth_injury_specify___8888) ~ "Not applicable"))
