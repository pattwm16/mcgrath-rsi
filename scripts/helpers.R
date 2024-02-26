# EDA helpers
# global libraries
require(labelled)
require(janitor)
require(latex2exp)
require(tidyverse)


# Define a function to create density plots
create_density_plot <- function(data, x_var, fill_var, x_label = NULL, title = NULL, directory = "figs/eda/") {
  if (is.null(x_label)) {
    x_label <- deparse(substitute(x_var))
  }
  
  if (is.null(title)) {
    if (is.null(x_label)) {
      title <- "Density Plot"
    } else {
      title <- paste("Density Plot of", x_var)
    }
  }
  
  plot <- ggplot(data, aes(x = eval(as.name(x_var)), fill = {{fill_var}})) +
    geom_density(alpha = 0.5) +
    labs(title = title,
         x = as.name(x_var),
         y = "",
         fill = "Intervention") +
    theme_classic()
  
  # Save the plot
  #filename <- paste0(directory, gsub("\\s", "_", x_var), ".png")
  #ggsave(filename, plot, width = 6, height = 4)
  
  return(plot)
}

calculate_stddiff <- function(data, group) {
  results <- list()
  t <- data %>% 
    mutate(trt = as.numeric(randomized_to) - 1) %>%
    select(-randomized_to) %>%
    droplevels()
  
  vars <- colnames(t)[!colnames(t) %in% c("uid", "trt")]
  
  for (var in vars) {
    if (is.numeric(data[[var]])) {
      result <- stddiff.numeric(t, grep("trt", colnames(t)), vcol = grep(var, colnames(t)))
    } else if (is.factor(data[[var]])) {
      result <- stddiff.category(t, grep("trt", colnames(t)), vcol = grep(var, colnames(t)))
    } else if (is.logical(data[[var]])) {
      result <- stddiff.binary(t, grep("trt", colnames(t)), vcol = grep(var, colnames(t)))
      
    } else {
      warning(paste("Variable", var, "is of unsupported type. Skipping..."))
      next
    }
    
    result_df <- as.data.frame(result)  # Convert matrix to data frame
    results[[var]] <- result_df
  }
  
  combined_results <- do.call(rbind, lapply(results, function(x) x %>% filter(row_number() == 1) %>% select(stddiff, stddiff.l, stddiff.u)))
  return(combined_results)
}


data_labels <- c(age = "Age (years)",
                 gender = "Sex",
                 mallampati_score = "Mallampati score",
                 mobility_cervical_spine = "Mobility of the cervical spine (degrees)",
                 upper_lip_bite_test_class = "Upper lip bite test class",
                 mandibular_protrusion_test = "Mandibular protrusion test",
                 difficult_airway_hx_yn = "History of difficult airway",
                 asa_class = "ASA physical status",
                 height_cm = "Height (cm)",
                 weight_kg = "Weight (kg)",
                 bmi = "Body mass index (BMI)",
                 inter_incisor_gap_cm = "Inter-incisor gap (cm)",
                 mouth_opening_cm = "Mouth opening (cm)",
                 thyromental_distance_cm = "Thyromental distance (cm)",
                 thyromental_height_cm = "Theromental height (cm)",
                 sternomental_distance_cm = "Sternomental distance (cm)",
                 sternomental_height_cm = "Sternomental height (cm)",
                 neck_circumference_cm = "Neck circumference (cm)",
                 endotracheal_tube_number = "Endotracheal tube number",
                 time_to_intubbation = "Time to intubation (seconds)",
                 ett_pressure_cmh2o = "Endotracheal tube pressure ($cmH_2O$)",
                 teeth_status = "Teeth status prior to intubation",
                 intub_fail_reason_n = "Number of reasons for intubation failure",
                 teeth_injury_n = "Number of teeth injuries",
                 cpap_use_yn = "Use of CPAP",
                 snoring_yn = "History of snoring",
                 osa_yn = "Diagnosed obstructive sleep apnea"
)
