# exploratory outcomes
source("scripts/helpers.R")
load("data/cleaned-data.RData")

file_path <- 'figs/exploratory/' # path for figures
## 1.	Ease of intubation ----
data %>%
  filter(!is.na(ease_of_intubation)) %>%  # Remove missing data
  ggplot(aes(x = ease_of_intubation, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(#title = "Ease of intubation by randomization group",
       caption = paste0("Missing data for ", data %>% filter(is.na(ease_of_intubation)) %>% nrow(), " patients"),
       x = "Randomization group",
       y = "Ease of intubation") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect()
  )
ggsave(paste0(file_path, "ease_of_intubation_distribution.png"), 
       width = 8, height = 6, bg = "white")

data %>%
  group_by(randomized_to) %>%
  summarise(median = median(as.numeric(ease_of_intubation), na.rm = TRUE))

## 2.	Glottis opening score (POGO) score ----
data %>%
  filter(!is.na(pogo_score)) %>%  # Remove missing data
  ggplot(aes(x = pogo_score, fill = randomized_to)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_bar(position = "dodge", color = 'black') +
  labs(#title = "POGO score by randomization group",
       caption = paste0("Missing data for ", data %>% filter(is.na(pogo_score)) %>% nrow(), " patients"),
       x = "Randomization group",
       y = "POGO score") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_blank(),
    legend.position = c(.45, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(-4, 6, 6, 6),
    legend.box.background = element_rect()
  )
ggsave(paste0(file_path, "pogo_score_distribution.png"), 
       width = 8, height = 6, bg = "white")

## 3.	Duration of intubation ----
data %>%
  filter(!is.na(time_to_intubation)) %>%  # Remove missing data
  ggplot(aes(y = time_to_intubation, x = randomized_to, fill = randomized_to)) +
  #geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  geom_boxplot(color = 'black') +
  labs(title = "Duration of intubation by randomization group",
       caption = paste0("Missing data for ", data %>% filter(is.na(time_to_intubation)) %>% nrow(), " patients"),
       x = "",
       y = "Duration of intubation (seconds)") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.position = "none",
  )
ggsave(paste0(file_path, "time_to_intubation_distribution.png"), 
       width = 8, height = 6, bg = "white")

# examine risk 
survfit(Surv(time_to_intubation, intub_success) ~ randomized_to,
        data = data) %>% 
  ggsurvfit(type = 'risk') +
  add_quantile(y_value = 0.5) + # add median survival
  add_confidence_interval() +
  add_risktable(risktable_group='risktable_stats') +
  scale_ggsurvfit() +
  add_risktable_strata_symbol(symbol = "\U25CF", size = 10) +
  labs(x = "Time (seconds)", y = "Cumulative incidence", 
       caption = paste0("Missing data for ", data %>% filter(is.na(time_to_intubation)) %>% nrow(), " patients"))
ggsave(paste0(file_path, "time_to_intubation_risk.png"), 
       width = 10, height = 6, bg = "white")

# what are the median 'intubation-free' survival times?
tbl_survfit(
  data,
  y = Surv(time_to_intubation, intub_success),
  include = c(randomized_to),
  probs = 0.5,
  label_header = "**Median Survival (seconds)**",
  label = data_labels
)
