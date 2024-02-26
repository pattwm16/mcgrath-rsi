# safety outcomes reported by study groups

source("scripts/helpers.R")
load("data/cleaned-data.RData")

#1. Any apparent airway or dental injury 
#   i.e., bleeding, airway trauma, dental fracture, aspiration, or bronchospasm.
# TODO: no dental frature or aspiration in teeth_injury_specify
(teeth_injury_plot <- data %>%
  mutate(teeth_injury_specify = fct_relevel(teeth_injury_specify, "No injury")) %>%
  #filter(teeth_injury_specify != "No injury") %>%
  ggplot(aes(x = randomized_to, fill = teeth_injury_specify)) +
  geom_bar(position="dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  labs(x = "", y = "", fill = "Airway / dental injury") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave(teeth_injury_plot, filename = "figs/teeth_injury_byRandomization.png", 
         width = 10, height = 6, bg="white")

# Incidence and severity of postoperative cough 
# rated as mild (< common cold), moderate (~common cold), or severe 
#(more than a common cold).30,31
(cough_plot <- data %>%
  ggplot(aes(x = randomized_to, fill = cough_severity)) +
  geom_bar(position="dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  labs(x = "", y = "", fill = "Cough") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = c(.98, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave(filename = "figs/cough_severity_byRandomization.png", 
         width = 10, height = 6, bg="white")

#Incidence and severity of postoperative sore throat, assessed after 2 hours of 
#extubation in the PACU, defined as an acoustic quality that was different from 
#the previous voice quality of the patients and rated as mild 
#(less than a common cold), moderate (similar to a common cold), or severe 
#(more than a common cold).30,31

(sorethroat_plot <- data %>%
  ggplot(aes(x = randomized_to, fill = sore_throat_severity)) +
  geom_bar(position="dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  labs(x = "", y = "", fill = "Sore throat") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = c(.98, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave(filename = "figs/sorethroat_severity_byRandomization.png", 
         width = 10, height = 6, bg="white")

#Incidence and severity of postoperative hoarseness, assessed after 2 hours of 
#extubation in the PACU, and rated as noticed by the patient only, apparent to 
#an observer, or aphonia.30,31

(hoarseness_plot <- data %>%
  ggplot(aes(x = randomized_to, fill = hoarseness_severity)) +
  geom_bar(position="dodge", color = 'black') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +  # Add count labels
  labs(x = "", y = "", fill = "Hoarseness") +
  theme_linedraw(base_size = 15) +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = c(.98, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 6, 6, 6),
    legend.box.background = element_rect(),
    panel.grid.major.x = element_blank()
  ))
ggsave(filename = "figs/hoarseness_severity_byRandomization.png", 
         width = 10, height = 6, bg="white")

# arrange all as one plot
ggpubr::ggarrange(teeth_injury_plot, cough_plot, sorethroat_plot, hoarseness_plot, ncol = 2, nrow = 2)
ggsave(filename = "figs/safety_outcomes_byRandomization.png", 
         width = 20, height = 15, bg="white")
