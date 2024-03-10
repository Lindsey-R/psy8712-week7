# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart)) %>%
  mutate(condition = recode_factor(condition, "A" = "Block A", "B" = "Block B", "C" = "Control")) %>%
  mutate(gender = recode_factor(gender,"M" = "Male", "F" = "Female")) %>%
  filter(q6 == 1) %>%
  select(-q6) %>%
  mutate(timeSpent = timeEnd - timeStart) %>%
  mutate(timeSpent = as.numeric(timeSpent))




# Visualization
select(week7_tbl, q1:q10) %>%
  ggpairs()

(ggplot(week7_tbl, aes(x = timeStart, y = q1)) +
  geom_point() +
  labs(x = "Date of Experiment", y = "Q1 Score")) %>%
  ggsave("../figs/fig1.png", ., dpi = 600, height = 3, width = 6)


(ggplot(week7_tbl,aes(x = q1, y = q2, color = gender)) +
  geom_jitter()+
    labs(color="Participant Gender")) %>%
  ggsave("../figs/fig2.png", ., dpi = 600, height = 4, width = 6.5)

(ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_jitter() +
  facet_grid(cols = vars(gender)) +
  labs(x = "Score on Q1", y = "Score on Q2")) %>%
  ggsave("../figs/fig3.png", ., dpi = 600, height = 4, width = 8)


(ggplot(week7_tbl, aes(x = gender, y = timeSpent)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Time Elapsed (mins)")) %>%
  ggsave("../figs/fig4.png", ., dpi = 600, height = 4, width = 8)
 

(ggplot(week7_tbl, aes(x = q5, y = q7, color = condition)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Score on Q5", y = "Score on Q7", color ="Experimental Condition") +
  theme(legend.position =  "bottom", 
        legend.background = element_rect(fill = "#DFDFDF"))) %>% #grey(0.875)
  ggsave("../figs/fig5.png", ., dpi = 600, height = 4, width = 7)

