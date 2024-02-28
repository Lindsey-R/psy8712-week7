# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggplot2)
library(GGally)


# Data Import and Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart)) %>%
  mutate(condition = recode_factor(condition, "A" = "Block A", "B" = "Block B", "C" = "Control")) %>%
  mutate(gender = recode_factor(gender,"F" = "Female", "M" = "Male")) %>%
  filter(q6 == 1) %>%
  select(-q6) %>%
  mutate(timeSpent = timeEnd - timeStart)





# Visualization
week7_tbl %>%
  select(q1:q10) %>%
  ggpairs()
week7_tbl %>%
  ggplot(aes(x = timeStart, y = q1)) +
  geom_point() +
  labs(x = "Date of Experiment", y = "Q1 Score")