# Clear environment
rm(list=ls())

# R Packages
if (!require('pacman')) install.packages('pacman'); library(pacman)
p_load("dplyr", "ggplot2", "lubridate")

# Data
preg_data <- read.csv("Pregnancy/NationalAndStatePregnancy_PublicUse.csv")

preg_filtered <- preg_data %>%
  select("state", "year", "pregnancyratetotal", "pregnanciestotal") %>%
  filter(
    state %in% c("MN", "US")
  )

# Average # of pregnancies from 1973-2020 for US and 1988-2020 for MN
preg_average <- preg_filtered %>%
  group_by(state) %>%
  summarize(
    avg_pregnancies = mean(pregnanciestotal, na.RM = TRUE)
  )

print(preg_average)

# # of pregnancies for just 2020 in US and MN
preg_2020 <- preg_filtered %>%
  filter(year == 2020)

print(preg_2020)
