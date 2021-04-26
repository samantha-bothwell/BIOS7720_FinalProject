##################################################
### BIOS 7720 Final Project                    ###
### Group : Samantha Bothwell                  ###
###         Ilana Trumble                      ###
###         Suneeta Godbole                    ###
### Professor : Andrew Leroux                  ###
### Date Modified : April 16th, 2021           ###
###                                            ###
### The purpose of this project is to analyze  ###
### functional data with FDA methods           ###
###                                            ###
### The purpose of this file is to visualize   ###
### step and sleep patterns                    ###
##################################################

rm(list = ls()) 

### Load packages
library(tidyverse)
library(stringr)
library(ggplot2)

### Load in data
step <- readRDS("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/FitBit_steps_1AcademicYear_long.rds")
sleep <- readRDS("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/Fitbit_sleep_1AcademicYr_long_matched.rds")
dem <- read.csv("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataRaw/BasicSurvey(3-6-20).csv")

### STEP COUNT BY COLLEGE 
# Clean and add college
dem$major1rc_1 <- sapply(strsplit(as.character(dem$major1rc_1), ""), `[`, 1)
dem$College <- factor(dem$major1rc_1, levels = c("1","2","3","4","5","6"), 
  labels = c("Arts and Letters", "Architecture", "Engineering", "Business", "Science", "Undecided"))
step$College <- dem$College[match(step$egoid, dem$egoid)]

# Organize data 
step_college <- step %>% 
  mutate(md = as.numeric(md)) %>% 
  filter(!is.na(College)) %>% 
  group_by(md, College) %>% 
  dplyr::summarise(step.mn = mean(steps, na.rm = T))

# plot
ggplot(step_college, aes(x = md, y = step.mn, color = College)) +
  geom_line() + ylab("Mean Step Count") + xlab("Academic Day")



### STEP AND SLEEP PLOT
# Organize data
step_mean <- step %>% 
  mutate(md = as.numeric(md)) %>% 
  group_by(md) %>% 
  dplyr::summarise(step.mn = mean(steps, na.rm = T))

sleep_mean <- sleep %>% 
  mutate(md = as.numeric(md), sleep.hrs = tot_min_sleep/60) %>% 
  group_by(md) %>% 
  dplyr::summarise(sleep.mn = mean(sleep.hrs, na.rm = T))

# Merge
mean_dat <- merge(sleep_mean, step_mean, by = "md")

# Make sure we can get two y-axes
ylim.step <- c(min(mean_dat$step.mn), max(mean_dat$step.mn)) # step y limits
ylim.sleep <- c(min(mean_dat$sleep.mn), max(mean_dat$sleep.mn))  # sleep y limits

b <- diff(ylim.step)/diff(ylim.sleep)
a <- ylim.step[1] - b*ylim.sleep[1]

# Assign colors and plot
colors <- c("Steps" = "turquoise4", "Sleep Hours" = "indianred3")
ggplot(mean_dat, aes(x = md, y = step.mn)) + 
  geom_line(aes(color = "Steps"), size = 1) + 
  geom_line(aes(y = a + sleep.mn*b , color = "Sleep Hours"), size = 1) + 
  scale_y_continuous("Mean Step Count", sec.axis = sec_axis(~ (. - a)/b, name = "Mean Sleep Hours")) + 
  labs(x = "Academic Day")+
  scale_color_manual(values = colors) +
  theme(legend.title = element_blank(), legend.position = c(0.925, 0.91))
