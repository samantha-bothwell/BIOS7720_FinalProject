##################################################################################
### BIOS 7720 Final Project                                                    ###
### Group : Samantha Bothwell                                                  ###
###         Ilana Trumble                                                      ###
###         Suneeta Godbole                                                    ###
### Professor : Andrew Leroux                                                  ###
### Date Modified : April 30th, 2021                                           ###
###                                                                            ###
### The purpose of this project is to analyze functional data with FDA methods ###
###                                                                            ###
### The purpose of this file is to fit a FoSR for time linked trends and a     ###
### linear model for aggregate trends. The models will include step count as   ###
### the outcome and student college as the primary predictor.                  ###
### NOTE: Our data is sparse so we will compare fpca.sc to methods in the face ###
###       package.                                                             ###
##################################################################################

rm(list = ls()) 

### Load packages
library(tidyverse)
library(stringr)
library(ggplot2)

### Load in cleaned data
step <- readRDS("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/FitBit_steps_1AcademicYear_long.rds")
dem <- read.csv("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/Demographics.csv")

### Complete step day by including missing md values
step <- step %>% 
  mutate(md = as.numeric(md), datadate = as.Date(datadate, format = "%Y-%m-%d")) %>% 
  group_by(egoid, AcademicYr, days_yr, maxMissing) %>% 
  tidyr::complete(md = 1:366) %>% 
  mutate

### Add college, gender, race, socio-economic status and BMI
step$College <- dem$College[match(step$egoid, dem$egoid)]
step$Gender <- dem$gender_1[match(step$egoid, dem$egoid)]
step$Race <- dem$race_1[match(step$egoid, dem$egoid)]
step$Income <- dem$Income[match(step$egoid, dem$egoid)]
step$BMI <- dem$BMI[match(step$egoid, dem$egoid)]


#######################################
### FPCA MODEL (TIME-VARYING TREND) ###
#######################################

# Fit a naive model not accounting for correlation
fit_naive <- bam(steps ~ s(sind, bs="cr",k=30) + s(sind, by=Age, bs="cr",k=30) + 
    s(sind, by = Charlson, bs="cr", k=30), method="fREML", data=df_fit, 
  discrete=TRUE)


######################################
### LINEAR MODEL (AGGREGATE TREND) ###
######################################

### Summarize average 
step_avg <- step %>% 
  group_by(egoid, College, Gender, Race, Income, BMI) %>% 
  dplyr::summarize(mn.step = mean(steps, na.rm = T))

### Linear model 
lm.mod <- lm(mn.step ~ College + Gender + Race + Income + BMI, data = step_avg)
summary(lm.mod)



