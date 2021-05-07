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
library(refund)
library(mgcv)
library(lme4)
library(zoo)
library(ggcorrplot)
library(face)

### Load in cleaned data
step <- readRDS("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/FitBit_steps_1AcademicYear_long.rds")
dem <- read.csv("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/Demographics.csv")
step_wide <- readRDS("D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/FitBit_steps_1AcademicYear_wide.rds")

### Complete step day by including missing md values
step <- step %>% 
   mutate(md = as.numeric(md), datadate = as.Date(datadate, format = "%Y-%m-%d")) %>% 
   group_by(egoid, AcademicYr, days_yr, maxMissing) %>% 
   tidyr::complete(md = 1:366) %>% 
   mutate(sind = md/366)

### Fill in date 
step$datadate <- as.Date(ifelse(step$AcademicYr == "2016-2017" & is.na(step$datadate),
  as.Date("2016-08-01", format = "%Y-%m-%d")  + step$md - 1,
  ifelse(step$AcademicYr == "2015-2016" & is.na(step$datadate),
    as.Date("2015-08-01", format = "%Y-%m-%d")  + step$md - 1,
    ifelse(step$AcademicYr == "2017-2018" & is.na(step$datadate),
      as.Date("2017-08-01", format = "%Y-%m-%d")  + step$md - 1,
      ifelse(step$AcademicYr == "2018-2019" & is.na(step$datadate),
        as.Date("2018-08-01", format = "%Y-%m-%d")  + step$md - 1,step$datadate)))))

### Get day of week
step$dow <- weekdays(as.Date(step$datadate))

### Indicator for weekday
step$weekday <- ifelse(step$dow %in% c("Saturday", "Sunday"), 0, 1)

### Add college, gender, race, socio-economic status and BMI
step$College <- dem$College[match(step$egoid, dem$egoid)]
step$Gender <- dem$gender_1[match(step$egoid, dem$egoid)]
step$Race <- dem$race_1[match(step$egoid, dem$egoid)]
step$Income <- dem$Income[match(step$egoid, dem$egoid)]
step$BMI <- dem$BMI[match(step$egoid, dem$egoid)]

### Remove missing college 
step <- step[!is.na(step$College),]

### Create class indicator variables 
CollegeInd <- model.matrix( ~ College - 1, data=step )
GenderInd <- model.matrix( ~ Gender - 1, data = step )
RaceInd <- model.matrix( ~ Race - 1, data = step )
IncomeInd <- model.matrix( ~ Income - 1, data = step )

### Combine data
Indicators = data.frame(cbind(CollegeInd, GenderInd, RaceInd, IncomeInd))
step <- data.frame(cbind(step, Indicators))

saveRDS(step, "D:/CU/Spring 2021/FDA/Final Project/BIOS7720_FinalProject/Data/DataProcessed/Step_Fit.rds")

#######################################
### FPCA MODEL (TIME-VARYING TREND) ###
#######################################

step_complete <- step[!is.na(step$steps) & !is.na(step$Gender) & !is.na(step$Race) & !is.na(step$Income) & !is.na(step$BMI),]

# Fit a naive model not accounting for correlation
fit_naive <- bam(steps ~ s(sind, bs="cr",k=30) + s(sind, by = CollegeArchitecture, bs="cr", k=30) + 
    s(sind, by = CollegeArts.and.Letters, bs="cr", k=30) + s(sind, by = CollegeBusiness, bs="cr", k=30) + 
    s(sind, by = CollegeScience, bs="cr", k=30) + s(sind, by = GenderMale, bs="cr", k=30) + 
    s(sind, by = RaceAfrican.American, bs="cr", k=30) + s(sind, by = RaceAsian.American, bs="cr", k=30) +
    s(sind, by = RaceForeign.Student, bs="cr", k=30) + s(sind, by = RaceLatino.a, bs="cr", k=30) +
    s(sind, by = RaceOther, bs="cr", k=30) + s(sind, by = Income.50.000....150.000, bs="cr", k=30) + 
    s(sind, by = Income...150.000, bs="cr", k=30) + s(sind, by = weekday, bs="cr", k=30) + 
    s(sind, by = BMI, bs="cr", k=30), 
    method="fREML", data=step_complete, discrete=TRUE)

# Save residuals
resid_dat <- data.frame(cbind(".id" = step_complete$egoid, 
  ".index" = step_complete$md, ".value" = fit_naive$residuals))

# Extract eigenfunctions 
# step_mat <- as.matrix(step_wide[,-c(1:4)])
resid_fpca <- fpca.sc(ydata = resid_dat)
Phi_hat <- resid_fpca$efunctions

# refund.shiny::plot_shiny(Steps_fpca) # Plot to see eigenfunctions

# Include first 4 eigenfunctions in data
N = length(unique(step$egoid))
step$Phi1 <- rep(Phi_hat[,1], N)
step$Phi2 <- rep(Phi_hat[,2], N)
step$Phi3 <- rep(Phi_hat[,3], N)
step$Phi4 <- rep(Phi_hat[,4], N)

# Fit a random functional intercept model
fit_rfi <- bam(steps ~ s(sind, bs="cr",k=30)  + s(sind, by = CollegeArchitecture, bs="cr", k=30) + 
    s(sind, by = CollegeArts.and.Letters, bs="cr", k=30) + s(sind, by = CollegeBusiness, bs="cr", k=30) + 
    s(sind, by = CollegeScience, bs="cr", k=30) + s(sind, by = GenderMale, bs="cr", k=30) + 
    s(sind, by = RaceAfrican.American, bs="cr", k=30) + s(sind, by = RaceAsian.American, bs="cr", k=30) +
    s(sind, by = RaceForeign.Student, bs="cr", k=30) + s(sind, by = RaceLatino.a, bs="cr", k=30) +
    s(sind, by = RaceOther, bs="cr", k=30) + s(sind, by = Income.50.000....150.000, bs="cr", k=30) + 
    s(sind, by = Income...150.000, bs="cr", k=30) + s(sind, by = weekday, bs="cr", k=30) + 
    s(sind, by = BMI, bs="cr", k=30) + s(egoid, by = Phi1, bs="cr", k=30) + 
    s(egoid, by = Phi2, bs="cr", k=30) + s(egoid, by = Phi3, bs="cr", k=30) + 
    s(egoid, by = Phi4, bs="cr", k=30), method="fREML", data=step, discrete=TRUE)


fhat_naive <- predict(fit_naive, newdata=step, se.fit=TRUE,type='terms')
fhat_rfi   <- predict(fit_rfi, newdata=step, se.fit=TRUE,type='terms')

fhat_rfi$fit



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



