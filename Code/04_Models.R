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

# refund.shiny::plot_shiny(resid_dat) # Plot to see eigenfunctions

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

df_pred <- data.frame(sind = step$sind, egoid = step$egoid[1], CollegeArchitecture = 1, 
  CollegeArts.and.Letters = 1, CollegeBusiness = 1, CollegeScience = 1, GenderMale = 1, 
  RaceAfrican.American = 1, RaceAsian.American = 1, RaceForeign.Student = 1, RaceLatino.a = 1,
  RaceOther = 1, Income.50.000....150.000 = 1, Income...150.000 = 1, weekday = 1, BMI = 1, 
  Phi1 = 0, Phi2 = 0, Phi3 = 0, Phi4 = 0)

fhat_naive <- predict(fit_naive, newdata=df_pred, se.fit=TRUE,type='terms')
fhat_rfi   <- predict(fit_rfi, newdata=df_pred, se.fit=TRUE,type='terms')

# Save estimates 
Arch_hat <- fhat_rfi$fit[,2]; Arch_se <- fhat_rfi$se.fit[,2]
AaL_hat <- fhat_rfi$fit[,3]; AaL_se <- fhat_rfi$se.fit[,3]
Bus_hat <- fhat_rfi$fit[,4]; Bus_se <- fhat_rfi$se.fit[,4]
Sci_hat <- fhat_rfi$fit[,5]; Sci_se <- fhat_rfi$se.fit[,5]
Arch_AaL_hat <- fhat_rfi$fit[,5]; Arch_AaL_se <- fhat_rfi$se.fit[,5]

# make dataframe
rfi_ests <- data.frame(Arch_hat, Arch_low = Arch_hat - 1.96*Arch_se, Arch_high = Arch_hat + 1.96*Arch_se,
  AaL_hat, AaL_low = AaL_hat - 1.96*AaL_se, AaL_high = AaL_hat + 1.96*AaL_se,
  Bus_hat, Bus_low = Bus_hat - 1.96*Bus_se, Bus_high = Bus_hat + 1.96*Bus_se,
  Sci_hat, Sci_low = Sci_hat - 1.96*Sci_se, Sci_high = Sci_hat + 1.96*Sci_se,
  Arch_AaL_hat, 
  sind = seq(1, 366, by = 1)/366)

# Make plots 
Arch_Eng <- ggplot(rfi_ests, aes(x = sind, y = Arch_hat)) + 
  geom_line() + geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_line(aes(x = sind, y = Arch_low), color = "red") + 
  geom_line(aes(x = sind, y = Arch_high), color = "blue") + 
  ylab("s(sind):CollegeArchitecture") + 
  ggtitle("Architecture vs Engineering") + theme_bw()

AaL_Eng <- ggplot(rfi_ests, aes(x = sind, y = AaL_hat)) + 
  geom_line() + geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_line(aes(x = sind, y = AaL_low), color = "red") + 
  geom_line(aes(x = sind, y = AaL_high), color = "blue") + 
  ylab("s(sind):CollegeArts.and.Letters") + 
  ggtitle("Arts and Letters vs Engineering") + theme_bw()

Bus_Eng <- ggplot(rfi_ests, aes(x = sind, y = Bus_hat)) + 
  geom_line() + geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_line(aes(x = sind, y = Bus_low), color = "red") + 
  geom_line(aes(x = sind, y = Bus_high), color = "blue") + 
  ylab("s(sind):CollegeBusiness") + 
  ggtitle("Business vs Engineering") + theme_bw()

Sci_Eng <- ggplot(rfi_ests, aes(x = sind, y = Sci_hat)) + 
  geom_line() + geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.9) + 
  geom_line(aes(x = sind, y = Sci_low), color = "red") + 
  geom_line(aes(x = sind, y = Sci_high), color = "blue") + 
  ylab("s(sind):CollegeScience") + 
  ggtitle("Science vs Engineering") + theme_bw()




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



