### Functional Data Analysis -- Final Project
### FitBit Steps Data Cleaning

### NOTE: All data saving steps are commented out to prevent overwritting files
### uncomment lines: 276; 328-332 to save cleaned data sets

## -- Libraries
library(ggplot2)
library(stringr)
library(lcsm)

## -- Folders
fda.folder <- "C:/Users/Suneeta/Documents/Colorado_PhD/BIOS7720_FunctionalDataAnalysis/FinalProject" 
infolder <- file.path(fda.folder, "Data")
fig.folder <- file.path(fda.folder, "Figures")
outfolder <- file.path(fda.folder, "ProcessedData")

# Data frame to store data cleaning items
data.cleaning.df <- data.frame("Tasks" = character(), 
                               "Days" = numeric(), 
                               "N" = numeric(), 
                               stringsAsFactors = F)

##  -- Basic Survey Data
survey <- read.csv(file = file.path(infolder, "BasicSurvey(3-6-20).csv"), 
                   header = T, stringsAsFactors = F)
table(survey$StudyStatus)

ids_in.study <- survey$egoid[survey$StudyStatus == "In Study"] ## In study ids

# --  READ IN FitBit PA data
pa_fb <- read.csv(file = file.path(infolder, "FitbitActivity(1-30-20).csv"), 
                      header = T, stringsAsFactors = F)
# Update data cleaning dataframe
i = 1
data.cleaning.df[i, 1] <- "Raw Data"
data.cleaning.df[i, 2] <- nrow(pa_fb)
data.cleaning.df[i, 3] <- length(unique(pa_fb$egoid))


# -- Find duplicate days
dup <- duplicated(pa_fb[, c("egoid", "datadate")])

## Keep rows with the most information
dup.days <- pa_fb[dup == T, c("egoid", "datadate")]
pa_fb.dup <- merge(dup.days, pa_fb, by = c("egoid", "datadate"), all.x = T)
sum(is.na(pa_fb.dup$steps)) # number of days with no step info = 8

## Find the number of complete variables in each row
pa_fb.dup$var.cc <- apply(pa_fb.dup, 
                              MARGIN = 1, 
                              FUN = function(x) sum(!(is.na(x))))
## order dup days data by id, date and # of complete variables
pa_fb.dup <- pa_fb.dup[order(pa_fb.dup$egoid, pa_fb.dup$datadate, pa_fb.dup$var.cc), ]
## keep the dup days data with the most number of complet variables
pa_fb.dup <- pa_fb.dup[duplicated(pa_fb.dup[, c("egoid", "datadate")]), ]


## Remove data for duplicate days from original file
dup.days$dup <- 1
pa_fb <- merge(pa_fb, dup.days, by = c("egoid", "datadate"), all.x = T)
pa_fb$dup[is.na(pa_fb$dup)] <- 0
pa_fb <- pa_fb[pa_fb$dup == 0, ]

## Remove vars that are not common between files
pa_fb$dup <- NULL
pa_fb.dup$var.cc <- NULL

## Join original file with collapsed duplicate days file
pa_fb <- rbind(pa_fb, pa_fb.dup)
pa_fb <- pa_fb[order(pa_fb$egoid, pa_fb$datadate), ]

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Removed Duplicate Days (", nrow(pa_fb.dup), " days)")
data.cleaning.df[i, 2] <- nrow(pa_fb)
data.cleaning.df[i, 3] <- length(unique(pa_fb$egoid)) # 623 in fitbit pa file
sum(unique(pa_fb$egoid) %in% ids_in.study) ## all pt in fitbit pa file in study


## -- Remove days with < 80%  compliance (19.2 hours out of 24)
summary(pa_fb$complypercent)
pa_fb <- pa_fb[pa_fb$complypercent >= 80, ] ## Subset to days with 80% compliance
length(unique(pa_fb$egoid))

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Remove Days w/<80% compliance")
data.cleaning.df[i, 2] <- nrow(pa_fb)
data.cleaning.df[i, 3] <- length(unique(pa_fb$egoid)) # 623 in fitbit pa file


## -- Nonsensical data
sum(is.na(pa_fb$egoid))
sum(is.na(pa_fb$steps))
# Remove NA steps days
pa_fb <- pa_fb[!(is.na(pa_fb$steps)), ]

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Remove Days w/ NA steps")
data.cleaning.df[i, 2] <- nrow(pa_fb)
data.cleaning.df[i, 3] <- length(unique(pa_fb$egoid)) # 623 in fitbit pa file

## --  Checking summaries
summary(pa_fb$steps)
sd(pa_fb$steps, na.rm = T)
hist(pa_fb$steps, breaks = 1000, 
     main = "Steps per day (duplicated, < 80% comp, & NA steps removed)")
boxplot(pa_fb$steps, main = "Steps per day")

pa_fb$wearDate <- as.Date(strptime(pa_fb$datadate, format = "%Y-%m-%d"))
min(pa_fb$wearDate)
max(pa_fb$wearDate)

pa_fb$year <- as.numeric(substr(pa_fb$datadate, 1, 4))
pa_fb <- pa_fb[order(pa_fb$egoid, pa_fb$year), ]


# Number of observations for each participant
obs.num <- aggregate(datadate ~ egoid, data = pa_fb, FUN = function(x) sum(!(is.na(x))))
hist(obs.num$datadate, breaks = 100, main = "Number of Observed Days per Individual", 
     xlab = "Number of days")
summary(obs.num) #min = 1, max = 1355

## -- Reshaping to Academic Year format
ac_template <- read.csv(file.path(infolder, "AcademicYear_Template.csv"), 
                        header = T, stringsAsFactors = F)
ac_template$md <- str_pad(as.character(ac_template$md), 
                          width = 3, side= "left", pad = "0")


pa_fb <- merge(pa_fb, ac_template, by.x = "datadate", by.y = "AcademicYrDates", 
               all.x = T)
pa_fb <- pa_fb[!(is.na(pa_fb$md)), ] ## Remove days that are before the start of the 1st Yr in the Academic Calendar

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Remove Days occuring before 1st Academic Year begins (start: 8/1/2015)")
data.cleaning.df[i, 2] <- nrow(pa_fb)
data.cleaning.df[i, 3] <- length(unique(pa_fb$egoid)) # 623 in fitbit pa file

## Reshape to wide format
pa_fb_yr <- reshape(pa_fb[, c("egoid", "AcademicYr", "md", "steps")], 
                        timevar = "md", 
                        idvar = c("egoid", "AcademicYr"), 
                        direction = "wide")

# Calculate number of days per year (366 days in a year indicates a leap year)
pa_fb_yr$days_yr <- apply(pa_fb_yr[, 3:368], 
                              MARGIN = 1, 
                              FUN = function(x) sum(!(is.na(x))))
hist(pa_fb_yr$days_yr)
summary(pa_fb_yr$days_yr)

sum(pa_fb_yr$days_yr >= 365)
sum(pa_fb_yr$days_yr < 100)

##################  --- Plot for preliminary presentation --- #########################

## Categories person-year by number of days of data ()
# quin.day <- quantile(pa_fb_yr$days_yr, probs = seq(0, 1, by=0.2))
# pa_fb_yr$days.quin <- NA
# pa_fb_yr$days.quin[pa_fb_yr$days_yr < quin.day[2]] <- 1
# pa_fb_yr$days.quin[pa_fb_yr$days_yr >= quin.day[2] & pa_fb_yr$days_yr < quin.day[3]] <- 2
# pa_fb_yr$days.quin[pa_fb_yr$days_yr >= quin.day[3] & pa_fb_yr$days_yr < quin.day[4]] <- 3
# pa_fb_yr$days.quin[pa_fb_yr$days_yr >= quin.day[4] & pa_fb_yr$days_yr < quin.day[5]] <- 4
# pa_fb_yr$days.quin[pa_fb_yr$days_yr >= quin.day[5]] <- 5
# table(pa_fb_yr$days.quin)
# 
# pa_fb_yr$pct_missing <- (365 - pa_fb_yr$days_yr)/365

## Choose ids to plot based on number of days of data (one from each quintile)
# set.seed(1359)

# pids <- c(sample(pa_fb_yr$egoid[pa_fb_yr$days.quin == 1], size = 1), 
#           sample(pa_fb_yr$egoid[pa_fb_yr$days.quin == 2], size = 1),
#           sample(pa_fb_yr$egoid[pa_fb_yr$days.quin == 3], size = 1),
#           sample(pa_fb_yr$egoid[pa_fb_yr$days.quin == 4], size = 1),
#           sample(pa_fb_yr$egoid[pa_fb_yr$days.quin == 5], size = 1))

pids <- c(33127, 98766, 18611, 28327, 71985)

pa_fb_yr$id_year <- paste(pa_fb_yr$egoid, pa_fb_yr$AcademicYr, sep = "_")
pa.names <- sort(names(pa_fb_yr)[3:368])

pa_fb_yr <- pa_fb_yr[, c("egoid", "AcademicYr", "days_yr", "id_year", 
                         pa.names)]

### Plot in Slides
jpeg(filename = file.path(fig.folder, "Sample_Steps_perAcademicYear.jpg"), 
     width = 9, height = 4, units = "in", res = 300)
plot_trajectories(data = pa_fb_yr[pa_fb_yr$egoid %in% pids, ],
                  id_var = "id_year", 
                  var_list = pa.names,
                  xlab = "Days", ylab = "Steps",
                  connect_missing = FALSE, 
                  random_sample_frac = 1)+
  facet_grid(row= vars(egoid), col = vars(AcademicYr))+
  theme(axis.text.x = element_blank())
dev.off()

#######################################################################################


## Remove years with < 100 days
pa_fb_yr <- pa_fb_yr[pa_fb_yr$days_yr >= 100, ] 

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Remove Years w < 100 days per year")
data.cleaning.df[i, 2] <- sum(pa_fb_yr$days_yr)
data.cleaning.df[i, 3] <- length(unique(pa_fb_yr$egoid)) # 623 in fitbit pa file


min(pa_fb_yr[, 5:370], na.rm=T) ### lowest steps recorded
max(pa_fb_yr[, 5:370], na.rm=T) ### highest steps recorded

### Reorder variables in data.frames
step.var.names <- names(pa_fb_yr)[5:370]

pa_fb_yr <- pa_fb_yr[, c("egoid", "AcademicYr", "days_yr", 
                         step.var.names)]

## Find the maximum consecutive missing days
pa_fb_pa <- ifelse(is.na(pa_fb_yr[, 4:369]), 0, 1)

maxMissingDays <- function(x){
  days <- rle(x)
  return(as.numeric(max(days$lengths[days$values == 0])))
}

pa_fb_yr$maxMissing <- apply(pa_fb_pa,
                             MARGIN = 1, 
                             FUN = maxMissingDays)

# plot max consecutive days missing against total days per year
# plot(pa_fb_yr$days_yr, pa_fb_yr$maxMissing)

## Find the year with the maximum number of days per year for each participant
pa_fb_yr <- pa_fb_yr[order(pa_fb_yr$egoid, -pa_fb_yr$days_yr, pa_fb_yr$maxMissing), ]

max.year <- aggregate(days_yr ~ egoid, data = pa_fb_yr, FUN = function(x) max(x, na.rm = T))
names(max.year)[names(max.year) == "days_yr"] <- "max.days"

pa_fb_yr <- merge(pa_fb_yr, max.year, by = "egoid", all.x = T)
pa_fb_yr$keep.max.days <- ifelse(pa_fb_yr$days_yr == pa_fb_yr$max.days, 1, 0)

## One participant had the same # of days per year in 2 years, keep year with less consecutive missing days
yr_sub <- aggregate(keep.max.days ~ egoid, data = pa_fb_yr, FUN = function(x) sum(x, na.rm = T))
table(yr_sub$keep.max.days)
yr_sub$egoid[yr_sub$keep.max.days == 2]
View(pa_fb_yr[pa_fb_yr$egoid == 23518, ])
pa_fb_yr$keep.max.days[pa_fb_yr$egoid == 23518 & pa_fb_yr$AcademicYr == "2017-2018"] <- 0

##Subset to one Acamdemic year per subject
pa_fb_yr_ind <- pa_fb_yr[pa_fb_yr$keep.max.days == 1, ] 

### Reorder variables in data.frames
step.var.names <- names(pa_fb_yr)[4:369]

pa_fb_yr <- pa_fb_yr[, c("egoid", "AcademicYr", "days_yr", "maxMissing", "keep.max.days",
                         step.var.names)]

pa_fb_yr_ind <- pa_fb_yr_ind[, c("egoid", "AcademicYr", "days_yr", "maxMissing",
                                 step.var.names)]

pa_fb_long <- reshape(pa_fb_yr, 
                 varying = step.var.names,
                 v.names = "steps", 
                 timevar = "md", 
                 times = substr(step.var.names, 7, 9), 
                 direction = "long")

## Save file with missing days for missing days analysis
# saveRDS(pa_fb_long, file.path(outfolder, "Fitbit_Steps_KeepNA.rds"))

# Remove missing days
pa_fb_long <- pa_fb_long[!(is.na(pa_fb_long$steps)), ]

# Add dates back to long formatted file
pa_fb_long <- merge(pa_fb_long, ac_template, 
                    by = c("AcademicYr", "md"), all.x = T)

names(pa_fb_long)[names(pa_fb_long) == "AcademicYrDates"] <- "datadate"

pa_fb_long <- pa_fb_long[order(pa_fb_long$egoid, pa_fb_long$datadate), ]
pa_fb_long$wearDate <- as.Date(pa_fb_long$datadate)

# Day of week (DOW) and weekday/weekend day
pa_fb_long$dow <- format(pa_fb_long$wearDate, "%a")
pa_fb_long$weekday <- ifelse(pa_fb_long$dow %in% c("Sat", "Sun"), 0, 1)

# Subset long format file to only one academic year per subject
pa_fb_long_1ac <- pa_fb_long[pa_fb_long$keep.max.days == 1, ]

# Reorder variables
pa_fb_long <- pa_fb_long[, c("egoid", "AcademicYr", "datadate", "wearDate",
                             "md", "dow", "weekday", "days_yr", "maxMissing", "steps")]

pa_fb_long_1ac <- pa_fb_long_1ac[, c("egoid", "AcademicYr", "datadate", "wearDate",
                             "md", "dow", "weekday", "days_yr", "maxMissing", "steps")]

# Update data cleaning dataframe
i = i+1
data.cleaning.df[i, 1] <- paste0("Filter to 1 Academic Year per student")
data.cleaning.df[i, 2] <- nrow(pa_fb_long_1ac)
data.cleaning.df[i, 3] <- length(unique(pa_fb_long_1ac$egoid))


## -- Encode wide datasets with month-day variable names
ac_template$var.names <- paste("steps", 
                               substr(as.character(ac_template$AcademicYrDates), 
                                      6, nchar(as.character(ac_template$AcademicYrDates))), 
                               sep = ".")
pa_fb_yr$keep.max.days <- NULL
names(pa_fb_yr) <- c("egoid", "AcademicYr", "days_yr", "maxMissing", 
                         ac_template$var.names[1:366])

names(pa_fb_yr_ind) <- c("egoid", "AcademicYr", "days_yr", "maxMissing", 
                         ac_template$var.names[1:366])

summary(pa_fb_long$steps)
summary(pa_fb_long_1ac$steps)
boxplot(pa_fb_long$steps, pa_fb_long_1ac$steps)


# saveRDS(pa_fb_yr, file = file.path(outfolder, "FitBit_steps_wide.rds"))
# saveRDS(pa_fb_yr_ind, file = file.path(outfolder, "FitBit_steps_1AcademicYear_wide.rds"))
# saveRDS(pa_fb_long, file = file.path(outfolder, "FitBit_steps_long.rds"))
# saveRDS(pa_fb_long_1ac, file = file.path(outfolder, "FitBit_steps_1AcademicYear_long.rds"))
# saveRDS(data.cleaning.df, file = file.path(outfolder, "FitBit_Step_DataCleaningSummary.rds"))

