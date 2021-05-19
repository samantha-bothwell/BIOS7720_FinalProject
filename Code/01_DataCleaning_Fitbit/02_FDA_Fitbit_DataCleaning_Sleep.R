### Functional Data Analysis -- Final Project
### FitBit Sleep Data Cleaning

### NOTE: All data saving steps are commented out to prevent overwritting files
### uncomment lines: 325; 370-371; 386-388 to save cleaned data sets

## -- Libraries
library(ggplot2)
library(stringr)
library(data.table)
library(lcsm)

## -- Folders
fda.folder <- "C:/Users/Suneeta/Documents/Colorado_PhD/BIOS7720_FunctionalDataAnalysis/FinalProject" 
infolder <- file.path(fda.folder, "Data")
fig.folder <- file.path(fda.folder, "Figures")
outfolder <- file.path(fda.folder, "ProcessedData")

# Data cleaning summary data frame
data.cleaning.df <- data.frame("Tasks" = character(), 
                               "Days" = numeric(), 
                               "N" = numeric(), 
                               stringsAsFactors = F)

## -- Study Participants
survey <- read.csv(file = file.path(infolder, "BasicSurvey(3-6-20).csv"), 
                   header = T, stringsAsFactors = F)
ids_in.study <- survey$egoid[survey$StudyStatus == "In Study"] ## In study ids

## -- Duplicate basic cleaning steps for PA data to extract compliance variable
## PA Compliance Data
pa_fb <- read.csv(file = file.path(infolder, "FitbitActivity(1-30-20).csv"), 
                  header = T, stringsAsFactors = F)

### Find duplicate days
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


### Remove data for duplicate days from original file
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

pa_compliance <- pa_fb[, c("egoid", "datadate", "complypercent")]

rm(list = c("dup.days", "pa_fb", "pa_fb.dup", "survey"))
####################################################

## -- SLEEP Data -- sleep DATE is for end of sleep period

sleep.fb <- read.csv(file.path(infolder, "FitbitSleep(1-30-20).csv"), 
                     header = T, stringsAsFactors = F)
sleep.fb$startSleepHr <- as.numeric(substr(sleep.fb$timetobed, 1, 2))
sleep.fb$endSleepHr <- as.numeric(substr(sleep.fb$timeoutofbed, 1, 2))

# Update data cleaning data frame
i = 1
data.cleaning.df[i, 1] <- "Raw Data"
data.cleaning.df[i, 2] <- nrow(sleep.fb)
data.cleaning.df[i, 3] <- length(unique(sleep.fb$egoid))

#### --- VISUALIZE SLEEP BOUTS BY START AND END HOURS --- ###

# jpeg(file = file.path(fig.folder, "SleepTime_By_EndHour_ofSleepPeriod.jpg"), 
#      width = 9, height = 4, units = "in", res = 300)
# 
# ggplot(data = sleep.fb, aes(x= as.factor(endSleepHr),  y = minsasleep/60))+
#   geom_boxplot()+
#   labs(x = "End Hour for Sleep Period",
#        y = "Hours Asleep")
# dev.off()

# View(sleep.fb[sleep.fb$endSleepHr > 10, ])

# jpeg(file = file.path(fig.folder, "SleepTime_By_StartHour_ofSleepPeriod.jpg"), 
#      width = 9, height = 4, units = "in", res = 300)
# ggplot(data = sleep.fb, aes(x= as.factor(startSleepHr),  y = minsasleep/60))+
#   geom_boxplot()+
#   labs(x = "Start Hour for Sleep Period", 
#        y = "Hours Asleep")
# dev.off()

## Sleep Efficiency plot 
# ggplot(data = sleep.fb, aes(x= as.factor(startSleepHr),  y = Efficiency))+
#   geom_boxplot()+
#   labs(x = "Start Hour for Sleep Period", 
#        y = "Sleep Efficiency")


# Increment pa.matchDate by 1 day if sleep period end at or after 8 pm
sleep.fb$sleepDate <- as.Date(sleep.fb$dataDate)
sleep.fb$pa.matchDate <- ifelse(sleep.fb$endSleepHr >= 20, 
                                sleep.fb$sleepDate +1, 
                                sleep.fb$sleepDate)
# Convert pa.matchDate back into a dateformat
sleep.fb$pa.matchDate <- as.Date(sleep.fb$pa.matchDate, 
                                 origin = "1970-01-01")

# pids <- c(33127, 98766, 18611, 28327, 71985)
# View(sleep.fb[sleep.fb$egoid %in% pids[4], c("egoid", "dataDate", "pa.matchDate", "timetobed",        "timeoutofbed", "minsasleep")])

hist(sleep.fb$minsasleep/60, breaks = 1000, 
     main = "Histogram of Hrs Asleep for each sleep period", 
     xlab = "Hours asleep") ## Odd "shadow" may indicate a change in FitBit Sleep algorithm
summary(sleep.fb$minsasleep/60)
sd(sleep.fb$minsasleep/60)
boxplot(sleep.fb$minsasleep/60)

sum(sleep.fb$minsasleep < 10)
sum(sleep.fb$minsasleep < 30)
sum(sleep.fb$minsasleep > 15*60)

hist(sleep.fb$minsasleep[sleep.fb$minsasleep >= 10 & sleep.fb$minsasleep <= 15*60]/60,
     breaks = 1000, 
     main = "Histogram of Hrs Asleep for each sleep period", 
     xlab = "Hours asleep")

## -- Check compliance with FitBit Step data compliance figures
sleep.fb <- merge(sleep.fb, pa_compliance, by.x = c("egoid", "dataDate"), 
                  by.y = c("egoid", "datadate"), 
                  all.x = T)
summary(sleep.fb$complypercent)
hist(sleep.fb$complypercent, breaks = 1000)

### Looks like duplicate entries are different sleep periods during the day

## Aggregate sleep time across sleep periods on the same date
sleep.dt <- as.data.table(sleep.fb)
sleep.days <- as.data.frame(sleep.dt[,list(tot_bedtime = sum(bedtimedur, 
                                                             na.rm = T),
                                           tot_fallasleepmins = sum(minstofallasleep,
                                                                    na.rm = T),
                                           tot_afterwakemins = sum(minsafterwakeup,
                                                                   na.rm = T), 
                                           tot_min_sleep = sum(minsasleep,
                                                               na.rm = T), 
                                           tot_min_awake = sum(minsawake,
                                                               na.rm = T), 
                                           num_sleepPeriod = sum(!(is.na(bedtimedur))), 
                                           avg_Efficiency = mean(Efficiency, na.rm = T), 
                                           avg_compliance = mean(complypercent,
                                                                 na.rm = T)
                                           ),
                                     by = c("egoid", "pa.matchDate")
                                     ])

## Remove NA match dates
View(sleep.days[is.na(sleep.days$pa.matchDate), ])
View(sleep.fb[sleep.fb$egoid %in% c(18611, 66682), ]) ## no out of bed time, bed time duration 1 minute

sleep.days <- sleep.days[!(is.na(sleep.days$pa.matchDate)), ]
View(sleep.days[sleep.days$egoid %in% c(18611, 66682), ])

summary(sleep.days$avg_compliance)
hist(sleep.days$avg_compliance, breaks = 1000)

summary(sleep.days$tot_min_sleep)
sd(sleep.days$tot_min_sleep)
hist(sleep.days$tot_min_sleep, breaks = 1000)
boxplot(tot_min_sleep ~ num_sleepPeriod, data = sleep.days)

# Update data cleaning data frame
i = i+1
data.cleaning.df[i, 1] <- "Aggregate sleep time across sleep periods in same day"
data.cleaning.df[i, 2] <- nrow(sleep.days)
data.cleaning.df[i, 3] <- length(unique(sleep.days$egoid))

## -- Check data for subjects with over 20 hours of sleeping in one day
check.sleepMatching <- sleep.days[sleep.days$tot_min_sleep >= 1200 &
                                    sleep.days$num_sleepPeriod > 1,
                                  c("egoid", "pa.matchDate", "tot_min_sleep")]

check.sleepMatching$check <- 1


sleep.fb <- merge(sleep.fb, check.sleepMatching, by = c("egoid", "pa.matchDate"), 
                  all.x = T)

sleep.fb$check[is.na(sleep.fb$check)] <- 0

View(sleep.fb[sleep.fb$check ==1, c("egoid", "dataDate", "pa.matchDate", 
                                       "timetobed", "timeoutofbed", "minsasleep", 
                                    "tot_min_sleep")])
full.sleeper <- sleep.days$egoid[sleep.days$tot_min_sleep >=1440]
View(sleep.fb[sleep.fb$egoid == full.sleeper[4], ])

sum(sleep.fb$sleepDate[sleep.fb$check ==1] != sleep.fb$pa.matchDate[sleep.fb$check ==1])

### 
table(sleep.days$num_sleepPeriod)

sum(sleep.days$tot_min_sleep > 1440)

## Remove days with >= 1440 sleep time (e.g. 1 day)
sleep.days <- sleep.days[sleep.days$tot_min_sleep < 1440, ]

i = i+1
data.cleaning.df[i, 1] <- "Remove days with aggregated sleep time of more than 1 day"
data.cleaning.df[i, 2] <- nrow(sleep.days)
data.cleaning.df[i, 3] <- length(unique(sleep.days$egoid))


# Number of participant
length(unique(sleep.days$egoid)) # 622 in fitbit pa file
sum(unique(sleep.days$egoid) %in% ids_in.study) ## all pt in fitbit pa file in study

sleep.days$datadate <- as.character(sleep.days$pa.matchDate)

ac_template <- read.csv(file.path(infolder, "AcademicYear_Template.csv"), 
                        header = T, stringsAsFactors = F)
ac_template$md <- str_pad(as.character(ac_template$md), 
                          width = 3, side= "left", pad = "0")

sleep.days <- merge(sleep.days, ac_template, by.x = "datadate", by.y = "AcademicYrDates", 
                    all.x = T)

## Removed days outside the first Academic Year
sleep.days <- sleep.days[!(is.na(sleep.days$md)), ]

sleep.days <- sleep.days[order(sleep.days$egoid, sleep.days$AcademicYr, sleep.days$md), ]

i = i+1
data.cleaning.df[i, 1] <- "Remove days before of 1st Academic year (start date: 8/1/2015)"
data.cleaning.df[i, 2] <- nrow(sleep.days)
data.cleaning.df[i, 3] <- length(unique(sleep.days$egoid))

#### NO COMPLIANCE DATA FOR SLEEP 

#Number of observations for each participant
slpobs.num <- aggregate(datadate ~ egoid, data = sleep.days, 
                        FUN = function(x) sum(!(is.na(x))))
hist(slpobs.num$datadate, breaks = 20, main = "Number of Observed Days per Individual", 
     xlab = "Number of days")
summary(slpobs.num) #min = 1, max = 1367

sleep.fb.yr <- reshape(sleep.days[, c("egoid", "AcademicYr", "md", "tot_min_sleep")], 
                       timevar = "md", 
                       idvar = c("egoid", "AcademicYr"), 
                       direction = "wide")

sleep.fb.yr$days_yr <- apply(sleep.fb.yr[, 3:368], 
                             MARGIN = 1, 
                             FUN = function(x) sum(!(is.na(x))))
hist(sleep.fb.yr$days_yr, breaks = 1000)
summary(sleep.fb.yr$days_yr)

sum(sleep.fb.yr$days_yr >= 365)

sleep.fb.yr$id_year <- paste(sleep.fb.yr$egoid, sleep.fb.yr$AcademicYr, sep = "_")
slp.names <- sort(names(sleep.fb.yr)[3:368])

sleep.fb.yr <- sleep.fb.yr[, c("egoid", "AcademicYr", "days_yr", "id_year", 
                               slp.names)]

#Convert to hours of sleep instead of minutes
sleep.fb.yr.hr <- round(sleep.fb.yr[, slp.names]/60, 2)
sleep.fb.yr.hr <- cbind(sleep.fb.yr[, c("egoid", "AcademicYr", "days_yr", "id_year")], 
                        sleep.fb.yr.hr)

############## --- PLOT FOR PRELIM PRESENTATON --- #############################
pids <- c(33127, 98766, 18611, 28327, 71985)
# jpeg(filename = file.path(fig.folder, "Sample_SleepHrs_perAcademicYear.jpg"), 
#      width = 9, height = 4, units = "in", res = 300)
# plot_trajectories(data = sleep.fb.yr.hr[sleep.fb.yr.hr$egoid %in% pids, ],
#                   id_var = "id_year", 
#                   var_list = slp.names,
#                   xlab = "Days", ylab = "Sleep Hours",
#                   connect_missing = FALSE, 
#                   random_sample_frac = 1)+
#   facet_grid(row= vars(egoid), col = vars(AcademicYr))+
#   theme(axis.text.x = element_blank())
# 
# dev.off()

################################################################################

## How many days of matched data step to sleep do we have
pa_fb.alldays <- readRDS(file.path(outfolder, "FitBit_steps_long.rds"))

pa_fb.days <- pa_fb.alldays[, c("egoid", "datadate")]
pa_fb.days$pa.data <- 1

slp.fb.days <- sleep.days[, c("egoid", "datadate")]
slp.fb.days$slp.data <- 1

all.days <- merge(pa_fb.days, slp.fb.days, by = c("egoid", "datadate"), all = T)
all.days$pa.data[is.na(all.days$pa.data)] <- 0
all.days$slp.data[is.na(all.days$slp.data)] <- 0

dup <- duplicated(all.days[, c("egoid", "datadate")]) ## No duplicated days
table(dup)

table(all.days$pa.data, all.days$slp.data)
prop.table(table(all.days$pa.data, all.days$slp.data))

flag.days <- all.days
names(flag.days)[names(flag.days) == "pa.data"] <- "AllDays_Steps"
names(flag.days)[names(flag.days) == "slp.data"] <- "AllDays_Sleep"


### Save files
# saveRDS(sleep.days, file.path(outfolder, "FitBit_sleep_long.rds"))
# Filter to matched days

pa_fb.acdays <- readRDS(file.path(outfolder, "FitBit_steps_1AcademicYear_long.rds"))

pa_fb.days <- pa_fb.acdays[, c("egoid", "datadate")]
pa_fb.days$pa.data <- 1

slp.fb.days <- sleep.days[, c("egoid", "datadate")]
slp.fb.days$slp.data <- 1

ac.days <- merge(pa_fb.days, slp.fb.days, by = c("egoid", "datadate"), all = T)
ac.days$pa.data[is.na(ac.days$pa.data)] <- 0
ac.days$slp.data[is.na(ac.days$slp.data)] <- 0


flag.days <- merge(flag.days, ac.days, by = c("egoid", "datadate"), 
                   all.x = T, all.y = T)

names(flag.days)[names(flag.days) == "pa.data"] <- "AcYr1_Steps"
names(flag.days)[names(flag.days) == "slp.data"] <- "AcYr1_Sleep"

flag.days$AllDays_Matched <- ifelse(flag.days$AllDays_Steps == 1 &
                                      flag.days$AllDays_Sleep == 1, 1, 0)
flag.days$AcYr1_Matched <- ifelse(flag.days$AcYr1_Steps == 1 &
                                      flag.days$AcYr1_Sleep == 1, 1, 0)
flag.days$AcYr1_Matched[is.na(flag.days$AcYr1_Matched)] <- 0

fb_sleep_matched <- merge(sleep.days, flag.days, by = c("egoid", "datadate"), 
                                 all.x = T)

fb_sleep_matched_allyrs <- fb_sleep_matched[fb_sleep_matched$AllDays_Matched == 1, 
                                            c("egoid", "datadate", "pa.matchDate", 
                                              "md", "AcademicYr",
                                              "tot_bedtime", "tot_fallasleepmins", 
                                              "tot_afterwakemins", "tot_min_sleep", 
                                              "tot_min_awake", "num_sleepPeriod", 
                                              "avg_Efficiency", "avg_compliance")]
i = i+1
data.cleaning.df[i, 1] <- "Days matched to Step data Across all Academic Years"
data.cleaning.df[i, 2] <- nrow(fb_sleep_matched_allyrs)
data.cleaning.df[i, 3] <- length(unique(fb_sleep_matched_allyrs$egoid))



# saveRDS(fb_sleep_matched_allyrs,
#         file.path(outfolder, "Fitbit_sleep_long_matched.rds"))

fb_sleep_matched_AcYr1 <- fb_sleep_matched[fb_sleep_matched$AcYr1_Matched == 1, 
                                            c("egoid", "datadate", "pa.matchDate", 
                                              "md", "AcademicYr",
                                              "tot_bedtime", "tot_fallasleepmins", 
                                              "tot_afterwakemins", "tot_min_sleep", 
                                              "tot_min_awake", "num_sleepPeriod", 
                                              "avg_Efficiency", "avg_compliance")]

i = i+1
data.cleaning.df[i, 1] <- "Days matched to Step data for 1 Academic Year"
data.cleaning.df[i, 2] <- nrow(fb_sleep_matched_AcYr1)
data.cleaning.df[i, 3] <- length(unique(fb_sleep_matched_AcYr1$egoid))

# saveRDS(fb_sleep_matched_AcYr1,
#         file.path(outfolder, "Fitbit_sleep_1AcademicYr_long_matched.rds"))
# saveRDS(data.cleaning.df, file.path(outfolder, "Fitbit_Sleep_DataCleaningSummary.rds"))
