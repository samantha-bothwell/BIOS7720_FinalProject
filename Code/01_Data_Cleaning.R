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
### The purpose of this file is to clean data  ###
### for analysis.                              ###
##################################################

rm(list = ls())
### Load packages
library(tidyverse)
library(stringr)

### Load in data
dem <- read.csv("D:/CU/Spring 2021/FDA/Final Project/Data/DataRaw/BasicSurvey(3-6-20).csv")
pa <- read.csv("D:/CU/Spring 2021/FDA/Final Project/Data/DataRaw/FitbitActivity(1-30-20).csv")
sleep <- read.csv("D:/CU/Spring 2021/FDA/Final Project/Data/DataRaw/FitbitSleep(1-30-20).csv")
grade <- read.csv("D:/CU/Spring 2021/FDA/Final Project/Data/DataRaw/CourseGrades(3-6-20).csv")
sleep_clean <- readRDS("D:/CU/Spring 2021/FDA/Final Project/Data/DataProcessed/FDA_FitBit_SleepDays.rds")

### Make sure demographic data only contains people we have pa data on
dem <- dem %>% 
  filter(egoid %in% unique(pa$egoid))

### Collect only demographic data we are interested in 
      # Height, Weight, Parent Income, Grade, Major, Race/Ethnicity, Gender, Pursuing Degree
dem <- dem %>% 
  select(egoid, Height_1, Weight_1, parentincome_1, gender_1, race_1, major1rc_1, degreeintent_1)
# Recode data
dem$major1rc_1 <- sapply(strsplit(as.character(dem$major1rc_1), ""), `[`, 1)
dem$College <- factor(dem$major1rc_1, levels = c("1","2","3","4","5","6"), 
  labels = c("Arts and Letters", "Architecture", "Engineering", "Business", "Science", "Undecided"))
dem$degreeintent_1 <- ifelse(dem$degreeintent_1 == "", NA, dem$degreeintent_1)
dem$Height_1 <- dem$Height_1*12 # conver height to inches
dem$BMI = (dem$Weight_1/((dem$Height_1)^2))*703 # calculate BMI
dem$Income = ifelse(dem$parentincome_1 %in% c("less than $25,000", "$25,000 - $49,999"), "< $50,000", 
  ifelse(dem$parentincome_1 %in% c("$50,000 - $74,999", "$75,000 - $99,999", "$100,000 - $149,999"), "$50,000 - $150,000", 
    "> $150,000"))

### Clean grades data 
# Remove "P", "S", "U", "V" and ""
# Remove 201750 semester
grade <- grade %>% 
  filter(!FinalGrade %in% c("P", "S", "U", "V", "")) %>% 
  filter(!AcademicPeriod == 201750)
# Calculate GPA using https://pages.collegeboard.org/how-to-convert-gpa-4.0-scale
grade$GPA <- as.numeric(as.character(factor(grade$FinalGrade, 
  levels = c("A","A-","B+","B","B-","C+","C","C-","D","F"), 
  labels = c("4","3.7","3.3","3","2.7","2.3","2.0","1.7","1","0"))))
# summarize by egoid
grade <- grade %>% 
  group_by(egoid) %>% 
  summarise(cumGPA = mean(GPA))
# Add grades to dem data 
dem$GPA <- grade$cumGPA[match(dem$egoid, grade$egoid)]


### Clean pa data
# Remove duplicates 
pa <- pa[!duplicated(pa[, c("egoid", "datadate")]),]
# Add days with no data on steps for each individual
pa <- pa %>% 
  mutate(datadate = as.Date(datadate, format = "%Y-%m-%d")) %>% 
  group_by(egoid) %>% 
  complete(datadate = seq.Date(min(datadate), max(datadate), by="day"))
# Extract month-day
pa$md <- substr(pa$datadate, 6, 10)
# Add year to id 
pa$id_yr <- paste0(pa$egoid, "_", substr(pa$datadate, 1, 4))
# Sort data 
pa <- pa[order(pa$md),]
# Transpose to wide format
pa_wide <- pa %>% 
  select(id_yr, md, steps) %>% 
  pivot_wider(names_from = md, values_from = steps)

### Clean sleep data further 
# Add days with no data on steps for each individual
sleep_clean <- sleep_clean %>% 
  mutate(dataDate = as.Date(dataDate, format = "%Y-%m-%d")) %>% 
  group_by(egoid) %>% 
  complete(dataDate = seq.Date(min(dataDate), max(dataDate), by="day"))
# Extract month-day
sleep_clean$md <- substr(sleep_clean$dataDate, 6, 10)
# Add year to id 
sleep_clean$id_yr <- paste0(sleep_clean$egoid, "_", substr(sleep_clean$dataDate, 1, 4))
# Sort data 
sleep_clean <- sleep_clean[order(sleep_clean$md),]
# Transpose to wide format
sleep_wide <- sleep_clean %>% 
  select(id_yr, md, tot_min_sleep) %>% 
  pivot_wider(names_from = md, values_from = tot_min_sleep)


### Summarize missing data 
miss <- pa %>% 
  group_by(egoid) %>% 
  summarize(num.miss = sum(is.na(steps)), num = sum(!is.na(steps)), perc.miss = num.miss/(num.miss + num))
# Add number of days and % missing to dem
dem$ndays <- miss$num[match(dem$egoid, miss$egoid)]
dem$perc.miss <- miss$perc.miss[match(dem$egoid, miss$egoid)]

### Summarize comply percent
comply <- pa %>% 
  mutate(complypercent = ifelse(is.na(complypercent), 0, complypercent)) %>% 
  group_by(egoid) %>% 
  summarise(mn.comply = mean(complypercent, na.rm = T))
# Add to dem
dem$comply <- comply$mn.comply[match(dem$egoid, comply$egoid)]

### Summarize total sleep time 
sleep_time <- sleep_clean %>% 
  group_by(egoid) %>% 
  summarize(mn.sleep = mean(tot_min_sleep, na.rm = T)) %>% 
  mutate(mn.sleep = mn.sleep/60)
# Add to dem 
dem$sleep <- sleep_time$mn.sleep[match(dem$egoid, sleep_time$egoid)]

### Save dem 
write.csv(dem, "D:/CU/Spring 2021/FDA/Final Project/Data/DataProcessed/Demographics.csv")


### Add college data to pa
pa_wide$College <- dem$College[match(pa_wide$egoid, dem$egoid)]
# Plot average daily step count by college 
pa_plot_dat = pa_wide
colnames(pa_plot_dat)[-c(1,2,369)] <- paste0("Day_",1:366)
pa_plot <- pa_plot_dat %>%
  pivot_longer(cols = starts_with("Day_"), names_prefix = "Day_", names_to = "Day", values_to = "Step Count") %>% 
  group_by(Day, College) %>% 
  summarise(mn.Steps = mean(`Step Count`, na.rm = T), sd.Steps = sd(`Step Count`, na.rm = T)) %>% 
  filter(!is.na(College)) %>%
  mutate(Day = as.numeric(as.character(Day))) %>% 
  ggplot(aes(x = Day, y = mn.Steps, colour = College)) +
  geom_line() + ylab("Mean Step Count")
pa_plot

# Plot average daily sleep time 
sleep_plot_dat = sleep_wide
colnames(sleep_plot_dat)[-c(1,2)] <- paste0("Day_",1:366)
sleep_plot <- sleep_plot_dat %>%
  pivot_longer(cols = starts_with("Day_"), names_prefix = "Day_", names_to = "Day", values_to = "Sleep Time") %>% 
  mutate(`Sleep Time` = `Sleep Time`/60) %>% 
  group_by(Day) %>% 
  summarise(mn.sleep = mean(`Sleep Time`, na.rm = T), sd.sleep = sd(`Sleep Time`, na.rm = T)) %>% 
  mutate(Day = as.numeric(as.character(Day))) %>% 
  ggplot(aes(x = Day, y = mn.sleep)) + geom_line(color = "turquoise4") + 
  ylab("Mean Sleep Time (Hours)")
sleep_plot

