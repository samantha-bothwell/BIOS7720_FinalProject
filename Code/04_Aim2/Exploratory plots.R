# Exploratory Plots for FDA final Project

# data location
setwd('C:/Users/trumblei/Dropbox (Personal)/Functional Data/Final Project/ProcessedData')

# read in data sets

# Sleep data matched to 1 Academic year, long format

matched_long <- readRDS("Fitbit_sleep_1AcademicYr_long_matched.rds")


# Steps in 1 academic year, long format
# these are data sets that will be used for FoSc regression
# for steps versus college
steps_long <- readRDS("FitBit_steps_1AcademicYear_long.rds")
steps_wide <- readRDS("FitBit_steps_1AcademicYear_wide.rds")

# demographics data
dem_df <- read.csv("Demographics.csv")

# Create Exploratory plots

### Boxplot of avg steps versus college ####

# first, average over all days for each subjects
only_steps <- steps_wide[,5:dim(steps_wide)[2]]
steps_avg <- rowMeans(only_steps,na.rm=T)

avg_steps <- data.frame(egoid=steps_wide$egoid,steps_avg=steps_avg)

# merge with dem data
avg_steps_dem <-merge(avg_steps,dem_df,by="egoid")

# plot average steps by college
library(ggplot2)
ggplot(avg_steps_dem,aes(x=College,y=steps_avg))+
  geom_boxplot()+
  ylab("Average steps per day")+
  theme_classic()


### Scatter plot of steps versus sleep ###

# merge steps long with sleep long

steps_sleep <- merge(matched_long,steps_long,by=c("egoid","datadate"))
steps_sleep$tot_hours_sleep <- steps_sleep$tot_min_sleep/60

unique_id <- unique(steps_sleep$egoid)
avg_steps <- avg_hours_sleep <- rep(NA,length(unique_id))

# get averages by id
i=1
for (id in unique_id){
  df <- steps_sleep[steps_sleep$egoid==id,]
  avg_hours_sleep[i] <- mean(df$tot_hours_sleep,na.rm=T)
  avg_steps[i] <- mean(df$steps,na.rm = T)
  i=i+1
}

avg_steps_sleep <- data.frame(egoid=unique_id,
                              avg_hours_sleep=avg_hours_sleep,
                              avg_steps=avg_steps)

ggplot(avg_steps_sleep, aes(x=avg_hours_sleep,y=avg_steps))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  ylab("Average steps per day")+
  xlab("Average hours asleep")

# correlation/covariance plots for matched sleep/steps across span of year

# get day of year variable
steps_sleep$doy <- as.numeric(strftime(as.Date(steps_sleep$datadate),"%j"))

unique_doy <- sort(unique(steps_sleep$doy))

corrs <- matrix(rep(NA,length(unique_doy)*length(unique_doy)),
                nrow=length(unique_doy))

for (day1 in unique_doy){
  print(day1)
  for (day2 in unique_doy){
    day1_df <- steps_sleep[steps_sleep$doy==day1,c("egoid","steps")]
    day2_df <- steps_sleep[steps_sleep$doy==day2,c("egoid","tot_hours_sleep")]
    merge_df <- merge(day1_df,day2_df,by="egoid")
    corrs[day1,day2] <- cor(merge_df$steps,merge_df$tot_hours_sleep)
  }
}


library(ggcorrplot)
ggcorrplot(corrs[1:5,1:5])

corrs_vec <- as.vector(corrs)
corrs_vec[77958] <- 0
corrs_vec[133803] <- 0

corrs2 <- matrix(corrs_vec,nrow=366)

# can't see much
ggcorrplot(corrs2[1:366,1:366])+
  scale_fill_gradient2(limit=c(-0.3,0.3),
                       low="blue",
                       high="red",
                       mid="white",
                       midpoint=-0.02)

# Zoom in on diagonals
ggcorrplot(corrs2[1:100,1:100])+
  scale_fill_gradient2(limit=c(-0.3,0.3),
                       low="blue",
                       high="red",
                       mid="white",
                       midpoint=-0.02)

ggcorrplot(corrs2[101:200,101:200])+
  scale_fill_gradient2(limit=c(-0.3,0.3),
                       low="blue",
                       high="red",
                       mid="white",
                       midpoint=-0.02)

ggcorrplot(corrs2[201:300,201:300])+
  scale_fill_gradient2(limit=c(-0.3,0.3),
                       low="blue",
                       high="red",
                       mid="white",
                       midpoint=-0.02)

ggcorrplot(corrs2[301:366,301:366])+
  scale_fill_gradient2(limit=c(-0.3,0.3),
                       low="blue",
                       high="red",
                       mid="white",
                       midpoint=-0.02)


