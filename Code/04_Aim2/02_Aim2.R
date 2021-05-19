## Aim 2
## Functional outcome: daily step count
## Functional predictor: previous night's sleep
## Date: 5/10/2021

library(ggplot2)
library(ggpubr)
library(mgcv)
library(gridExtra)

# data location
setwd('C:/Users/trumblei/Dropbox (Personal)/Functional Data/Final Project/ProcessedData')

# Sleep data matched to steps 1 Academic year, long format
sleep_df <- readRDS("Fitbit_sleep_1AcademicYr_long_matched.rds")

# Steps 1 Academic year
steps_df <- readRDS("FitBit_steps_1AcademicYear_long.rds")

## Make sind variable
steps_df$sind <- as.numeric(steps_df$md)/366

# Demographics data
dem_df <- read.csv("Demographics.csv")

# Merge sleep and step data
# Merge first by id, then by date
matched_df <- merge(sleep_df,steps_df,by=c("egoid","datadate"))

## Create total sleep hours variable
matched_df$tot_hr_sleep <- matched_df$tot_min_sleep/60

## Get day of week
matched_df$dow <- weekdays(as.Date(matched_df$datadate))

## Indicator for weekday or weekend
matched_df$weekday <- ifelse(matched_df$dow %in% c("Saturday", "Sunday"), 0, 1)

## Add gender, race, socio-economic status and BMI

matched_df$Gender <- dem_df$gender_1[match(matched_df$egoid, dem_df$egoid)]
matched_df$Race <- dem_df$race_1[match(matched_df$egoid, dem_df$egoid)]
matched_df$Income <- dem_df$Income[match(matched_df$egoid, dem_df$egoid)]
matched_df$BMI <- dem_df$BMI[match(matched_df$egoid, dem_df$egoid)]

### Create class indicator variables 
GenderInd <- model.matrix( ~ Gender - 1, data = matched_df )
RaceInd <- model.matrix( ~ Race - 1, data = matched_df )
IncomeInd <- model.matrix( ~ Income - 1, data = matched_df )
YearInd <- model.matrix( ~ AcademicYr.y -1, data=matched_df )

## Combine data
Indicators <- data.frame(cbind(GenderInd, RaceInd, IncomeInd,YearInd))
matched_df <- data.frame(cbind(matched_df,Indicators))

saveRDS(matched_df,"C:/Users/trumblei/Dropbox (Personal)/Functional Data/Final Project/ProcessedData/matched_df.RDS")


## Function on Function Regression Model

## We will run a complete case analysis
matched_complete <- matched_df[!is.na(matched_df$weekday) & 
                                 !is.na(matched_df$steps) &
                                 !is.na(matched_df$tot_hr_sleep) &
                                 !is.na(matched_df$sind)&
                                 !is.na(matched_df$GenderFemale)&
                                 !is.na(matched_df$RaceAfrican.American)&
                                 !is.na(matched_df$BMI)&
                                 !is.na(matched_df$AcademicYr.y)
                                ,
                               c("egoid","weekday","steps","tot_hr_sleep",
                                 "sind","GenderFemale","GenderMale","RaceAfrican.American",
                                 "RaceAsian.American","RaceForeign.Student",
                                 "RaceLatino.a","RaceOther","RaceWhite",
                                 "Income.50.000....150.000","Income...50.000",
                                 "Income...150.000","BMI", "AcademicYr.y2015.2016",
                                 "AcademicYr.y2016.2017","AcademicYr.y2017.2018",
                                 "AcademicYr.y2018.2019")]

## Create id variable that is factor
matched_complete$id <- factor(matched_complete$egoid)



## Function on Function Regression

# Is outcome approximately normal?

ggdensity(matched_complete$steps)

# Try square root of steps? Looks better...
matched_complete$steps_sqrt <- sqrt(matched_complete$steps)
ggdensity(matched_complete$steps_sqrt)

## Fit FoFR 

k <- 30 # number of knots

time_st <- Sys.time()


## see if works: 
# matched_complete <- matched_complete[1:6000,]

fit <- bam(steps ~ s(sind,bs="cr",k=k) +
                 s(sind,by=tot_hr_sleep, bs="cr", k=k) +
                 
                 s(sind,by=weekday, bs="cr", k=k) +
                 
                 s(sind,by=GenderMale, bs="cr", k=k) +
                 
                 s(sind, by = RaceAfrican.American, bs="cr", k=k) + 
                 s(sind, by = RaceAsian.American, bs="cr", k=k) +
                 s(sind, by = RaceForeign.Student, bs="cr", k=k) + 
                 s(sind, by = RaceLatino.a, bs="cr", k=k) +
                 s(sind, by = RaceOther, bs="cr", k=k) + 
                 
                 s(sind, by = Income.50.000....150.000, bs="cr", k=k) + 
                 s(sind, by = Income...150.000, bs="cr", k=k) +
             
                 s(sind, by = AcademicYr.y2016.2017, bs="cr", k=k)+
                 s(sind, by = AcademicYr.y2017.2018, bs="cr", k=k)+
                 s(sind, by = AcademicYr.y2018.2019, bs="cr", k=k)+
                
                 s(sind, by = BMI, bs="cr", k=k)+
                 
                ti(id,sind,bs=c("re","cr"), k=c(8,8),mc=c(TRUE,FALSE)),
               
               data=matched_complete, 
               family = gaussian,
               method="fREML", 
               discrete=TRUE
               )

time_end <- Sys.time()
difftime(time_end,time_st,units="mins")

# save so don't have to wait an hour and a half to run that model again!
# save.image(file="aim2_workspace.RData")

# load("aim2_workspace.RData")

# Plot f_1 (sleep)
sind <- (1:366)/366
df_pred <- data.frame(sind=sind,
                      tot_hr_sleep=1,
                      weekday=0,
                      GenderMale=0,
                      RaceAfrican.American=0,
                      RaceAsian.American=0,
                      RaceForeign.Student=0,
                      RaceLatino.a=0,
                      RaceOther=0,
                      Income.50.000....150.000=0,
                      Income...150.000=0,
                      BMI = 0,
                      AcademicYr.y2016.2017 = 0,
                      AcademicYr.y2017.2018 = 0,
                      AcademicYr.y2018.2019 = 0,
                      id=factor(10237)
)

coefs <- predict.gam(fit,df_pred,type="iterms",se=T)

f1_hat <- as.vector(coefs$fit[,2])
f1_se <- as.vector(coefs$se.fit[,2])
  
lcl <- f1_hat - 2*f1_se
ucl <- f1_hat + 2*f1_se

df_plot <- data.frame(f1_hat = f1_hat,lcl=lcl,ucl=ucl,sind=sind)


ggplot(df_plot,aes(x=sind,y=f1_hat))+geom_line()+
  geom_line(aes(x=sind,y=lcl),color="red")+
  geom_line(aes(x=sind,y=ucl),color="blue")+
  ylab("f1_hat +/- 2SE")+
  ggtitle("Previous night's hours of sleep: f1(s)")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  xlab("Date")

## Obtain subject predictions

ests <- fit$fitted.values

matched_complete$ests <- ests

p1 <- ggplot(matched_complete[matched_complete$egoid==10237,],
       aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("10237")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))
  

p2 <-ggplot(matched_complete[matched_complete$egoid==10469,],
       aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("10469")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p3 <-ggplot(matched_complete[matched_complete$egoid==11002,],
       aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("11002")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p4 <-ggplot(matched_complete[matched_complete$egoid==11128,],
       aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("11128")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p5 <-ggplot(matched_complete[matched_complete$egoid==11148,],
            aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("11148")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))


p6 <-ggplot(matched_complete[matched_complete$egoid==11402,],
            aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("11402")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p7 <-ggplot(matched_complete[matched_complete$egoid==12156,],
            aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("12156")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p8 <-ggplot(matched_complete[matched_complete$egoid==13486,],
            aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("13486")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

p9 <-ggplot(matched_complete[matched_complete$egoid==14250,],
            aes(x=sind,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sind,y=steps),size=.5)+
  ggtitle("14250")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0,1,by=0.25),limits=c(0,1),
                     labels=c("Aug 1", "Oct 31","Jan 31","May 1",
                              "July 31"))+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(size=8),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=7))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3)

# Aggregate trends

unique_id <- unique(matched_complete$egoid)

steps_avg <- rep(NA,length(unique_id))
sleep_avg <- rep(NA,length(unique_id))
GenderMale_subj <- rep(NA,length(unique_id))
RaceAfrican.American_subj <- rep(NA,length(unique_id))
RaceAsian.American_subj <- rep(NA,length(unique_id))
RaceForeign.Student_subj <- rep(NA,length(unique_id))
RaceLatino.a_subj <- rep(NA,length(unique_id))
RaceOther_subj <- rep(NA,length(unique_id))
Income.50.000....150.000_subj <- rep(NA,length(unique_id))
Income...150.000_subj <- rep(NA,length(unique_id))
BMI_subj <- rep(NA,length(unique_id))
AcademicYr.y2016.2017_subj <- rep(NA,length(unique_id))
AcademicYr.y2017.2018_subj <- rep(NA,length(unique_id))
AcademicYr.y2018.2019_subj <- rep(NA,length(unique_id))

i=1
for (id in unique_id){
  df_subj <- matched_complete[matched_complete$egoid==id,]
  
  steps_avg[i] <- mean(df_subj$steps)
  sleep_avg[i] <- mean(df_subj$tot_hr_sleep)
  GenderMale_subj[i] <- df_subj$GenderMale[1]
  RaceAfrican.American_subj[i] <- df_subj$RaceAfrican.American[1]
  RaceAsian.American_subj[i] <- df_subj$RaceAsian.American[1]
  RaceForeign.Student_subj[i] <- df_subj$RaceForeign.Student[1]
  RaceLatino.a_subj[i] <- df_subj$RaceLatino.a[1]
  RaceOther_subj[i] <- df_subj$RaceOther[1]
  Income.50.000....150.000_subj[i] <- df_subj$Income.50.000....150.000[1]
  Income...150.000_subj[i] <- df_subj$Income...150.000[1]
  BMI_subj[i] <- df_subj$BMI[1]
  AcademicYr.y2016.2017_subj[i] <- df_subj$AcademicYr.y2016.2017[1]
  AcademicYr.y2017.2018_subj[i] <- df_subj$AcademicYr.y2017.2018[1]
  AcademicYr.y2018.2019_subj[i] <- df_subj$AcademicYr.y2018.2019[1]
  
  i=i+1
}

agg_df <- data.frame(unique_id,
                     steps_avg,
                     sleep_avg,
                     GenderMale_subj,
                     RaceAfrican.American_subj,
                     RaceAsian.American_subj,
                     RaceForeign.Student_subj,
                     RaceLatino.a_subj,
                     RaceOther_subj,
                     Income.50.000....150.000_subj,
                     Income...150.000_subj,
                     BMI_subj,
                     AcademicYr.y2016.2017_subj,
                     AcademicYr.y2017.2018_subj,
                     AcademicYr.y2018.2019_subj)


ggdensity(agg_df$steps_avg)
ggqqplot(agg_df$steps_avg)
ggqqplot(agg_df$steps_avg*agg_df$steps_avg)
ggdensity(agg_df$steps_avg*agg_df$steps_avg)

agg_mod <- lm(steps_avg~ sleep_avg + GenderMale_subj+RaceAfrican.American_subj+
                RaceAsian.American_subj+RaceForeign.Student_subj+RaceLatino.a_subj+
                RaceOther_subj+Income.50.000....150.000_subj+Income...150.000_subj+
                BMI_subj+ AcademicYr.y2016.2017_subj+ AcademicYr.y2017.2018_subj+
                AcademicYr.y2018.2019_subj,data=agg_df)


summary(agg_mod)

agg_pred <- agg_mod$fitted.values

agg_df$ests <- agg_pred

ggplot(agg_df,aes(x=sleep_avg,y=ests))+geom_line(color="red")+
  geom_point(aes(x=sleep_avg,y=steps_avg),size=1)+
  ggtitle("Average Steps vs. Average Sleep")+
  xlab("Year-Average hours of sleep")+
  ylab("Year-Average steps")
  theme_bw()
  
  ## Try regressing on sleep squared
  
agg_df$sleep_sq <- agg_df$sleep_avg^2


agg_mod2 <- lm(steps_avg~ sleep_sq + GenderMale_subj+RaceAfrican.American_subj+
                RaceAsian.American_subj+RaceForeign.Student_subj+RaceLatino.a_subj+
                RaceOther_subj+Income.50.000....150.000_subj+Income...150.000_subj+
                BMI_subj,data=agg_df)

agg_df$ests2 <- agg_mod2$fitted.values

ggplot(agg_df,aes(x=sleep_sq,y=ests2))+geom_line(color="red")+
  geom_point(aes(x=sleep_sq,y=steps_avg),size=1)+
  ggtitle("Average Steps vs. Average Sleep Squared")+
  xlab("Year-Average hours of sleep squared")+
  ylab("Year-Average steps")
theme_bw()

AIC(agg_mod)
AIC(agg_mod2)

BIC(agg_mod)
BIC(agg_mod2)


