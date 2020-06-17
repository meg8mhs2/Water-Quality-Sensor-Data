#Cleaning the Data: MC

library(tidyverse)
library(dplyr)
library(lubridate)
library(fpc)
library(cluster)
set.seed(2002)
library(mice)

filename1 <- file.path("MC_WQ.csv")
data_MC_raw <- read_csv(filename1)

#Outliers:
# Turb
# Single outliers (high):
#  Sept 25, 2017 (01:03)
# August 20, 2017 (03:10)
# 
# Single outliers (low)
# June 30, 2015 (both times)
# Nov 2, 2016 (12:52)
# Oct 26, 2017 (there is no data for the 3 days preceeding this point)

Clustered<-read.csv("ClusteredDataMC.csv")
HighTurb<- Clustered%>% filter(Cluster==6)
HighTurb

# Aug 13-14 (2016), July 21-23 (2017)
# 
# CHL
# Single (low) outliers seen on the all-data graph
# June 30-July 1, 2015 (all times)
# June 17, Aug 18, 2016 (15:16, 13:28)
# May 11, Sept 15 2017 (11:02, 10:23)
# May 10, July 29-Aug 1, 2018 (all times)
# {Note that many of these points occur at the very beginning/end of testing periods}
# 
# BGA
# Single Outlier (low):
# June 30, 2015 (18:06) At the beginning of the data
# 
# FDOM
# Single Outliers (low):
#  June 30, 2015 (17:53, 18:06) {This date comes up a lot, it should be taken out of the set}
# Nov 2, 2016 (12:52)
# Aug 10, Oct 26 (12:08, 11:19)
# May 21, 2018 (09:38, 11:38, 13:38, 15:38)
# 
# NO3
# Single outliers (high):
#  June 30, 2015 (18:06)
#  June 13, 2017 (07:15)
# 
# Single Outliers (low):
#    June 30, 2015 (17:53)
#  May 16, Nov 2 2016 (10:34, 12:52)
#  June 14, Aug 10, Oct 26 2017 (12:08, 11:19)
# May 21 2018 (09:38, 11:38, 13:38, 15:38) {This has comeup multiple times, needs to be removed}

# OUTLIERS TO REMOVE (if its starred(*), it came up multiple times):
# June 30, 2015 (all****)
# July 1, 2015 (all)
# 
# May 16 (10:34)
# June 17 2016 (15:16)
# Aug 13 & 14, 2016 (all- Turb)
# Aug 18, 2016 (13:28)
# Nov 2, 2016 (12:52*)
# 
# May 11, 2017 (11:02)
# June 13, 2017 (07:15)
# June 14, 2017 (unknown time)
# July 21-23,2017 (all- Turb)
# Aug 10, 2017 (12:08*)
# August 20, 2017 (03:10)
# Sept 15 2017 (10:23)
# Sept 25, 2017 (01:03)
# Oct 26, 2017 (all**)
# 
# May 10, 2018 (all times)
# May 21, 2018 (09:38, 11:38, 13:38, 15:38**)
# July 29-Aug 1, 2018 (all times)



#IDENTIFYING the missing points-------------------------------------------
data_MC
MC_2018 <- data_MC_raw %>% filter(yr==2018) %>% select(c(samp.date="YYYY-MM-DD",samp.time="hh:mm",Turb,CHLugL, BGAugL, FDOMqsu, NO3_mgL)) %>% mutate(month = month(mdy(samp.date)), day=day(mdy(samp.date)), hour=hour(hms(samp.time)), samp.date.time = mdy(samp.date) + hms(samp.time))
ggplot(data=MC_2018,aes(x=day,y=hour))+geom_point()+facet_wrap(~month)
#June 22, 11 am
#July 10, 11 am

MC_2017 <- data_MC_raw %>% filter(yr==2017) %>% select(c(samp.date="YYYY-MM-DD",samp.time="hh:mm",Turb,CHLugL, BGAugL, FDOMqsu, NO3_mgL)) %>% mutate(month = month(mdy(samp.date)), day=day(mdy(samp.date)), hour=hour(hms(samp.time)), samp.date.time = mdy(samp.date) + hms(samp.time))
ggplot(data=MC_2017,aes(x=day,y=hour))+geom_point()+facet_wrap(~month)
# Lots of issues. Perhaps focus on July 8- October 7

MC_2016 <- data_MC_raw %>% filter(yr==2016) %>% select(c(samp.date="YYYY-MM-DD",samp.time="hh:mm",Turb,CHLugL, BGAugL, FDOMqsu, NO3_mgL)) %>% mutate(month = month(mdy(samp.date)), day=day(mdy(samp.date)), hour=hour(hms(samp.time)), samp.date.time = mdy(samp.date) + hms(samp.time))
ggplot(data=MC_2016,aes(x=day,y=hour))+geom_point()+facet_wrap(~month)
# No issues

MC_2015 <- data_MC_raw %>% filter(yr==2015) %>% select(c(samp.date="YYYY-MM-DD",samp.time="hh:mm",Turb,CHLugL, BGAugL, FDOMqsu, NO3_mgL)) %>% mutate(month = month(mdy(samp.date)), day=day(mdy(samp.date)), hour=hour(hms(samp.time)), samp.date.time = mdy(samp.date) + hms(samp.time))
ggplot(data=MC_2015,aes(x=day,y=hour))+geom_point()+facet_wrap(~month)
# Lots of issues. Perhaps focus July 2- Sept 30


#IDENTIFYING the NAs AND CLEANING--------------------------------------------------


#2018 CLEANED==================================================

#Identify NAs, remove those at the beginning/end of the data
md.pattern(MC_2018)
MC_2018_Clean<- MC_2018 %>% filter(!(month %in% c(5, 10)))
MC_2018_Clean %>% filter(is.na(Turb)) #Sept 6 (11:47)
MC_2018_Clean%>% filter(samp.date=="9/6/2018")

#Function to find the average for a data point that is set at NA
#DataSet is the data set
#DataSetVar is the data set with the var, in the format DataSet$Var
#DateTime is a String that is in the format "YYYY-MM-DD HH:MM:SS"
#The function returns the average value of the data directly above and below the inputted point (DateTime) for the given variable

reassign<- function(DataSet, DataSetVar, DateTime){
  t<-((DataSetVar[which(DataSet$samp.date.time==DateTime)-1]+
         DataSetVar[which(DataSet$samp.date.time==DateTime)-3])/2)
  return(t)
}


#Reassigning the NA values for all of the variables for the missing point Sept 6 at 11:47
MC_2018_Clean$Turb[(which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2)] <- reassign(MC_2018_Clean, MC_2018_Clean$Turb, "2018-09-06 11:47:00")

MC_2018_Clean$NO3_mgL[(which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2)] <- reassign(MC_2018_Clean, MC_2018_Clean$NO3_mgL, "2018-09-06 11:47:00")

MC_2018_Clean$CHLugL[(which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2)] <- reassign(MC_2018_Clean, MC_2018_Clean$CHLugL, "2018-09-06 11:47:00")

MC_2018_Clean$BGAugL[(which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2)] <- reassign(MC_2018_Clean, MC_2018_Clean$BGAugL, "2018-09-06 11:47:00")

MC_2018_Clean$FDOMqsu[(which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2)] <- reassign(MC_2018_Clean, MC_2018_Clean$FDOMqsu, "2018-09-06 11:47:00")

MC_2018_Clean%>% filter(samp.date=="9/6/2018")



## Trying different things
#t<- (MC_2018_Clean$Turb[MC_2018_Clean$samp.date.time=="2018-09-06 09:47:00"]+
#          MC_2018_Clean$Turb[MC_2018_Clean$samp.date.time=="2018-09-06 13:47:00"])/2
#  t
# MC_2018_Clean$Turb[which(MC_2018_Clean$samp.date.time=="2018-09-06 11:47:00")-2] <- t #Not really sure why we subtract 2
# MC_2018_Clean%>% filter(samp.date=="9/6/2018")


#A function that finds the average of 2 points
#DataSet is the data set
#DataSetVar is the data set with the var, in the format DataSet$Var
##DateTime is a String that is in the format "YYYY-MM-DD HH:MM:SS". Note that this should be the time and data point 
#directly AFTER where the inputted value would be
#The function returns the average value of the inputted data point and the one directly before, so that a new point can
#be added with the returned value.

Assign<- function(DataSet, DataSetVar, DateTime){
  t<-((DataSetVar[which(DataSet$samp.date.time==DateTime)-2]+
         DataSetVar[which(DataSet$samp.date.time==DateTime)-3])/2)
  return(t)
}

#A function that creates a new data point with the inputted values
#DataSet is the data set.
#Value is the value of the point directly after where the new point will be, in the format "YYYY-MM-DD HH:MM:SS"
#samp.date is the date of the new point, in the format "MM/DD/YYYY"
#samp.time is the time of the new point, in the format "HH:MM:SS"
#month, day and hour are integers of the new point
#samp.date.time is the date and time of the new point, in the format, "YYYY-MM-DD HH:MM:SS". 
#In error, the function adds 4 hours to the samp.date.time. As there is no current solution, one must substract 4 hours
#The function returns a new point with all of the given inputs
NewPoint<- function(DataSet, Value, samp.date, samp.time, month, day, hour, samp.date.time){
  NewPoint1Turb <-Assign(DataSet, DataSet$Turb, Value)
  NewPoint1CHL <-Assign(DataSet, DataSet$CHLugL, Value)
  NewPoint1BGA <-Assign(DataSet, DataSet$BGAugL, Value)
  NewPoint1FDOM <-Assign(DataSet, DataSet$FDOMqsu, Value)
  NewPoint1NO3 <-Assign(DataSet, DataSet$NO3_mgL, Value)
  NewPoint <- data.frame("samp.date"= samp.date, "samp.time"= samp.time, "Turb" = NewPoint1Turb, 
                         "CHLugL"= NewPoint1CHL,"BGAugL"= NewPoint1BGA,"FDOMqsu"= NewPoint1FDOM,
                         "NO3_mgL"= NewPoint1NO3, "month"=  month, "day"= day, "hour"= hour,
                         "samp.date.time"= samp.date.time)
  return(NewPoint)
}

MC_2018_Clean%>% filter(samp.date=="7/10/2018")
MC_2018_Clean%>% filter(samp.date=="6/22/2018")

#Adding the missing points, sorting the data
Attempt1<-NewPoint(MC_2018_Clean, "2018-06-22 13:26:00", "6/22/2018", "11:30:00", 6, 22, 11, "2018-06-22 07:30:00") #For some really odd reason, the samp.date.time adds 4 hours
Attempt2<-NewPoint(MC_2018_Clean, "2018-07-10 13:43:00", "7/10/2018", "11:35:00", 7, 10, 11, "2018-07-10 07:35:00")
MC_2018_Clean<- rbind(MC_2018_Clean, Attempt1, Attempt2)
MC_2018_Clean<- MC_2018_Clean %>% arrange(samp.date.time)

#Checking that its all good
MC_2018_Clean%>% filter(samp.date=="6/22/2018")
MC_2018_Clean%>% filter(samp.date=="7/10/2018")


#2017=====================================================
md.pattern(MC_2017)
MC_NA<- MC_2017%>% filter(is.na(Turb))
MC_NA
ggplot(data= MC_NA, aes(x=day, y=hour))+geom_point()+facet_wrap(~month)
#Sept 12  (13:13)
#Several other issues in October

#2016====================================================
md.pattern(MC_2016)
MC_NA<- MC_2016%>% filter(is.na(Turb))
ggplot(data= MC_NA, aes(x=day, y=hour))+geom_point()+facet_wrap(~month)
MC_NA
MC_NA %>% slice(31:41)
#May 16 (10:34) Has a nitrate reading
#June 17 (10:38)- June 20 (14:38)
#Oct 11 (10:49)

#2015: CLEANED===========================================
#identifying NAs
md.pattern(MC_2015)
MC_NA<- MC_2015%>% filter(is.na(Turb))
ggplot(data= MC_NA, aes(x=day, y=hour))+geom_point()+facet_wrap(~month)
MC_NA
MC_NA %>% slice(74:84)
#July 1 (17:37) - July 5 (4:37) Perhaps just start July 5

MC_2015_Clean <- MC_2015 %>% filter (!(month==7 & day %in% c(1, 2, 3, 4, 5))) %>% filter(!(month %in% c(6, 10, 11)))
# Removed about 400 observations, cleaned to July 5 - Sept 30
                                                                                         