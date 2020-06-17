#Average By Day, Compared to the Discharge Data
library(tidyverse)
library(dplyr)
library(dataRetrieval)
library(lubridate)
set.seed(2002)

#Import the Data - No Change for added variables
filename1 <- file.path("MC_WQ.csv")
data_WQ_raw <- read_csv(filename1)
data_discharge_raw <- dataRetrieval::readNWISdv('05378500', '00060', startDate =  "2015-06-30", endDate = "2018-10-12")


#Dealing with the dates- No Change for added variables
dates <- as.character(mdy(data_WQ_raw$`YYYY-MM-DD`)) %>% sort

#Incorporate the dates and remove extra variables
#For added variables: Add to select, filter NAs
data_WQ_raw_2 <- data_WQ_raw %>% 
  mutate(Date_WQ_Char= dates) %>%  
  select(Date_WQ_Char, Turb, Temp) %>%
  filter(Turb != "NA") 

#Group the dates
#For added variables: Add to summaraize
data_WQ_raw_3<- data_WQ_raw_2 %>% 
  group_by(Date_WQ_Char) %>% 
  summarize(Average_Turb = mean(Turb), Average_Temp= mean(Temp))

#Add the years, months and days as separate variables, No change for added variables
data_WQ <- data_WQ_raw_3 %>%
  mutate(Date_WQ= as_date(Date_WQ_Char), Month_WQ = month(data_WQ_raw_3$Date_WQ_Char), Day_WQ= day(data_WQ_raw_3$Date_WQ_Char), Year_WQ=as.character(year(data_WQ_raw_3$Date_WQ_Char))) %>%
  select(!Date_WQ_Char) %>%
  filter(Average_Turb<30)
data_WQ

#Clean discharge data
dates_dis <- ymd(data_discharge_raw$Date) %>% sort
data_Discharge <- data_discharge_raw %>% 
  mutate(Date_Dis= dates_dis) %>% 
  mutate(Discharge= `X_00060_00003`) %>% 
  select(Date_Dis, Discharge)
data_Discharge

Joint_Data<-left_join(data_WQ, data_Discharge, by=c("Date_WQ"="Date_Dis"))

#Turbidity Against Discharge, Removing Outliers
Plot<- ggplot(data = Joint_Data, mapping=aes(x= Discharge, y=Average_Turb))+
  geom_point(aes(color=Year_WQ)) +
  geom_smooth(method="lm", se=FALSE)
Plot

