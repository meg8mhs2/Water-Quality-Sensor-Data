#Average By Day, Compared to the Discharge Data
library(tidyverse)
library(dplyr)
library(dataRetrieval)
library(lubridate)
library(fpc)
library(cluster)
library(gridExtra)
set.seed(2002)
#MC2018.csv, MC2018Day.csv, ClusteredDataMC, MCByDay, SIByDay, ClusteredDataMCNoDis.csv

#Import the Data
filename1 <- file.path("MC_WQ.csv")
data_MC_raw <- read_csv(filename1)

filename2 <- file.path("SI_WQ.csv")
data_SI_raw <- read_csv(filename2)

data_MC<- data_MC_raw %>% mutate(Date_MC= mdy(`YYYY-MM-DD`), Month_MC = month(mdy(`YYYY-MM-DD`)), Year_MC = year(mdy(`YYYY-MM-DD`))) %>% select(Date_MC, Month_MC, Year_MC, Turb, CHLugL, BGAugL, FDOMqsu, NO3_mgL, `hh:mm`)
data_SI<- data_SI_raw %>% mutate(Date_SI= mdy(`YYYY-MM-DD`), Month_SI = month(mdy(`YYYY-MM-DD`)), Year_SI = year(mdy(`YYYY-MM-DD`))) %>% select(Date_SI, Month_SI, Year_SI, Turb, CHLugL, BGAugL, FDOMqsu, NO3_mgL, `hh:mm`)

data_MC_day <- data_MC %>% 
  group_by(Date_MC) %>%
  summarize(Turb = mean(Turb), CHLugL = mean(CHLugL), BGAugL= mean(BGAugL), FDOMqsu= mean(FDOMqsu), NO3_mgL = mean(NO3_mgL))

data_SI_day <- data_SI %>% 
  group_by(Date_SI) %>%
  summarize(Turb = mean(Turb), CHLugL = mean(CHLugL), BGAugL= mean(BGAugL), FDOMqsu= mean(FDOMqsu), NO3_mgL = mean(NO3_mgL))

data_discharge_raw <- dataRetrieval::readNWISdv('05378500', '00060', startDate =  "2015-06-30", endDate = "2018-10-12")

#Clean discharge data
data_Discharge <- data_discharge_raw %>%
  rename(Discharge= `X_00060_00003`) %>% 
  select(Date, Discharge)

#Join the data
Joint_Data_MC<-left_join(data_MC_day, data_Discharge, by=c("Date_MC"="Date")) %>% filter(!is.na(Turb), !is.na(CHLugL), !is.na(BGAugL), !is.na(FDOMqsu), !is.na(NO3_mgL), !is.na(Discharge))
summary(Joint_Data_MC)
Joint_Data_SI<-left_join(data_SI_day, data_Discharge, by=c("Date_SI"="Date"))  %>% filter(!is.na(Turb), !is.na(CHLugL), !is.na(BGAugL), !is.na(FDOMqsu), !is.na(NO3_mgL), !is.na(Discharge))

#Cluster
MC.scaled <- scale(Joint_Data_MC[-c(1)])
MC.k8 <- kmeans(MC.scaled, centers=8, iter.max=100, nstart=25)
MC.k8
MC.k6 <- kmeans(MC.scaled, centers=6, iter.max=100, nstart=25)
MC.k6

#Plot to show cluster
plot(Joint_Data_MC$Date_MC, MC.k8$cluster)
plot(Joint_Data_MC$Date_MC, MC.k6$cluster)


MC.k8.clust <- lapply(1:8, function(nc) Joint_Data_MC$Date_MC[MC.k8$cluster==nc])  
MC.k8.clust

#MC.pc <- princomp(Joint_Data_MC[,-c(1)],cor=T)
#MC.pc

#Visualize 2 components
clusplot(MC.scaled, MC.k8$cluster, color=TRUE, shade=TRUE)
plotcluster(MC.scaled, MC.k8$cluster)
 
#How many groups??? Eight!
n <- nrow(MC.scaled)
wss <- rep(0, 10)
wss[1] <- (n - 1) * sum(apply(MC.scaled, 2,var))
for (i in 2:7){
  wss[i] <- sum(kmeans(MC.scaled, centers = i, iter.max=100, nstart=25)$withinss)
  plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
}

#Join the data with the groups now
Joint_Data <- Joint_Data_MC %>% mutate(Cluster= MC.k8$cluster)

Joint_Data<- read.csv("ClusteredDataMC.csv")



#Graphing the Results

time_joint_data <- Joint_Data %>% filter(year(Date_MC)==2016) %>% mutate(Month= month(Date_MC))
mean(time_joint_data$Discharge) #5500
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= date(Date_MC), y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))+ 
  scale_color_discrete(name= "Cluster", labels= c("1: High Chlorophyll", "2: High Nitrate", "3: Smaller Storms", "5: Average Quality", "6: High Turbidity", "8: High Discharge")) +
  scale_x_date("Month", date_breaks="months", date_labels = c("Oct", "April", "May", "June", "July", "Aug", "Sept"))+
  ggtitle("Main Channel Discharge in 2018")


  
?scale_color_discrete
Time_Discharge
#1: Very low levels (July)
#2: Low levels
#3: Lower peaks and points between large peaks
#5: Fast rising
#8: Top peaks

time_joint_data <- Joint_Data %>% filter(year(Date_MC)==2017)
mean(time_joint_data$Discharge) #40537
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date_MC, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Mid between top peak and trough
#2: Mid levels
#3: Lower peaks and points between large peaks
#5: Trough
#8: Top peaks


#Note there is a large gap in late October 

time_joint_data <- Joint_Data %>% filter(year(Date_MC)==2018)
mean(time_joint_data$Discharge) #56078
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date_MC, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Troughs
#3: Lower peaks and points between large peaks
#5: Nearing troughs
#8: Top peaks

time_joint_data <- Joint_Data %>% filter(year(Date_MC)==2015)
mean(time_joint_data$Discharge) #28307
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date_MC, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Decreasing
#2: Peak
#4: Dec and low (Oct)
#7: Mid levels and peaks (Sept)

?geom_line
Time_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= yday(Date_MC), y=Discharge, group= as.character(year(Date_MC)))) +
  geom_line() +
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Low discharge, high CHL, BGA
#2: Higher averages, after peaks, high nitrate
#3: Smaller peaks (smaller storms). Average nitrate
#4: Low discharge, BGA
#5: Average, low discharge
#6: High turbidity
#7: Low and same time/year, low nitrate and FDOM
#8: Peaks

#You can see 5 has high discharge and mid nitrate, 2 has high nitrate and mid discharge, 4, 8, 3 have low discharge and nitrate
Nitrate_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=NO3_mgL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Nitrate_Discharge

#You can clearly see group 4 has low discharge and BGA, 8 has low Discharge and high BGA, 5 has high Discharge and mid-low BGA
BGA_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=BGAugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
BGA_Discharge

#5 is high discharge, 4 is 2016 and low discharge, 3 is low discharge, 2 is early 2017
Time_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Date_MC, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge

#2 is high nitrate, mid BGA, 6 is mid both, 8 is high BGA and low nitrate
BGA_Nitrate <- ggplot(data= Joint_Data, mapping= aes(x= NO3_mgL, y=BGAugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
BGA_Nitrate

#1 is low FDOM and discharge, mid BGA, 5 is high FDOM and discharge
FDOM_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=FDOMqsu))+
  geom_point(mapping=aes(color= as.character(Cluster)))
FDOM_Discharge

#8 has high CHL and low discharge
CHL_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=CHLugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
CHL_Discharge



#Saving the Clusters

?write.csv
write.csv(Joint_Data, file="ClusteredDataMC.csv", row.names = FALSE)

Joint_Data<- read.csv("ClusteredDataMC.csv")




#All Years Plots for Presentation

mean(time_joint_data$Discharge) #5500
Time_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= yday(Date_MC), y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))+ 
  scale_color_discrete(name= "Cluster", labels= c("1: High Chlorophyll", "2: High Nitrate", "3: Smaller Storms", "4:Low Discharge", "5: Average Quality", "6: High Turbidity", "7: Low Nitrate", "8: High Discharge")) +
  #scale_x_date("Day of the Year", date_breaks="months", date_labels = c("Oct", "April", "May", "June", "July", "Aug", "Sept"))+
  xlab("Day of the Year")+
  ggtitle("Main Channel Discharge") +
  facet_wrap(~year(Date_MC))
Time_Discharge

MC.scale_data<- Joint_Data %>% select(!Cluster) 
#%>% filter(!is.na(Turb), !is.na(CHLugL), !is.na(BGAugL), !is.na(FDOMqsu), !is.na(NO3_mgL), !is.na(Discharge))
MC.scaled <- scale(MC.scale_data[,-c(2)])
plotcluster(MC.scaled, Joint_Data$Cluster, xlab= "Discharge (High<->Low)", ylab= "Blue Green Algae (High<->Low)", title= "Clustered Main Channel Data")

BGA_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=BGAugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))+
  ylab("Blue Green Algae")+
  scale_color_discrete(name= "Cluster", labels= c("1: High Chlorophyll", "2: High Nitrate", "3: Smaller Storms", "4:Low Discharge", "5: Average Quality", "6: High Turbidity", "7: Low Nitrate", "8: High Discharge")) +
  ggtitle("Blue green Algae vs Discharge for the Main Channel")
CHL_Discharge <- ggplot(data= Joint_Data, mapping= aes(x= Discharge, y=CHLugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))+
  ylab("Chlorophyll")+
  scale_color_discrete(name= "Cluster", labels= c("1: High Chlorophyll", "2: High Nitrate", "3: Smaller Storms", "4:Low Discharge", "5: Average Quality", "6: High Turbidity", "7: Low Nitrate", "8: High Discharge")) +
  ggtitle("Chlorophyll vs Discharge for the Main Channel")

grid.arrange(BGA_Discharge, CHL_Discharge)



#Re-bring in the data :)
MC_Data_NoDis<- read.csv("MCByDay.csv")

MC_Data_NoDis_Clean <-MC_Data_NoDis %>% filter(!is.na(NO3_mgL), !is.na(CHLugL), !is.na(BGAugL), !is.na(Turb), !is.na(FDOMqsu))
#Cluster without the discharge
MC.scaled <- scale(MC_Data_NoDis_Clean[-c(1, 7)])
MC.k7 <- kmeans(MC.scaled, centers=7, iter.max=100, nstart=25)
MC.k7



#How many groups??? Seven!
n <- nrow(MC.scaled)
wss <- rep(0, 10)
wss[1] <- (n - 1) * sum(apply(MC.scaled, 2,var))
for (i in 2:6){
  wss[i] <- sum(kmeans(MC.scaled, centers = i, iter.max=100, nstart=25)$withinss)
  plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")
}


MC.k8.clust <- lapply(1:7, function(nc) MC_Data_NoDis_Clean$Date[MC.k7$cluster==nc])  
MC.k8.clust

#Visualize 2 components
clusplot(MC.scaled, MC.k7$cluster, color=TRUE, shade=TRUE)
plotcluster(MC.scaled, MC.k7$cluster)

#Join the data with the groups now
Clustered_No_Dis <- MC_Data_NoDis_Clean %>% mutate(Cluster= MC.k7$cluster)


Joint_Data<- read.csv("ClusteredDataMC.csv") #Only do to redo graphs! 

time_joint_data <- Clustered_No_Dis %>% filter(year(Date)==2016) %>% mutate(Month= month(Date))
mean(time_joint_data$Discharge) #5500
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= date(Date), y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))#+ 
  #scale_color_discrete(name= "Cluster", labels= c("1: High Chlorophyll", "2: High Nitrate", "3: Smaller Storms", "5: Average Quality", "6: High Turbidity", "8: High Discharge")) +
  #scale_x_date("Month", date_breaks="months", date_labels = c("Oct", "April", "May", "June", "July", "Aug", "Sept"))+
  #ggtitle("Main Channel Discharge in 2018")
Time_Discharge
#6: Low discharge
#1: High-mid discharge
#7: Early high discharge and rising

time_joint_data <- Clustered_No_Dis %>% filter(year(Date)==2017)
mean(time_joint_data$Discharge) #40537
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: High discharge
#7: Low discharge
#4: Very specific peak
#6: Right at the beginning


time_joint_data <- Clustered_No_Dis %>% filter(year(Date)==2018)
mean(time_joint_data$Discharge) #56078
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Peaks
#4: Low levels
#6: Very specific point
#7: Lower/mid

time_joint_data <- Clustered_No_Dis %>% filter(year(Date)==2015)
mean(time_joint_data$Discharge) #28307
Time_Discharge <- ggplot(data= time_joint_data, mapping= aes(x= Date, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Peak
#2: Lower discharge, negative slope, end of season
#3: Mid of season, mid levels
#4: Decreasing
#6: Reaching peaks
#7: Top decreasing

?geom_line
Time_Discharge <- ggplot(data= Clustered_No_Dis, mapping= aes(x= yday(Date), y=Discharge, group= as.character(year(Date)))) +
  geom_line() +
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge
#1: Low discharge, high CHL, BGA
#2: Higher averages, after peaks, high nitrate
#3: Smaller peaks (smaller storms). Average nitrate
#4: Low discharge, BGA
#5: Average, low discharge
#6: High turbidity
#7: Low and same time/year, low nitrate and FDOM
#8: Peaks

#You can see 5 has high discharge and mid nitrate, 2 has high nitrate and mid discharge, 4, 8, 3 have low discharge and nitrate
Nitrate_Discharge <- ggplot(data= Clustered_No_Dis, mapping= aes(x= Discharge, y=NO3_mgL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Nitrate_Discharge

#You can clearly see group 4 has low discharge and BGA, 8 has low Discharge and high BGA, 5 has high Discharge and mid-low BGA
BGA_Discharge <- ggplot(data=Clustered_No_Dis, mapping= aes(x= Discharge, y=BGAugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
BGA_Discharge

#5 is high discharge, 4 is 2016 and low discharge, 3 is low discharge, 2 is early 2017
Time_Discharge <- ggplot(data= Clustered_No_Dis, mapping= aes(x= Date, y=Discharge))+
  geom_point(mapping=aes(color= as.character(Cluster)))
Time_Discharge

#2 is high nitrate, mid BGA, 6 is mid both, 8 is high BGA and low nitrate
BGA_Nitrate <- ggplot(data= Clustered_No_Dis, mapping= aes(x= NO3_mgL, y=BGAugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
BGA_Nitrate

#1 is low FDOM and discharge, mid BGA, 5 is high FDOM and discharge
FDOM_Discharge <- ggplot(data= Clustered_No_Dis, mapping= aes(x= Discharge, y=FDOMqsu))+
  geom_point(mapping=aes(color= as.character(Cluster)))
FDOM_Discharge

#8 has high CHL and low discharge
CHL_Discharge <- ggplot(data= Clustered_No_Dis, mapping= aes(x= Discharge, y=CHLugL))+
  geom_point(mapping=aes(color= as.character(Cluster)))
CHL_Discharge



#Saving the Clusters

?write.csv
write.csv(Clustered_No_Dis, file="ClusteredDataMCNoDis.csv", row.names = FALSE)

Joint_Data<- read.csv("ClusteredDataMC.csv")


