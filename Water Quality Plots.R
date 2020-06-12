#Variable Function, which can be used to graph water quality variables over time

library(tidyverse)
library(dplyr)
library(lubridate)
set.seed(2002)


#Data_Set_MC is the data set used. If it is set to true, the main channel is analyzed.
#If it is set to false, the back channel is analyzed
#ByDay is whether the graph graphs all data points, or the average per day. When ByDay is false,
#all points will be graphed
#Month is how the graph is organized. When it is set to false, it will show all the data over
#all 4 of the years. When it is set to true, it shows all of the data over a calendar year
#Measurement is the variable to analyze


Water_Quality_Graph<- function(Data_Set_MC= TRUE, ByDay= FALSE, Month= FALSE, Measurement){
  if(Data_Set_MC== TRUE){
    filename1 <- file.path("MC_WQ.csv")
  }
  else{
    filename1 <- file.path("SI_WQ.csv")
  }
  data_WQ_raw <- read_csv(filename1)
  
  dates <- mdy(data_WQ_raw$`YYYY-MM-DD`) %>% sort
  
  data_WQ<- data_WQ_raw %>% 
      mutate(Month = month(dates), Date= dates) %>%
      select(Date, Month, Turb, CHLugL, BGAugL, FDOMqsu, NO3_mgL)
  if(ByDay==TRUE){
    data_WQ <- data_WQ %>% 
      group_by(Date) %>%
      summarize(Turb = mean(Turb), CHLugL = mean(CHLugL), BGAugL= mean(BGAugL), FDOMqsu= mean(FDOMqsu), NO3_mgL = mean(NO3_mgL))
  }
  if(Month==FALSE){
    VarX= data_WQ$Date
    Name = "Year"
  }
  else{
    VarX=data_WQ$Month
    Name = "Month"
  }
  ggplot(data= data_WQ, mapping=aes_string(VarX, Measurement)) +
    geom_point() +
    xlab(Name) + 
    ggtitle("Water Quality Over Time")
}

#Sample Graphs
Water_Quality_Graph(Data_Set_MC= TRUE, Month= FALSE, Measurement='BGAugL')
Water_Quality_Graph(Data_Set_MC= FALSE, ByDay= TRUE, Month= FALSE, Measurement='BGAugL')
Water_Quality_Graph(Data_Set_MC= FALSE, Month= TRUE, Measurement='FDOMqsu')
Water_Quality_Graph(Data_Set_MC= TRUE, Month= TRUE, Measurement='Turb')



