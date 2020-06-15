# Phytoplankton Data Cleaning James
library(tidyverse)
library(dplyr)
library(lubridate)

phytos_9606 <- read_csv('Phytos_1996-2006.csv')
phytos_0812 <- read_csv('Phytos_2008-2012.csv')
phyto_info <- read_csv('Phyto_info_WQdata.csv')

# Combining data from all time periods
phytos_whole <- phytos_9606 %>% bind_rows(phytos_0812)

# Adding water quality data, joined by SHEETBAR (ID) of sample
phyto <- phytos_whole %>% left_join(phyto_info,by="SHEETBAR")

# Cleaning the dates
phyto$DATE <- phyto$DATE %>% mdy()

# Removing outliers
## Following the lead of Kireta et al., remove outliers that have values outside
##    of a 95% C.I. for biomass, density, and water quality measures?
## Also, which water quality measures are we using here?