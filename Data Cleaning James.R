# Phytoplankton Data Cleaning James
library(tidyverse)
library(dplyr)

phytos_9606 <- read_csv('Phytos_1996-2006.csv')
phytos_0812 <- read_csv('Phytos_2008-2012.csv')
phyto_info <- read_csv('Phyto_info_WQdata.csv')

# Combining data from all time periods
phytos_whole <- phytos_9606 %>% bind_rows(phytos_0812)

# Adding water quality data, joined by SHEETBAR (ID) of sample
phyto <- phytos_whole %>% left_join(phyto_info,by="SHEETBAR")

# In the future -- rename variables & select only variables that are interesting