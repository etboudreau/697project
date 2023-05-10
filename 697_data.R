# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

#setwd('/Users/emmaboudreau/Documents/GitHub/697proj/')
#/Users/samuelesquivel/Documents/GitHub/697project
setwd('/Users/samuelesquivel/Documents/GitHub/697project/')


# read in the data-----

data = read.csv('export.csv')
  
 
#----

data_munge = select(data,-c(dist_dirc_exit, age))%>%
  filter(!is.na(speed_limit))%>% #filter out anything without speed limit
  rename("severity" = "crash_severity_descr")%>% #changed column name
  rename("weather" = "weath_cond_descr")%>%
  mutate(severity=ifelse(severity=="Property damage only (none injured)",0, 
          ifelse(severity=="Non-fatal injury",1,ifelse(severity=="Fatal injury",2,3)
           )
          )
         )%>%
  filter(severity!="3")

#different ways to check number of observations for things
  #filter(numb_fatal_injr!="0")
  #filter(severity=="1")
#none injured = 0, non fatal = 1, fatal = 2
#removed unreported or unknown

#---- 1 dataframe we could use
data_munge2 = select(data,-c(dist_dirc_exit, age, max_injr_svrty_cl,
                             numb_fatal_injr, numb_nonfatal_injr,injy_stat_descr,
                             vehc_unit_numb,crash_status,max_injr_svrty_vl))%>%
  filter(!is.na(speed_limit))%>% #filter out anything without speed limit
  rename("severity" = "crash_severity_descr")%>% #changed column name
  rename("weather" = "weath_cond_descr")%>%
  mutate(severity=ifelse(severity=="Property damage only (none injured)",0, 
                         ifelse(severity=="Non-fatal injury",1,ifelse(severity=="Fatal injury",2,3)
                         )
  )
  )%>%
  filter(severity!="3")%>% #remove any unreported or unknown severity levels
  drop_na()%>% #drop any row with NA
  mutate(weather = ifelse(weather =="Clear/Clear","Clear",ifelse(weather == "Rain/Rain","Rain",
                          ifelse(weather=="Not Reported","Unknown",
                                 ifelse(weather=="Snow/Snow","Snow",weather)
                              )
                          )
                      )
                )%>% #concatenating weather conditions 
  mutate(weather=ifelse(weather=="Clear",0, 
                         ifelse(weather=="Cloudy",1,ifelse(weather=="Snow",2, 
                                                      ifelse(weather=="Rain",3,
                                                           ifelse(weather=="Unknown",5,weather)
                         )
                   )
             )
      )
) #creating numerical code for different weather conditions

         
  







