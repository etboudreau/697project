# script to read in raw data and organize it
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(stringi)

setwd('/Users/emmaboudreau/Documents/GitHub/697proj/')

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



         
  







