#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Conductivity Data Analysis
#Coder: Nate Jones
#Date: 6/22/2021
#Purpose: Organize Conductivity Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#load packages
library(tidyverse) #join the cult
library(lubridate) #date handling
library(readxl) #read xl files

#load custom functions
source("R/read_xle.R")

#Define directories of interest
#   The data directory is where the sensor data is housed
data_dir<-"data/20210530/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Ready files ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create list of files ----------------------------------
#List of files
files<-list.files(data_dir, full.names =  T)

#Identify field log
field_log<-files[str_detect(files, ".xlsx")]

#Remove field log
files<-files[!str_detect(files, ".xlsx")]

#2.2 Create function to read files -------------------------
load_fun<-function(n){

  #Read file to extract header info
  header<-read_lines(files[1])
  
  #Define serial number
  SN<-which(startsWith(header, 'Serial'))
  SN<-SN+1
  SN<-header[SN]
  
  #Define where ts data starts
  m<-which(startsWith(header, 'Date'))
  m<-m-1
  
  #read file
  ts<-read_csv(files[1], skip=m) %>% 
    mutate(SN = SN)
  
  #Exort ts
  ts
}

#2.3 Apply function to all files----------------------------
ts<-lapply(seq(1,length(files)), load_fun) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Join field sheet data to ts data ------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Organize field log ------------------------------------
#Download field log
field_log<-read_xlsx(field_log)

#Wrangle datetime 
#note, read_xl  loaded date as integer (i.e., 44355)
field_log<-field_log %>% 
  #wrangle date format 
  mutate(
    Date = as.numeric(paste(Date)),
    Date = as_date(Date, origin="1899-12-30"),
    Date = as.POSIXct(format(Date), tz="America/Chicago" )) %>% 
  #add time
  mutate(
    Inj_Time = as.numeric(paste(Inj_Time)),
    Timestamp = Date + (Inj_Time*3600*24)) 

#Select columns of interest
field_log<-field_log %>% 
  select(
    LocationID, Lat, Long, Timestamp, 
    SN_background_1, SN_background_2, 
    SN_downstream_1, SN_downstream_2) %>% 
  rename(start_time = Timestamp)

#Define start and stop times
stop_time_fun<-function(n){

  #Creat list of sn's for specific injection
  sondes<-field_log %>% slice(n) %>% select(matches("SN_*")) 
  
  #Create long format of start times and SNs
  logs<-field_log %>% 
    select(matches("SN*")) %>% 
    pivot_longer(-start_time) 
  
  #Filter to sondes 
  logs<-logs %>% filter(value %in% sondes)
  
  #identify stop time
  stop_time<-logs %>% 
    filter(start_time>field_log$start_time[n]) %>% 
    summarise(stop_time = min(start_time)) %>% 
    mutate(LocationID = field_log$LocationID[n])
    
  #export endtime
  stop_time
}
stop_time<-lapply(seq(1,length(files)), stop_time_fun) %>% bind_rows()
field_log<-left_join(field_log, stop_time)

#Pivot Longer 
field_log<-field_log %>% 
  pivot_longer(
    -c(LocationID, Lat, Long, start_time, stop_time), 
    names_to='sonde_location', 
    names_prefix = 'SN_',
    values_to = "SN"
  ) 

#Join to ts data
field_log<-field_log %>% mutate(SN = as.numeric(paste(SN)))
ts<-ts %>% mutate(SN = as.numeric(paste(SN)))
ts<-left_join(ts, field_log)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 4: Print curves for each sonde -----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
field_log

