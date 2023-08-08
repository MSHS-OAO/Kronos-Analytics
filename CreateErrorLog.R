# Script to create initial error log

# Load packages ----------------
rm(list = ls())

library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)
library(ggpubr)
library(reshape2)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(zipcodeR)
library(tidyr)
library(purrr)
library(janitor)
library(DT)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(odbc)
library(dbplyr)
library(DBI)
library(glue)
library(knitr)

# Define root path --------------------
define_root_path <- function(){
  #Check if OS in Linux
  if(Sys.info()['sysname'] == 'Linux'){
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files("/SharedDrive/") == "Presidents",
           #Define prefix of path to share drive with R Workbench format
           output <- "/SharedDrive/Presidents/",
           output <- "/SharedDrive/deans/Presidents/")
  }#Check if directory is from R Studio; starts with an uppercase letter than ':'
  else if(grepl("^[[:upper:]]+:", dirname(getwd()))){
    #Determine which drive is mapped to Sharedrive (x)
    for(i in LETTERS){
      if(any(grepl("deans|Presidents", list.files(paste0(i, "://"))))){x <- i}
    }
    #Check if mapped Sharedrvie starts at folder Presidents or deans
    ifelse(list.files(paste0(x, "://")) == "Presidents",
           #Define prefix of path to share drive with R Studio format
           output <- paste0(x, ":/Presidents/"),
           output <- paste0(x, ":/deans/Presidents/"))
  }
  return(output)
} 

root_path <- define_root_path()

# Connect to OAO Cloud database and pull Target table data --------------
oao_personal_dsn <- "OAO Cloud DB Greg"

oao_personal_conn <- dbConnect(odbc(),
                               oao_personal_dsn)

# query target log to start data refresh error log
error_log_query <- glue(
  "SELECT
    T.SITE,
    T.DEPARTMENT,
    T.REFRESH_TIME,
    TRUNC(T.REFRESH_TIME) AS REFRESH_DATE,
    T.REFRESH_HOUR,
    T.REFRESH_NUMBER,
    T.DOW,
    T.CENSUS
   FROM TARGET T"
)

# execute error log query
error_log_data <- dbFetch(dbSendQuery(oao_personal_conn, error_log_query))

# Summarize data and identify days with missing refreshes  ---------------
error_log_refresh <- error_log_data %>%
  group_by(REFRESH_TIME, REFRESH_DATE, REFRESH_HOUR) %>%
  summarise(COUNT = n()) %>%
  group_by(REFRESH_DATE) %>%
  summarise(COUNT = n()) %>%
  filter(COUNT != 4,
         REFRESH_DATE != Sys.Date())

dates_missing_refresh <- error_log_data %>%
  filter(REFRESH_DATE %in% error_log_refresh$REFRESH_DATE)

#create df for all unique refresh hours and numbers
refresh_hour_number <- tbl(oao_personal_conn, "TARGET") %>%
  select(REFRESH_HOUR, REFRESH_NUMBER) %>%
  distinct() %>%
  arrange(REFRESH_NUMBER) %>%
  collect()

# create error log output df
error_log_final <- data.frame(
  REFRESH_DATE = rep.int(error_log_refresh$REFRESH_DATE, 4)) %>%
  arrange(REFRESH_DATE) %>%
  mutate(REFRESH_HOUR = rep.int(refresh_hour_number$REFRESH_HOUR,
                                length(error_log_refresh$REFRESH_DATE)),
         REFRESH_NUMBER = rep.int(refresh_hour_number$REFRESH_NUMBER,
                                  length(error_log_refresh$REFRESH_DATE))) %>%
  left_join(dates_missing_refresh, join_by(REFRESH_DATE == REFRESH_DATE,
                                           REFRESH_HOUR == REFRESH_HOUR,
                                           REFRESH_NUMBER == REFRESH_NUMBER)) %>%
  filter(is.na(SITE)) %>%
  mutate(REFRESH_DATE = as.Date(REFRESH_DATE)) %>%
  select(REFRESH_DATE, REFRESH_HOUR, REFRESH_NUMBER) %>%
  rename(MISSING_REFRESH_DATE = REFRESH_DATE,
         MISSING_REFRESH_HOUR = REFRESH_HOUR,
         MISSING_REFRESH_NUMBER = REFRESH_NUMBER)

# save error_log with date
write_xlsx(error_log_final,
           paste0(root_path, "HSPI-PM/Operations Analytics and Optimization",
                  "/Projects/System Operations/Kronos Analytics/Data",
                  "/Census Refresh Error Logs/",
                  "Census_Refresh_Error_Log_", Sys.Date(),".xlsx"))


