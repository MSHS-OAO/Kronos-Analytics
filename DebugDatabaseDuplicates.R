rm(list = ls())

# Load libraries ---------------
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

# Root path -----------
# Determine root path for  shared drive on R Workbench
define_root_path <- function(){
  #Check if directory is from R Workbench; starts with '/home'
  if(grepl("^/home", dirname(getwd()))){
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

# Census connection and default filters --------
# Connect to OAO Cloud Database census data
oao_personal_dsn <- "OAO Cloud DB Kate"
census_conn <- dbConnect(odbc(),
                         oao_personal_dsn)

census_tbl <- tbl(census_conn, "CENSUS_TEST")

census_df <- census_tbl %>%
  collect()

# census_deleted_rows <- tbl(census_conn, "DELETE_DUPL") %>%
#   collect()

# census_tbl_remove_dupl <- tbl(census_conn, "CENSUS_DUPL_TEST")
# 
# census_df_remove_dupl <- census_tbl_remove_dupl %>%
#   collect()

dbDisconnect(census_conn)

census_test_filter <- census_df %>%
  mutate(Concate = paste(SITE, DEPARTMENT, REFRESH_TIME),
         Dupl = duplicated(Concate)) %>%
  filter(!Dupl) %>%
  arrange(SITE, DEPARTMENT, REFRESH_TIME) %>%
  select(-Concate, -Dupl) #%>%
  # relocate(CENSUS, .after = REFRESH_TIME)

census_deleted_rows <- census_deleted_rows %>%
  arrange(SITE, DEPARTMENT, REFRESH_TIME)



# census_df_dupl_removed <- census_df_remove_dupl %>%
#   arrange(SITE, DEPARTMENT, REFRESH_TIME)

dupl_times <- census_df %>%
  mutate(Concate = paste(SITE, DEPARTMENT, REFRESH_TIME),
         Dupl = duplicated(Concate)) %>%
  filter(Dupl) %>%
  select(REFRESH_TIME) %>%
  distinct()
