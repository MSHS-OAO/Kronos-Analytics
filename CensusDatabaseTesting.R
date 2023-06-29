# Test connection to Oracle database

# rm(list = ls())

# Load libraries ---------------
# library(readxl)
# library(writexl)
# library(ggplot2)
# library(lubridate)
# library(dplyr)
# library(reshape2)
# library(svDialogs)
# library(stringr)
# library(formattable)
# library(scales)
# library(ggpubr)
# library(reshape2)
# library(knitr)
# library(kableExtra)
# library(rmarkdown)
# library(zipcodeR)
# library(tidyr)
# library(purrr)
# library(janitor)
# library(DT)
# library(shiny)
# library(shinyWidgets)
# library(shinydashboard)
# library(odbc)
# library(dbplyr)
# library(DBI)
# library(glue)
# library(knitr)

# Root path -----------
# Determine root path for  shared drive on R Workbench
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

print("0")
epic_dsn <- "Clarity_prod_kate"
oao_personal_dsn <- "OAO Cloud DB Kate"
print("1")

epic_conn <- dbConnect(odbc(), epic_dsn)

oao_personal_conn <- dbConnect(odbc(),
                               oao_personal_dsn)
print("3")

census_refresh <- tbl(epic_conn,
                 in_schema("CRREPORT_REP", "MSHS_CENSUS_TEMP")) %>%
  collect()

census_summary <- census_refresh %>%
  distinct() %>%
  filter(ADMIT_STATUS %in% "Admission") %>%
  group_by(HOSPITAL_AREA, DEPARTMENT, CURRENT_DATE, CURRENT_TIME) %>%
  summarize(CENSUS = n()) %>%
  ungroup() %>%
  mutate(REFRESH_TIME = as_datetime(paste0(CURRENT_DATE, " ", CURRENT_TIME),
         format = "%m/%d/%Y %I:%M %p",
         tz = "EST")) %>%
  rename(SITE = HOSPITAL_AREA) %>%
  select(-CURRENT_DATE, -CURRENT_TIME)

saveRDS(census_summary,
        file = paste0(root_path,
                      "HSPI-PM/Operations Analytics and Optimization/Projects/",
                      "System Operations/Kronos Analytics/Data/Epic Clarity Census/",
                      "EpicClarityCensusPull_",
                      format(unique(census_summary$REFRESH_TIME), "%Y-%m-%d %H%M"),
                      ".RDS"))

get_values <- function(x, table_name){
  
  site <- x[1]
  dept <- x[2]
  census <- x[3]
  refresh_time <- x[4]
  
  values <- glue("INTO \"{table_name}\" (SITE,DEPARTMENT,CENSUS,REFRESH_TIME) 
                 VALUES ('{site}','{dept}','{census}',TO_TIMESTAMP('{refresh_time}','YYYY-MM-DD HH24:MI:SS'))")
  
  return(values)
}


processed_input_data <- census_summary

DATA_TYPES <- c(SITE = "Varchar2(50 CHAR)",
                DEPARTMENT = "Varchar2(50 CHAR)",
                CENSUS = "Number(38,0)",
                REFRESH_TIME = "Timestamp")
    
TABLE_NAME <- paste0("CENSUS_TEST")
    

processed_input_data <- processed_input_data %>%
  mutate(SITE = as.character(SITE),
         DEPARTMENT = as.character(DEPARTMENT),
         CENSUS = as.integer(CENSUS),
         REFRESH_TIME = format(REFRESH_TIME, "%Y-%m-%d %H:%M")
         )

# Convert the each record/row of tibble to INTO clause of insert statment
inserts <- lapply(
  lapply(
    lapply(split(processed_input_data , 
                 1:nrow(processed_input_data)),
               as.list), 
        as.character),
      FUN = get_values ,TABLE_NAME)

# simple_test <- inserts[[1]]
    
values <- glue_collapse(inserts,sep = "\n\n")

# Combine into statements from get_values() function and combine with
# insert statements
all_data <- glue('INSERT ALL
                        {values}
                      SELECT 1 from DUAL;')
# all_data <- glue('INSERT ALL
#                  {values};')
    
print("before conn")
# conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
    #                   dsn = dsn)
    
oao_personal_conn <- dbConnect(odbc(),
                                   oao_personal_dsn)
    
dbExecute(oao_personal_conn,all_data)

