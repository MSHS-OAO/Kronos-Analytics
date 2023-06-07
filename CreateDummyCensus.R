# Code for creating dummy census data

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

patient_level_census <- read_excel(
  path = paste0(root_path,
                "HSPI-PM/Operations Analytics and Optimization",
                "/Projects/System Operations/Kronos Analytics",
                "/Data/Dummy Data/",
                "MSQ System Daily Huddle Census 06062023 1044AM.xlsx"
  )
)

census_summary <- patient_level_census %>%
  group_by(Department) %>%
  summarize(Census = n())

msq_ip_capacity <- read_excel(
  path = paste0(root_path,
                "HSPI-PM/Operations Analytics and Optimization",
                "/Projects/System Operations/Kronos Analytics",
                "/Data/Dummy Data/",
                "MSQ IP Capacity.xlsx"
  )
)

msq_ip_dept <- msq_ip_capacity$Department

dates <- seq.Date(from = as.Date("5/1/23", "%m/%d/%y"),
                  by = "day",
                  length.out = 10)

pull_time <- c(1, 2, 3, 4)

census_df <- expand.grid(
  "Site" = "MSQ",
  "Department" = msq_ip_dept,
  "Date" = dates,
  "TimePull" = pull_time
)

census_df <- census_df %>%
  arrange(Department, Date, TimePull)

census_df <- left_join(census_df, msq_ip_capacity,
                       by = c("Department" = "Department")
)

census_df$Census <- sapply(census_df$Capacity,
                           function(x) {
                             sample(as.integer(.75*x):x, 1)
                           }
)

saveRDS(census_df,
        file = paste0(root_path,
                      "HSPI-PM/Operations Analytics and Optimization",
                      "/Projects/System Operations/Kronos Analytics",
                      "/Data/Dummy Data/",
                      "MSQ_Dummy_Census_",
                      format(Sys.Date(), "%m%d%Y"),
                      ".RDS"))
