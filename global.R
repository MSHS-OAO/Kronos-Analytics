# Global file for shiny app
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

# Load dummy census data ------
census_df <- readRDS(
  file = paste0(root_path,
                "HSPI-PM/Operations Analytics and Optimization",
                "/Projects/System Operations/Kronos Analytics",
                "/Data/Dummy Data/",
                "MSQ_Dummy_Census_06072023.RDS")
)

sites <- unique(census_df$Site)

departments <- unique(census_df$Department)

dates <- sort(unique(census_df$Date), decreasing = TRUE)

hosp_summary_df <- census_df %>%
  rename("Volume" = "Census") %>%
  mutate("TotalWorkedHours" = NA,
         "Worked FTE" = NA,
         "WHpU" = NA,
         "Total Paid Hours" = NA,
         "Total Paid FTE" = NA,
         "Total Overtime Hours" = NA,
         "Total Nonproductive Hours" = NA,
         "Total Agency Hours" = NA,
         "Total Education & Orientation Hours" = NA,
         "Total PTO Hours" = NA
  )