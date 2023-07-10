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

site_choices <- census_tbl %>%
  select(SITE) %>%
  summarize(SITE = unique(SITE)) %>%
  collect() %>%
  arrange()

# default_site <- site_choices$SITE[1]
default_site <- "MOUNT SINAI QUEENS"
  
date_choices <- census_tbl %>%
  filter(SITE == default_site) %>%
  select(REFRESH_TIME) %>%
  summarize(REFRESH_TIME = unique(REFRESH_TIME)) %>%
  collect() %>%
  mutate(DATE = date(REFRESH_TIME)) %>%
  select(DATE) %>%
  arrange(desc(DATE)) %>%
  distinct()

default_date <- date_choices$DATE[2]

dept_choices <- census_tbl %>%
  filter(SITE == default_site) %>%
  select(DEPARTMENT) %>%
  summarize(DEPARTMENT = unique(DEPARTMENT)) %>%
  collect() %>%
  arrange(DEPARTMENT)

default_dept <- dept_choices$DEPARTMENT[1]

dbDisconnect(census_conn)

# Import dummy labor data ------------------
refresh_times <- data.frame("TimePull" = c(1, 2, 3, 4),
                            "Time" = c("08:00AM", "04:00PM", "08:00PM", "11:00PM"))

# Load dummy labor data
labor_df <- readRDS(
  file = paste0(root_path,
                "HSPI-PM/Operations Analytics and Optimization",
                "/Projects/System Operations/Kronos Analytics",
                "/Data/Dummy Data/",
                "MSQ_Dummy_Labor_06122023.RDS")
)

paycode_mappings <- read_excel(
  path = paste0(root_path,
                "HSPI-PM/Operations Analytics and Optimization",
                "/Projects/System Operations/Kronos Analytics",
                "/Data/",
                "PaycodeMapping_2023-06-21.xlsx")
)

labor_df <- labor_df %>%
  mutate(TimePull = as.integer(TimePull))

labor_df <- left_join(labor_df, refresh_times, by = c("TimePull" = "TimePull"))

labor_df <- labor_df %>%
  select(-TimePull) %>%
  mutate(Date = Date + (max(date_choices$DATE) - max(.$Date)),
         Site = "MOUNT SINAI QUEENS")

# MSHS Colors -----

# Mount Sinai corporate colors
# Update color palette based on brand manual and set medium and light to 50% and 25% tint
MountSinai_colors <- c(
  `dark purple`  = "#221F72",
  `dark pink`    = "#d80b8c",
  `dark blue`    = "#00aeef",
  `dark grey`    = "#58595B",
  `yellow`       = "#ffc000",
  `purple`       = "#7030a0",
  `med purple`   = "#918FB9",
  `med pink`     = "#EC85C6",
  `med blue`     = "#80D7F7",
  `med grey`     = "#ACACAD",
  `light purple` = "#C8C7DC",
  `light pink`   = "#F5C2E2",
  `light blue`   = "#BFEBFB",
  `light grey`   = "#D5D6D6"
)

# Function to extract Mount Sinai colors as hex codes
# Use Character names of MountSinai_colors

MountSinai_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MountSinai_colors)
  
  MountSinai_colors[cols]
}

# Color Function that can be used to call all colors is "MountSinai_cols()"
# Use in ggplot 

#MountSinai_cols()       # will provide all colors and their hex codes in a table 
#MountSinai_cols("pink") # will provide color name and the hex code for the pink color

# Create palettes 
MountSinai_palettes <- list(
  `all`   = MountSinai_cols("dark purple","dark pink","dark blue","dark grey",
                            "med purple","med pink","med blue","med grey", 
                            "light purple","light pink","light blue","light grey"),
  
  `main`  = MountSinai_cols("dark purple","dark pink","dark blue","dark grey"),
  
  `purple`  = MountSinai_cols("dark purple","med purple","light purple"),
  
  `pink`  = MountSinai_cols("dark pink","med pink","light pink"),
  
  `blue`  = MountSinai_cols("dark blue", "med blue", "light blue"),
  
  `grey`  = MountSinai_cols("dark grey", "med grey", "light grey"),
  
  `purpleGrey` = MountSinai_cols("dark purple", "dark grey"),
  
  `pinkBlue` = MountSinai_cols("dark pink", "dark blue")
  
)

# MountSinai_palettes
# Return function to interpolate a Mount Sinai color palette
# default value is the main palette, reverse = True will change the order

MountSinai_pal <- function(palette = "all", reverse = FALSE, ...) {
  pal <- MountSinai_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Scale Function for ggplot can be used instead of scale_color_manual
scale_color_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Scale Fill for ggplot instead of scale_fill_manual 
scale_fill_MountSinai <- function(palette = "all", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MountSinai_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MountSinai_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



