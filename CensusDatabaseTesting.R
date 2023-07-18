# Test connection to Oracle database


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

print("Start")

# Connect to Clarity table and pull in latest census data
epic_dsn <- "Clarity_prod_kate"
epic_conn <- dbConnect(odbc(), epic_dsn)

print("Connected to Epic Clarity database")

# Pull in data from census table
census_refresh <- tbl(epic_conn,
                      in_schema("CRREPORT_REP", "MSHS_CENSUS_TEMP"))

census_summary <- census_refresh %>%
  distinct() %>%
  filter(ADMIT_STATUS %in% "Admission" &
           !is.na(HOSPITAL_AREA) &
           !is.na(DEPARTMENT)) %>%
  mutate(MAX_TIME = max(CURRENT_TIME)) %>%
  group_by(HOSPITAL_AREA, DEPARTMENT, CURRENT_DATE, MAX_TIME) %>%
  summarize(CENSUS = n()) %>%
  ungroup() %>%
  rename(SITE = HOSPITAL_AREA) %>%
  mutate(REFRESH_TIME = to_timestamp(
    paste0(CURRENT_DATE, " ", MAX_TIME),
    'MM/DD/YYYY HH:MI AM')) %>%
  select(-CURRENT_DATE, -MAX_TIME) %>%
  arrange(SITE, DEPARTMENT, REFRESH_TIME) %>%
  collect()

dbDisconnect(epic_conn)

print("Disconnected from Epic Clarity database")

saveRDS(census_summary,
        file = paste0(root_path,
                      "HSPI-PM/Operations Analytics and Optimization/Projects/",
                      "System Operations/Kronos Analytics/Data/Epic Clarity Census/",
                      "EpicClarityCensusPull_",
                      format(max(unique(census_summary$REFRESH_TIME)), "%Y-%m-%d %H%M"),
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

write_temp_census_table_to_db_and_merge <- function(processed_input_data,table_name){
  if(nrow(processed_input_data) == 0) {
    print("no new data")
  } else{
    
    # Constants
    DATA_TYPES <- c(SITE = "Varchar2(50 CHAR)",
                    DEPARTMENT = "Varchar2(50 CHAR)",
                    CENSUS = "Number(38,0)",
                    REFRESH_TIME = "Timestamp")
    
    TABLE_NAME <- paste0("CENSUS_LAST_REFRESH")
    
    
    # Ensure all the fields are correct data type
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
    
    values <- glue_collapse(inserts,sep = "\n\n")
    
    # Combine into statements from get_values() function and combine with
    # insert statements
    all_data <- glue('INSERT ALL
                        {values}
                      SELECT 1 from DUAL;')
    
    # glue() query to merge data from temporary table to summary_repo table
    query = glue('MERGE INTO MSHS_CENSUS_REPO CR
                    USING "{TABLE_NAME}" SOURCE_TABLE
                    ON (  CR."SITE" = SOURCE_TABLE."SITE" AND
                          CR."DEPARTMENT" = SOURCE_TABLE."DEPARTMENT" AND
                          CR."REFRESH_TIME" = SOURCE_TABLE."REFRESH_TIME")
                    WHEN MATCHED THEN 
                    UPDATE  SET CR."CENSUS" = SOURCE_TABLE."CENSUS"
                    WHEN NOT MATCHED THEN
                    INSERT( CR."SITE",
                            CR."DEPARTMENT",
                            CR."CENSUS",
                            CR."REFRESH_TIME"
                            )  
                    VALUES( SOURCE_TABLE."SITE",
                            SOURCE_TABLE."DEPARTMENT",
                            SOURCE_TABLE."CENSUS",
                            SOURCE_TABLE."REFRESH_TIME");')
    
    # glue query for dropping the table
    truncate_query <- glue('TRUNCATE TABLE "{TABLE_NAME}";')
    
    print("Before OAO Cloud DB Connection")
    # conn <- dbConnect(drv = odbc::odbc(),  ## Create connection for updating picker choices
    #                   dsn = dsn)
    
    oao_personal_dsn <- "OAO Cloud DB Kate"
    
    oao_personal_conn <- dbConnect(odbc(),
                                   oao_personal_dsn)
    
    dbBegin(oao_personal_conn)
    # ## Execute staments and if there is an error  with one of them rollback changes
    tryCatch({
      print("Before first truncate")
      dbExecute(oao_personal_conn, truncate_query)
      print("After first truncate")
      dbExecute(oao_personal_conn, all_data)
      print("After all data glue statement")
      dbExecute(oao_personal_conn, query)
      print("After merge")
      dbExecute(oao_personal_conn, truncate_query)
      print("After second truncate")
      dbCommit(oao_personal_conn)
      dbDisconnect(oao_personal_conn)
      print("Success!")
    },
    error = function(err){
      #print(err)
      dbRollback(oao_personal_conn)
      dbDisconnect(oao_personal_conn)
      dbExecute(oao_personal_conn, truncate_query)
      print("Error")
    })
  }
}

write_temp_census_table_to_db_and_merge(processed_input_data = census_summary,
                                        table_name = "CENSUS_LAST_REFRESH")
