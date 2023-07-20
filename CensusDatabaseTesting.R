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

# saveRDS(census_summary,
#         file = paste0(root_path,
#                       "HSPI-PM/Operations Analytics and Optimization/Projects/",
#                       "System Operations/Kronos Analytics/Data/Epic Clarity Census/",
#                       "EpicClarityCensusPull_",
#                       format(max(unique(census_summary$REFRESH_TIME)), "%Y-%m-%d %H%M"),
#                       ".RDS"))

get_values <- function(x, table_name){
  
  site <- x[1]
  dept <- x[2]
  census <- x[3]
  refresh_time <- x[4]
  
  values <- glue("INTO \"{table_name}\" (SITE,DEPARTMENT,CENSUS,REFRESH_TIME) 
                 VALUES ('{site}','{dept}','{census}',TO_TIMESTAMP('{refresh_time}','YYYY-MM-DD HH24:MI:SS'))")
  
  return(values)
}

write_temp_census_table_to_db_and_merge <- function(processed_input_data,TABLE_NAME){
  if(nrow(processed_input_data) == 0) {
    print("no new data")
  } else{
    
    # # Constants
    # DATA_TYPES <- c(SITE = "Varchar2(50 CHAR)",
    #                 DEPARTMENT = "Varchar2(50 CHAR)",
    #                 CENSUS = "Number(38,0)",
    #                 REFRESH_TIME = "Timestamp")
    # 
    # TABLE_NAME <- paste0("CENSUS_LAST_REFRESH")
    
    
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
    
    oao_personal_dsn <- "OAO Cloud DB Greg"
    
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
      dbRollback(oao_personal_conn)
      dbExecute(oao_personal_conn, truncate_query)
      dbCommit(oao_personal_conn)
      dbDisconnect(oao_personal_conn)
      print("Error")
    })
  }
}

write_temp_census_table_to_db_and_merge(processed_input_data = census_summary,
                                        TABLE_NAME = "CENSUS_LAST_REFRESH")

# ---------------------------------------------------------------------------
oao_personal_dsn <- "OAO Cloud DB Greg"

oao_personal_conn <- dbConnect(odbc(),
                               oao_personal_dsn)

# get refresh hour to check if it is one of four refresh numbers
refresh_hour <- unique(as.character(hour(census_summary$REFRESH_TIME) + 1))
if (nchar(refresh_hour) == 1) {
  refresh_hour <- paste0("0", refresh_hour)
}
data_refresh <- tbl(oao_personal_conn, "DATA_REFRESH") %>%
  filter(REFRESH_NUMBER %in% c("1", "2", "3", "4")) %>%
  collect()
if (refresh_hour %in% data_refresh$REFRESH_HOUR) {
  target_update <- "yes"
} else {
  target_update <- "no"
}


# if this run is a refresh number then we will merge to the target table
if (target_update == "yes") {
  
  # get current max refresh time in the target table
  max_target_refresh <- max(tbl(oao_personal_conn, "TARGET") %>%
                              select(REFRESH_TIME) %>%
                              collect() %>%
                              pull())
      
  # function to convert each row of target query to an insert clause
  get_values_target <- function(x, table_name){
    
    site <- x[1]
    dept <- x[2]
    refresh_time <- x[3]
    refresh_hour <- x[4]
    refresh_number <- x[5]
    dow <- x[6]
    census <- x[7]
    patient_charge <- x[8]
    patient_rn <- x[9]
    patient_na <- x[10]
    charge_rn <- x[11]
    rn <- x[12]
    na <- x[13]
    manager <- x[14]
    assistant_manager <- x[15]
    secretary <- x[16]
    administrator <- x[17]
    
    values <- glue("INTO \"{table_name}\" (SITE,DEPARTMENT,REFRESH_TIME, 
                                           REFRESH_HOUR, REFRESH_NUMBER, DOW, 
                                           CENSUS, PATIENT_CHARGE, PATIENT_RN, 
                                           PATIENT_NA, CHARGE_RN, RN, NA, 
                                           MANAGER, ASSISTANT_MANAGER, 
                                           SECRETARY, ADMINISTRATOR) 
                 VALUES ('{site}','{dept}',
                         TO_TIMESTAMP('{refresh_time}','YYYY-MM-DD HH24:MI:SS'),
                         '{refresh_hour}', '{refresh_number}', '{dow}',
                         '{census}', '{patient_charge}', '{patient_rn}',
                         '{patient_na}', '{charge_rn}', '{rn}', '{na}',
                         '{manager}', '{assistant_manager}', '{secretary}',
                         '{administrator}')")
    
    return(values)
  }
  
  # target update query
  target_update_query <- glue(
    "select CT.SITE,
       CT.DEPARTMENT,
       CT.REFRESH_TIME,
       EXTRACT(HOUR from CT.REFRESH_TIME) + 1 as REFRESH_HOUR,
       DR.REFRESH_NUMBER,
       TRIM(TO_CHAR(CT.REFRESH_TIME, 'DAY')) as DOW,
       CT.CENSUS,
       R.PATIENT_CHARGE,
       R.PATIENT_RN,
       R.PATIENT_NA,
       1 as CHARGE_RN,
       CEIL((CT.CENSUS - R.PATIENT_CHARGE) / R.PATIENT_RN) as RN,
       CEIL((CT.CENSUS) / R.PATIENT_NA) as NA,
       M.MANAGER,
       M.ASSISTANT_MANAGER,
       M.SECRETARY,
       M.ADMINISTRATOR
  from MSHS_CENSUS_REPO CT
  left join DATA_REFRESH DR
    on EXTRACT(HOUR from CT.REFRESH_TIME) + 1 = DR.REFRESH_HOUR
  left join RATIOS R
    on CT.SITE = R.SITE AND
       CT.DEPARTMENT = R.DEPARTMENT
  left join MANAGEMENT M
    on CT.SITE = M.SITE AND
       CT.DEPARTMENT = M.DEPARTMENT AND
       TRIM(TO_CHAR(CT.REFRESH_TIME, 'DAY')) = M.DOW AND
       DR.REFRESH_NUMBER = M.REFRESH_NUMBER
  where CT.REFRESH_TIME >= TIMESTAMP '{max_target_refresh}' AND
        DR.REFRESH_NUMBER <> 'NA' AND
        CT.SITE = 'MOUNT SINAI QUEENS';"
  )
  
  # convert target query to proper data format for insert to TARGET_TEMP
  target <- dbFetch(dbSendQuery(oao_personal_conn, target_update_query)) %>%
    mutate(SITE = as.character(SITE),
           DEPARTMENT = as.character(DEPARTMENT),
           REFRESH_TIME = format(REFRESH_TIME, "%Y-%m-%d %H:%M"),
           REFRESH_HOUR = as.character(REFRESH_HOUR),
           REFRESH_NUMBER = as.character(REFRESH_NUMBER),
           DOW = as.character(DOW),
           CENSUS = as.integer(CENSUS),
           PATIENT_CHARGE = as.integer(PATIENT_CHARGE),
           PATIENT_RN = as.integer(PATIENT_RN),
           PATIENT_NA = as.integer(PATIENT_NA),
           CHARGE_RN = as.integer(CHARGE_RN),
           RN = as.integer(RN),
           `NA` = as.integer(`NA`),
           MANAGER = as.integer(MANAGER),
           ASSISTANT_MANAGER = as.integer(ASSISTANT_MANAGER),
           SECRETARY = as.integer(SECRETARY),
           ADMINISTRATOR = as.integer(ADMINISTRATOR))
  
  # temp and live target tables
  TABLE_NAME_TARGET_TEMP <- paste0("TARGET_TEMP")
  TABLE_NAME_TARGET <- paste0("TARGET")
  
  # Convert the each record/row of tibble to INTO clause of insert statment
  inserts_target <- lapply(
    lapply(
      lapply(split(target , 
                   1:nrow(target)),
             as.list), 
      as.character),
    FUN = get_values_target, TABLE_NAME_TARGET_TEMP)
  
  values_target <- glue_collapse(inserts_target, sep = "\n\n")
  
  # Combine into statements from get_values() function and combine with
  # insert statements
  all_data_target <- glue('INSERT ALL
                              {values_target}
                          SELECT 1 from DUAL;')
  
  # merge query for merging TARGET_TEMP into TARGET
  merge_target <- glue(
    "MERGE INTO \"{TABLE_NAME_TARGET}\" TARGET
     USING \"{TABLE_NAME_TARGET_TEMP}\" TEMP
     ON ( TARGET.SITE = TEMP.SITE AND
          TARGET.DEPARTMENT = TEMP.DEPARTMENT AND
          TARGET.REFRESH_TIME = TEMP.REFRESH_TIME )
     WHEN MATCHED THEN 
     UPDATE SET TARGET.CENSUS = TEMP.CENSUS,
                TARGET.PATIENT_CHARGE = TEMP.PATIENT_CHARGE,
                TARGET.PATIENT_RN = TEMP.PATIENT_RN,
                TARGET.PATIENT_NA = TEMP.PATIENT_NA,
                TARGET.CHARGE_RN = TEMP.CHARGE_RN,
                TARGET.RN = TEMP.RN,
                TARGET.NA = TEMP.NA,
                TARGET.MANAGER = TEMP.MANAGER,
                TARGET.ASSISTANT_MANAGER = TEMP.ASSISTANT_MANAGER,
                TARGET.SECRETARY = TEMP.SECRETARY,
                TARGET.ADMINISTRATOR = TEMP.ADMINISTRATOR
    WHEN NOT MATCHED THEN
    INSERT( TARGET.SITE,
            TARGET.DEPARTMENT,
            TARGET.REFRESH_TIME,
            TARGET.REFRESH_HOUR,
            TARGET.REFRESH_NUMBER,
            TARGET.DOW,
            TARGET.CENSUS,
            TARGET.PATIENT_CHARGE,
            TARGET.PATIENT_RN,
            TARGET.PATIENT_NA,
            TARGET.CHARGE_RN,
            TARGET.RN,
            TARGET.NA,
            TARGET.MANAGER,
            TARGET.ASSISTANT_MANAGER,
            TARGET.SECRETARY,
            TARGET.ADMINISTRATOR )
    VALUES( TEMP.SITE,
            TEMP.DEPARTMENT,
            TEMP.REFRESH_TIME,
            TEMP.REFRESH_HOUR,
            TEMP.REFRESH_NUMBER,
            TEMP.DOW,
            TEMP.CENSUS,
            TEMP.PATIENT_CHARGE,
            TEMP.PATIENT_RN,
            TEMP.PATIENT_NA,
            TEMP.CHARGE_RN,
            TEMP.RN,
            TEMP.NA,
            TEMP.MANAGER,
            TEMP.ASSISTANT_MANAGER,
            TEMP.SECRETARY,
            TEMP.ADMINISTRATOR )")
  
  truncate_target <- glue(
    'TRUNCATE TABLE \"{TABLE_NAME_TARGET_TEMP}\"'
  )
  
  dbBegin(oao_personal_conn)
  # ## Execute staments and if there is an error  with one of them rollback changes
  tryCatch({
    print("Before first truncate")
    dbExecute(oao_personal_conn, truncate_target)
    print("After first truncate")
    dbExecute(oao_personal_conn, all_data_target)
    print("After all data glue statement")
    dbExecute(oao_personal_conn, merge_target)
    print("After merge")
    dbExecute(oao_personal_conn, truncate_target)
    print("After second truncate")
    dbCommit(oao_personal_conn)
    dbDisconnect(oao_personal_conn)
    print("Success!")
  },
  error = function(err){
    dbRollback(oao_personal_conn)
    dbExecute(oao_personal_conn, truncate_query)
    dbCommit(oao_personal_conn)
    dbDisconnect(oao_personal_conn)
    print("Error")
  })
} else {
  print("TARGET table received no updates")
}
