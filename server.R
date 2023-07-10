# Code for server functions

server <- function(input, output) {
  
  output$hosp_summary_table <- function() {
    
    # Connect to OAO Cloud Database to pull census data
    oao_personal_dsn <- "OAO Cloud DB Kate"
    census_conn <- dbConnect(odbc(),
                             oao_personal_dsn)
    
    census_tbl <- tbl(census_conn, "CENSUS_TEST")
    
    selected_site <- input$hosp_summary_site
    # selected_site <- "MOUNT SINAI QUEENS"
    
    census_df <- census_tbl %>%
      filter(SITE %in% selected_site) %>%
      collect() %>%
      rename(Site = SITE,
             Department = DEPARTMENT,
             Census = CENSUS) %>%
      mutate(NearestHour = round(REFRESH_TIME, units = "hours"),
             Date = date(NearestHour),
             Time = format(NearestHour, "%I:%M%p"),
             Site = selected_site) %>%
      group_by(Date) %>%
      filter(Time == "08:00PM") %>%
      ungroup()
    
    dbDisconnect(census_conn)

    selected_date <- as.Date(input$hosp_summary_date, format = "%m/%d/%y")
    # selected_date <- default_date
    
    census_filter <- census_df %>%
      filter(Site %in% selected_site,
             Date %in% selected_date) %>%
      # Time %in% "11PM") %>%
      select(-Site, -REFRESH_TIME, -NearestHour, -Date, -Time)
    
    labor_filter <- labor_df %>%
      filter(Site %in% selected_site,
             Date %in% selected_date,
             Time %in% "08:00PM")
    
    labor_template <- expand_grid(
      "Department" = unique(labor_filter$Department),
      "Description" = unique(paycode_mappings$Description)
    )
    
    labor_template <- left_join(labor_template,
                                paycode_mappings %>%
                                  select(Description,
                                         InclWorkedHours,
                                         InclNonProdHours),
                                by = c("Description" = "Description"))
    
    labor_filter <- left_join(labor_filter, paycode_mappings,
                               by = c("PAYCODE_CATEGORY" = "Paycode"))
    
    if (nrow(census_filter) > 0 & nrow(labor_filter) > 0) {
      
      labor_paid_hrs <- labor_filter %>%
        filter(InclPaidHours) %>%
        group_by(Department) %>%
        summarize(TotalPaidHrs = sum(Hours, na.rm = TRUE))
      
      labor_worked_hrs <- labor_filter %>%
        filter(InclWorkedHours) %>%
        group_by(Department, Description) %>%
        summarize(TotalWorkedHrs = sum(Hours, na.rm = TRUE)) %>%
        full_join(labor_template %>%
                    filter(InclWorkedHours)) %>%
        select(-InclWorkedHours, -InclNonProdHours) %>%
        mutate(TotalWorkedHrs = replace_na(TotalWorkedHrs, 0)) %>%
        pivot_wider(names_from = Description,
                    values_from = TotalWorkedHrs) %>%
        adorn_totals(where = "col", name = "WorkedHrsTotal") %>%
        relocate(WorkedHrsTotal, .after = Department)
      
      labor_nonproductive_hrs <- labor_filter %>%
        filter(InclNonProdHours) %>%
        group_by(Department, Description) %>%
        summarize(TotalNonProdHrs = sum(Hours, na.rm = TRUE)) %>%
        full_join(labor_template %>%
                    filter(InclNonProdHours)) %>%
        select(-InclWorkedHours, -InclNonProdHours) %>%
        mutate(TotalNonProdHrs = replace_na(TotalNonProdHrs, 0)) %>%
        pivot_wider(names_from = Description,
                    values_from = TotalNonProdHrs) %>%
        adorn_totals(where = "col", name = "NonProdHrsTotal") %>%
        relocate(NonProdHrsTotal, .after = Department)
      
      hosp_summary_table <- left_join(census_filter,
                                      left_join(labor_paid_hrs,
                                                left_join(labor_worked_hrs,
                                                          labor_nonproductive_hrs
                                                )
                                      )
      )
      
      hosp_summary_table <- hosp_summary_table %>%
        mutate(TotalPaidFTE = NA,
               WorkedFTE = NA,
               WHpU = NA) %>%
        relocate(TotalPaidFTE, .after = TotalPaidHrs) %>%
        relocate(c(WorkedFTE, WHpU), .after = WorkedHrsTotal) %>%
        relocate(NonProdHrsTotal, .after = WHpU)
      
      kable(hosp_summary_table,
            escape = FALSE,
            align = "c") %>%
        kable_styling(bootstrap_options = "hover",
                      position = "center",
                      font_size = 11,
                      full_width = FALSE) %>%
        column_spec(column = c(1, 2, 4, 7, 8, 8 + ncol(labor_worked_hrs) - 2,
                               ncol(hosp_summary_table)),
                    border_right = "thin solid lightgray") %>%
        row_spec(row = 0,
                 bold = TRUE,
                 color = "white",
                 background = MountSinai_colors["dark purple"]) %>%
        add_header_above(c(" " = 2,
                           "Paid" = 2,
                           "Worked Hours" = 3,
                           "NonProductive Hours" = 1,
                           "Worked Hours Breakdown" = ncol(labor_worked_hrs) - 2,
                           "NonProductive Hours Breakdown" = ncol(labor_nonproductive_hrs) - 2),
                         escape = FALSE)
    } else {
        
      print("No relevant data.")
      
      }
    
    
  }
  
  output$unit_retro_radio_button <- renderText("Select Breakdown:")

  # Graph for staffing to volume by skill -------------
  output$retro_staff_vol_graph <- renderPlot({

    site_input <- input$unit_retro_site

    dept_input <- input$unit_retro_dept

    date_input <- as.Date(input$unit_retro_date, format = "%m/%d/%y")

    strat <- input$unit_retro_metric

    labor_df_filter <- labor_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input)

    if (strat == "By Skill") {
      labor_df_filter <- labor_df_filter %>%
        group_by(Time, JOB_CATEGORY) %>%
        summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
        rename(Stratification = JOB_CATEGORY)
    } else{
      labor_df_filter <- left_join(labor_df_filter,
                                   paycode_mappings %>%
                                     select(Paycode, Description),
                                   by = c("PAYCODE_CATEGORY" = "Paycode"))

      labor_df_filter <- labor_df_filter %>%
        group_by(Time, Description) %>%
        summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
        rename(Stratification = Description)
    }

    labor_df_filter <- labor_df_filter %>%
      mutate(Time = factor(Time, levels = data_times, ordered = TRUE)) %>%
      arrange(Time, Stratification)

    vol_df_filter <- census_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      select(Time, Census, Capacity)

    scale_factor <- unique(vol_df_filter$Capacity) /
      max(labor_df_filter %>%
            group_by(Time) %>%
            summarize(TotalHours = sum(TotalHours, na.rm = TRUE)) %>%
            select(TotalHours))

    ggplot() +
      geom_col(data = labor_df_filter,
               mapping = aes(x = Time,
                             y = TotalHours,
                             color = Stratification,
                             fill = Stratification,
                             group = Stratification),
               width = 0.5) +
      geom_text(data = labor_df_filter,
                aes(x = Time,
                    y = TotalHours,
                    label = TotalHours,
                    group = Stratification),
                position = position_stack(vjust = 0.5),
        color = "white") +
      scale_color_MountSinai(palette = "main",
                             name = ifelse(input$unit_retro_metric == "By Skill",
                                           "Job Category",
                                           "Paycode")) +
      scale_fill_MountSinai(palette = "main",
                            name = ifelse(input$unit_retro_metric == "By Skill",
                                          "Job Category",
                                          "Paycode")) +
      geom_line(data = vol_df_filter,
                mapping = aes(x = Time,
                              y = Census / scale_factor,
                              group = 1)) +
      geom_point(data = vol_df_filter,
                 mapping = aes(x = Time,
                               y = Census / scale_factor)) +
      geom_text(data = vol_df_filter,
                aes(x = Time,
                    y = Census / scale_factor,
                    label = Census),
                color = "black",
                hjust = -0.5,
                vjust = 1.5) +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ . * scale_factor,
                            name = "Census")
      ) +
      labs(title = paste0("Staff to Volume ",
                          input$unit_retro_metric,
                          " ",
                          format(date_input, "%m/%d/%y")),
           x = "Time",
           y = "Total Hours") +
      theme(legend.position="bottom")

  })

  # Table for staffing to volume ---------------
  output$retro_staff_vol_table <- function() {

    site_input <- input$unit_retro_site

    dept_input <- input$unit_retro_dept

    date_input <- as.Date(input$unit_retro_date, format = "%m/%d/%y")

    strat <- input$unit_retro_metric

    # site_input <- "MSQ"
    # dept_input <- "MSQ 2 EAST"
    # date_input <- as.Date("5/10/23", format = "%m/%d/%y")
    # strat <- "By Paycode"

    vol_df_filter <- census_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      mutate(Time = factor(Time, levels = data_times, ordered = TRUE)) %>%
      arrange(Time) %>%
      pivot_wider(names_from = Time,
                  values_from = Census) %>%
      select(-Site) %>%
      mutate(Metric = "Census",
             Stratification = paste0("Dept Capacity: ", Capacity)) %>%
      relocate(Metric) %>%
      relocate(Stratification, .after = Metric) %>%
      select(-Department, -Date, -Capacity)

    labor_df_filter <- labor_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input)

    if (strat == "By Skill") {
      labor_df_filter <- labor_df_filter %>%
        group_by(Time, JOB_CATEGORY) %>%
        summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
        rename(Stratification = JOB_CATEGORY)
    } else{
      labor_df_filter <- left_join(labor_df_filter,
                                   paycode_mappings %>%
                                     select(Paycode, Description),
                                   by = c("PAYCODE_CATEGORY" = "Paycode"))

      labor_df_filter <- labor_df_filter %>%
        group_by(Time, Description) %>%
        summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
        rename(Stratification = Description)
    }

    labor_df_filter <- labor_df_filter %>%
      mutate(Time = factor(Time, levels = data_times, ordered = TRUE)) %>%
      arrange(Time, Stratification) %>%
      pivot_wider(names_from = Time,
                  values_from = TotalHours) %>%
      adorn_totals(where = "row",
                   name = "Total") %>%
      mutate(Metric = "Paid Hours") %>%
      relocate(Metric)

    vol_labor_df <- rbind(vol_df_filter, labor_df_filter)

    kable(vol_labor_df,
          escape = FALSE,
          align = "c",
          col.names = c(" ", " ", "8AM", "4PM", "8PM", "11PM")) %>%
      kable_styling(bootstrap_options = "hover",
                    position = "center",
                    font_size = 11,
                    full_width = FALSE) %>%
      column_spec(column = c(1, 2, ncol(vol_labor_df)),
                  border_right = "thin solid lightgray") %>%
      column_spec(column = 1,
                  border_left = "thin solid lightgray") %>%
      column_spec(column = 1,
                  bold = TRUE,
                  italic = TRUE,
                  background = MountSinai_colors["med grey"],
                  color = "white") %>%
      row_spec(row = 0,
               background = MountSinai_colors["dark purple"],
               color = "white") %>%
      row_spec(row = nrow(vol_labor_df),
               bold = TRUE,
               italic = TRUE) %>%
      collapse_rows(columns = 1, valign = "top")

  }
  
}