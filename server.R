# Code for server functions

server <- function(input, output) {
  
  output$hosp_summary_table <- function() {
    
    site_input <<- input$hosp_summary_site
    
    date_input <<- as.Date(input$hosp_summary_date, format = "%m/%d/%y")
    
    labor_filter <- labor_df %>%
      filter(Site %in% site_input,
             Date %in% date_input,
             Time %in% "11PM")
    
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
    

    census_filter <- census_df %>%
      filter(Site %in% site_input,
             Date %in% date_input,
             Time %in% "11PM") %>%
      select(-Site, -Time, -Date, -Capacity)
    
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
    
    
    # kable_styling(bootstrap_options = "hover", position = "center",
    #               font_size = 11,
    #               full_width = FALSE) %>%
    #   column_spec(column = c(1, (num_col - 1) / 2 + 1, num_col),
    #               border_right = "thin solid lightgray") %>%
    #   add_header_above(c(" " = 1,
    #                      "Receive to Result Within Target" =
    #                        (num_col - 1) / 2,
    #                      "Collect to Result Within Target" =
    #                        (num_col - 1) / 2),
    #                    background = c("white", "#00AEEF", "#221f72"),
    #                    color = "white", line = FALSE, font_size = 13) %>%
    #   column_spec(column = 2:((num_col - 1) / 2 + 1), background = "#E6F8FF", color = "black") %>%
    #   column_spec(column = ((num_col - 1) / 2 + 2):num_col, background = "#EBEBF9", color = "black") %>%
    #   #column_spec(column = 2:17, background = "inherit", color = "inherit") %>%
    #   column_spec(column = 1, width_min = "125px") %>%
    #   column_spec(column = c(3, (num_col - 1) / 2 + 3), width_min = "100px") %>%
    #   row_spec(row = 0, font_size = 13) %>%
    #   collapse_rows(columns = c(1, 2, ((num_col - 1) / 2 + 2)))
    
  }
  
}