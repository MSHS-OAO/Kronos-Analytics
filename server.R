# Code for server functions

server <- function(input, output) {
  
  output$hosp_summary_table <- function() {
    
    site_input <- input$hosp_summary_site
    
    date_input <- as.Date(input$hosp_summary_date, format = "%m/%d/%y")
    
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
  }
  
  output$retro_staff_vol_graph_skill <- renderPlot({
    
    site_input <- input$unit_retro_site
    
    dept_input <- input$unit_retro_dept
    
    date_input <- as.Date(input$unit_retro_date, format = "%m/%d/%y")

    labor_df_skill <- labor_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      group_by(Time, JOB_CATEGORY) %>%
      summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
      mutate(Time = factor(Time, levels = data_times, ordered = TRUE)) %>%
      arrange(Time, JOB_CATEGORY)
    
    vol_df <- census_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      select(Time, Census, Capacity)
    
    scale <- unique(vol_df$Capacity) /
      max(labor_df_skill %>%
            group_by(Time) %>%
            summarize(TotalHours = sum(TotalHours, na.rm = TRUE)) %>%
            select(TotalHours))
      
    # ggplot() +
    ggplot(data = labor_df_skill,
           mapping = aes(x = Time, y = TotalHours)) +
      geom_col(data = labor_df_skill,
               mapping = aes(x = Time,
                             y = TotalHours,
                             color = JOB_CATEGORY,
                             fill = JOB_CATEGORY,
                             group = JOB_CATEGORY),
               width = 0.5) +
      geom_text(#data = labor_df_skill,
                aes(#x = Time,
                    y = TotalHours,
                    label = TotalHours,
                    group = JOB_CATEGORY),
                position = position_stack(vjust = 0.5),
                color = "white") +
      scale_color_MountSinai(palette = "main",
                             name = "Job Category") +
      scale_fill_MountSinai(palette = "main",
                            name = "Job Category") +
      geom_line(data = vol_df,
                mapping = aes(x = Time,
                              y = Census / scale,
                              group = 1)) +
      geom_point(data = vol_df,
                 mapping = aes(x = Time,
                               y = Census / scale)) +
      geom_text(data = vol_df,
                aes(y = Census / scale,
                    label = Census),
                color = "black",
                hjust = -0.5,
                vjust = 1.5) +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ . * scale,
                            name = "Census")
      ) +
      labs(title = paste0("Staff to Volume by Skill Mix on ",
                          format(date_input, "%m/%d/%y")),
           x = "Time",
           y = "Total Hours") +
      theme(legend.position="bottom")
    
  })
  
  output$retro_staff_vol_graph_paycode <- renderPlot({

    site_input <<- input$unit_retro_site

    dept_input <<- input$unit_retro_dept

    date_input <<- as.Date(input$unit_retro_date, format = "%m/%d/%y")

    labor_df_paycode <- left_join(labor_df,
                                  paycode_mappings %>%
                                    select(Paycode, Description),
                                  by = c("PAYCODE_CATEGORY" = "Paycode"))
    
    labor_df_paycode <<- labor_df_paycode %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      group_by(Time, Description) %>%
      summarize(TotalHours = sum(Hours, na.rm = TRUE)) %>%
      mutate(Time = factor(Time, levels = data_times, ordered = TRUE)) %>%
      arrange(Time, Description)

    vol_df <<- census_df %>%
      filter(Site %in% site_input,
             Department %in% dept_input,
             Date %in% date_input) %>%
      select(Time, Census, Capacity)

    scale <- unique(vol_df$Capacity) /
      max(labor_df_paycode %>%
            group_by(Time) %>%
            summarize(TotalHours = sum(TotalHours, na.rm = TRUE)) %>%
            select(TotalHours))

    ggplot() +
    # ggplot(data = labor_df_paycode,
    #        mapping = aes(x = Time, y = TotalHours)) +
      geom_col(data = labor_df_paycode,
               mapping = aes(x = Time,
                             y = TotalHours,
                             color = Description,
                             fill = Description,
                             group = Description),
               width = 0.5) #+
      geom_text(#data = labor_df_skill,
        aes(#x = Time,
          y = TotalHours,
          label = TotalHours,
          group = Description),
        position = position_stack(vjust = 0.5),
        color = "white") +
      scale_color_MountSinai(palette = "main",
                             name = "Paycode Category") +
      scale_fill_MountSinai(palette = "main",
                            name = "Paycode Category") +
      geom_line(data = vol_df,
                mapping = aes(x = Time,
                              y = Census / scale,
                              group = 1)) +
      geom_point(data = vol_df,
                 mapping = aes(x = Time,
                               y = Census / scale)) +
      geom_text(data = vol_df,
                aes(y = Census / scale,
                    label = Census),
                color = "black",
                hjust = -0.5,
                vjust = 1.5) +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ . * scale,
                            name = "Census")
      ) +
      labs(title = paste0("Staff to Volume by Paycode Category on ",
                          format(date_input, "%m/%d/%y")),
           x = "Time",
           y = "Total Hours") +
      theme(legend.position="bottom")

  })
  
}