# Code for UI for shiny app

ui <- fluidPage(
  
  navbarPage(title = "MSHS Daily Productivity Tool",
             tabPanel(title = "Hospital Overview",
                      fluidRow(
                        column(4,
                               pickerInput(
                                 "hosp_summary_site",
                                 label = "Select Site",
                                 choices = sites,
                                 selected = sites[1],
                                 multiple = TRUE
                               )
                        ),
                        column(4,
                               pickerInput(
                                 "hosp_summary_date",
                                 label = "Select Date",
                                 choices = format(dates, "%m/%d/%y"),
                                 selected = format(dates[1], "%m/%d/%y"),
                                 multiple = FALSE
                               )
                        )
                      ),
                      tableOutput("hosp_summary_table")
             ),
             tabPanel(title = "Unit Details - Retrospective",
                      fluidRow(
                        column(3,
                               pickerInput(
                                 "unit_retro_site",
                                 label = "Select Site",
                                 choices = sites,
                                 selected = sites[1],
                                 multiple = FALSE
                               )
                        ),
                        column(3,
                               pickerInput(
                                 "unit_retro_dept",
                                 label = "Select Department",
                                 choices = departments,
                                 selected = departments[1],
                                 multiple = FALSE
                               )
                        ),
                        column(3,
                               pickerInput(
                                 "unit_retro_date",
                                 label = "Select Date",
                                 choices = format(dates, "%m/%d/%y"),
                                 selected = format(dates[1], "%m/%d/%y"),
                                 multiple = FALSE
                               )
                        )#,
                        # column(3,
                        #        pickerInput(
                        #          "unit_retro_time",
                        #          label = "Select Time",
                        #          choices = data_times,
                        #          selected = data_times[1],
                        #          multiple = FALSE
                        #        )
                        # )
                      ),
                      fluidRow(
                        column(5,
                               plotOutput("retro_staff_vol_graph_skill")),
                        column(5,
                               plotOutput("retro_staff_vol_graph_paycode"))
                        
                      )#,
                      # fluidRow(
                      #   column(6,
                      #          tableOutput("retro_staff_vol_tbl_skill")),
                      #   column(6,
                      #          tableOutput("test"))
                      # )
             ),
             tabPanel(title = "Unit Details - Scheduled"),
             tabPanel(title = "Employee Details")
  )
)
  