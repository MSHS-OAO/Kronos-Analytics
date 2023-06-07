# Code for UI for shiny app

ui <- fluidPage(
  
  navbarPage(title = "Daily Productivity Tool",
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
                        ),
                        tableOutput("hosp_summary_table")
                      )
             ),
             tabPanel(title = "Unit Details - Retrospective"),
             tabPanel(title = "Unit Details - Scheduled"),
             tabPanel(title = "Employee Details")
  )
  
)