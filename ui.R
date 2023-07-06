# Code for UI for shiny app

ui <- fluidPage(
  
  navbarPage(title = "MSHS Daily Productivity Tool",
             tabPanel(title = "Hospital Overview",
                      fluidRow(
                        column(4,
                               pickerInput(
                                 "hosp_summary_site",
                                 label = "Select Site",
                                 choices = site_choices$SITE,
                                 selected = default_site,
                                 multiple = FALSE
                               )
                        ),
                        column(4,
                               pickerInput(
                                 "hosp_summary_date",
                                 label = "Select Date",
                                 choices = format(date_choices$DATE, "%m/%d/%y"),
                                 selected = max(format(default_date, "%m/%d/%y")),
                                 multiple = FALSE
                               )
                        )
                      ),
                      tableOutput("hosp_summary_table")
             ),
             # tabPanel(title = "Unit Details - Retrospective",
             #          fluidRow(
             #            column(3,
             #                   pickerInput(
             #                     "unit_retro_site",
             #                     label = "Select Site",
             #                     choices = site_choices,
             #                     selected = default_site,
             #                     multiple = FALSE
             #                   )
             #            ),
             #            column(3,
             #                   pickerInput(
             #                     "unit_retro_dept",
             #                     label = "Select Department",
             #                     choices = dept_choices,
             #                     selected = default_dept,
             #                     multiple = FALSE
             #                   )
             #            ),
             #            column(3,
             #                   pickerInput(
             #                     "unit_retro_date",
             #                     label = "Select Date",
             #                     choices = format(date_choices, "%m/%d/%y"),
             #                     selected = format(default_date, "%m/%d/%y"),
             #                     multiple = FALSE
             #                   )
             #            )
             #          ),
             #          
             #          radioButtons("unit_retro_metric",
             #                      label = "Select Breakdown",
             #                      choices = c("By Skill", "By Paycode"),
             #                      selected = "By Skill",
             #                      inline = TRUE),
             #          tags$style("#unit_retro_metric {text-align: center;}"),
             #          fluidRow(
             #            column(8,
             #                   offset = 2,
             #                   plotOutput("retro_staff_vol_graph")
             #            )
             #          ),
             #          tableOutput("retro_staff_vol_table")
             #          # fluidRow(
             #          #   column(6,
             #          #          tableOutput("retro_staff_vol_tbl_skill")),
             #          #   column(6,
             #          #          tableOutput("test"))
             #          # )
             # ),
             tabPanel(title = "Unit Details - Scheduled"),
             tabPanel(title = "Employee Details")
  )
)
  