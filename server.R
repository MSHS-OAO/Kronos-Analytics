# Code for server functions

server <- function(input, output) {
  
  output$hosp_summary_table <- function() {
    
    site_input <<- input$hosp_summary_site
    
    date_input <<- as.Date(input$hosp_summary_date, format = "%m/%d/%y")
    
    hosp_summary_table <<- hosp_summary_df %>%
      filter(Site %in% site_input,
             Date %in% date_input,
             Time %in% "8AM") %>%
      select(-Site, -Time, -Capacity, -Date)
    
    kable(hosp_summary_table,
          escape = FALSE,
          align = "c") %>%
      kable_styling(bootstrap_options = "hover",
                    position = "center",
                    font_size = 11,
                    full_width = FALSE) %>%
      column_spec(column = c(1, 2, 5, 7, ncol(hosp_summary_table)),
                  border_right = "thin solid lightgray") %>%
      row_spec(row = 0,
               bold = TRUE,
               color = "white",
               background = MountSinai_colors["dark purple"])
    
    
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