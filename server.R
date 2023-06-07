# Code for server functions

server <- function(input, output) {
  
  output$hosp_summary_table <- function() {
    
    site_input <<- input$hosp_summary_site
    
    date_input <<- as.Date(input$hosp_summary_date, format = "%m/%d/%y")
    
    hosp_summary_table <<- hosp_summary_df %>%
      filter(Site %in% site_input,
             Date %in% date_input,
             TimePull %in% 1) %>%
      select(-Site, -TimePull, -Capacity, -Date)
    
    kable(hosp_summary_table,
          escape = FALSE) %>%
      kable_styling(bootstrap_options = "hover")
    
  }
  
}