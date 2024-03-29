# setup.R
library(purrr)

# Get and Transform Data
data <- read_csv("data/data.csv") %>% 
  mutate(Date = ymd(Date))

kpis <- c('% Within 6 Hours', '% Within 9 Hours', '% Within 24 Hours')

sidebar_content <-
  list(
    selectInput("hospitals",
                "Select Hospital",
                choices = unique(data$Hospital) %>% discard(~ .x == 'National'),
                selected = "",
                multiple  = FALSE),
    selectInput("kpis",
                "Select KPI",
                choices = kpis,
                selected = "",
                multiple  = FALSE),

    "This Web-App uses publicly available data published by the HSE each month.
    The Monthly Data Managment Report is available at", 
    tags$a("Link to HSE Performance Reports", href = "https://www.hse.ie/eng/services/publications/performancereports/") ,
    "HSE MDR Data is a moment in time snapshot of a hospitals performance. Data Validation is always being performed by HSE hospitals, and as such, this data may change over time"
  )