library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(thematic)
library(plotly)

source("setup.R")
source("helper.R")

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())


# Apply CSS to ggplots
thematic_shiny()


# Define UI for application that draws a histogram
ui <- page_sidebar(
  
  # Set CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   version = 5,
                   success = "#4D9086"),
  
  title = 'Free-PVP: Health Service Executive Key Performance Indicator Visualization',
  sidebar = sidebar(
    sidebar_content,
    #HTML('<img src = "HSE Logo White PNG.png", width = "100%", height = "auto">')
    
  ),
  
  layout_columns(
    card(card_header("% of Attendees Admitted/Discharged within the selected KPI Time Period. (All Ages)"),
         plotOutput("line_All")),

    card(card_header("% of Attendees Admitted/Discharged within the selected KPI Time Period. (75 and Older)"),
         plotOutput("line_75")),
    
    value_box(title = "Average Monthly Number of Attendees Who Leave before decision to admit/discharge",
              textOutput('avg_dna'),
              showcase = bs_icon("person-walking"),
              theme = "success"),
    
    value_box(title = "Average Monthly Number of Attendees Who Leave before decision to admit/discharge. (Nationally)",
              textOutput('avg_nat_dna'),
              showcase = bs_icon("globe"),
              theme = "success"),
    
    value_box(title = "Average Monthly # of Attendees waiting Over 24 Hours in ED. (75 and Older)",
              textOutput('avg_24_75'),
              showcase = bs_icon("radioactive"),
              theme = "danger"),
    
    card(card_header("# of Attendees Who Leave before decision to admit/discharge."),
         plotOutput("barplot")),
    
    card(card_header("# Waiting Longer than 24 Hours in ED (75+)"),
         plotOutput("line_24_75")),
    

    
    col_widths = c(6, 6, 4, 4, 4, 6, 6),
    row_heights = c(3, 1, 3)
  )
  
  
  
 
)
# Define server logic
server <- function(input, output) {

  
  selected_hospital <-
    reactive(if (is.null(input$hospitals)) hospitals else input$hospitals)
  
  selected_kpi <-
    reactive(if (is.null(input$kpis)) kpis else input$kpis)
  
  
  selected_data <-
    reactive({
    filter_data(selected_hospital(),
                selected_kpi())
      })
  
# Plots and Cards Output  
  
  # Line Plot for All Patients
  output$line_All <- renderPlot({
    plot_line_All(selected_data())

  })
  
  # Line Plot for 75+ Patients
  output$line_75 <- renderPlot({
    plot_line_75(selected_data())
    
  })
  
  
  
  
  # DNA Column Chart
  output$barplot <- renderPlot({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    plot_combo(combo_data)
    
  })
  

  
  # Line Plot for 75+ Patients
  output$line_24_75 <- renderPlot({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    plot_combo_24_75(combo_data)
    
  })
  
  # Average of Monthly DNA's Card
  output$avg_dna <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    monthly_average_dna(combo_data)
    
  })
  
  # National Average of DNA's Card
  output$avg_nat_dna <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == 'National')
    
    monthly_average_dna(combo_data)
    
  })
  
  
  # Average of Over 24 hours - 75+
  output$avg_24_75 <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    monthly_average_24_75(combo_data)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
