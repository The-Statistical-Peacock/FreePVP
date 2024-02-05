# helpers.R

filter_data <- function(hospital, kpi) {
  
  data |>
    filter(Hospital %in% hospital,
           KPI %in% kpi)
}


monthly_average_24_75 <- function(data) {
  
  data %>% 
    filter(Metric == '75.0',
           KPI == '# > 24 Hours') %>% 
    summarise(Mean = mean(Scores)) %>% 
    pull(Mean) %>% 
    round() %>% 
    format(big.mark = ",") 
}



monthly_average_dna <- function(data) {
  
  data %>% 
    filter(Metric == 'none',
           KPI == '# DNA') %>% 
    summarise(Mean = mean(Scores)) %>% 
    pull(Mean) %>% 
    round() %>% 
    format(big.mark = ",") 
}

plot_line_All <- function(data) {
  
  data %>% 
    filter(Metric == 'All') %>% 
    ggplot(aes(x = Date, y = Scores)) +
    geom_line(color = "#FF9E00", size = 2) +
    geom_point(color = "orange", size = 3) +
    labs(title = "",
         x = "",
         y = "") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis font size
          axis.text.y = element_text(size = 12),  # Increase y-axis font size
          axis.title = element_text(size = 14)) + 
    scale_y_continuous(labels = scales::percent_format()) + # Format y-axis as percentage
    scale_x_date(labels = scales::date_format("%b-%y"),  # Format x-axis dates
                 date_breaks = "1 month",  # Set breaks at every month
                 expand = c(0.02, 0.02))  # Adjust the spacing
}





plot_line_75 <- function(data){
 
    data %>% 
      filter(Metric == '75.0') %>% 
      ggplot(aes(x = Date, y = Scores)) +
      geom_line(color = "#FF9E00", size = 2) +
      geom_point(color = "orange", size = 3) +
      labs(title = "",
           x = "",
           y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase x-axis font size
            axis.text.y = element_text(size = 12),  # Increase y-axis font size
            axis.title = element_text(size = 14)) + 
      scale_y_continuous(labels = scales::percent_format()) + # Format y-axis as percentage
      scale_x_date(labels = scales::date_format("%b-%y"),  # Format x-axis dates
                   date_breaks = "1 month",  # Set breaks at every month
                   expand = c(0.02, 0.02))  # Adjust the spacing
  
  
}


plot_combo <- function(data){
  
  data %>% 
    filter(Metric == 'none',
           KPI == '# DNA')%>% 
    ggplot(aes(x = Date, y = Scores)) +
    geom_col(fill = "#5F3DC4") +
    labs(title = "",
         x = "",
         y = "") +
    scale_x_date(labels = scales::date_format("%b-%y"),  # Format x-axis dates
                 date_breaks = "1 month",  # Set breaks at every month
                 expand = c(0.02, 0.02)) + # Adjust the spacing
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),  # Increase y-axis font size
          axis.title = element_text(size = 14))  # Rotate x-axis text
  
}



plot_combo_24_75 <- function(data){
  
  data %>% 
    filter(Metric == '75.0',
           KPI == '# > 24 Hours')%>% 
    ggplot(aes(x = Date, y = Scores)) +
    geom_col(fill = "#FFDE0E") +
    labs(title = "",
         x = "",
         y = "") +
    scale_x_date(labels = scales::date_format("%b-%y"),  # Format x-axis dates
                 date_breaks = "1 month",  # Set breaks at every month
                 expand = c(0.02, 0.02)) + # Adjust the spacing
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),  # Increase y-axis font size
          axis.title = element_text(size = 14))  # Rotate x-axis text
  
}




