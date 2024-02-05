library(tidyverse)

df <- read_excel("MDR ED_Data.xlsx")

data <- df %>% 
  pivot_longer(cols = c(4:9),
                    names_to = 'Hospital',
                    values_to = 'Scores') %>% 
  mutate(Date = ymd(Date))

setwd("/cloud/project/data")
write_csv(data, 'data.csv')
