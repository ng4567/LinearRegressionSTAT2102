
# %>% <- tibble, certain type of object used within tinyverse, google, tibbles and summarize across multiple variables, tidyverse R guide, summarise function
library(dplyr)
data <- read.csv("C:/Users/d/Downloads/2.4 Data Set for Assignment.csv")

for (v in data$date) {
  print(v)
}