library(reg)
library(tidyverse)

read_csv("mess/apathy_depression.csv") %>%
  select(sub_id, a1:d5) %>%
  write_csv("mess/apathy_depression_raw.csv")

raw <- read_csv("mess/apathy_depression_raw.csv")

# testing data referencing
mystudy <- study("AD Study") %>%
  add_data(data = "mess/apathy_depression_raw.csv", id = "raw") %>%
  add_prep(data = "raw", code = prep)
