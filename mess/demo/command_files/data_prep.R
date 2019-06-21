library(tidyverse)

#raw_data <- read_csv("apathy_depression_raw.csv")

processed_data <- raw_data %>%
  gather(key, val, a1:d5) %>%
  separate(key, "q", 1, extra = "drop") %>%
  group_by(sub_id, q) %>%
  summarise(val = mean(val)) %>%
  spread(q, val) %>%
  rename(apathy = a, depression = d)
