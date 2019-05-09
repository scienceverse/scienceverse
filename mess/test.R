library(reg)

# testing data referencing
mystudy <- study("Demo Study") %>%
  add_data(data = iris, id = "iris") %>%
  add_analysis(func = "View", params = list(x = ".data[iris]")) %>%
  study_analyse()
