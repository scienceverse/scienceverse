library(reg)

# set working dir to this file's dir
rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% setwd()

s <- study("Demo")

# add prep from a file
s <- add_prep(s, code = "command_files/data_prep.R",
              params = list("raw_data" = ".data[ad]"),
              return = c("processed_data"))

source("command_files/eq_test_r.R")

s <- add_analysis(s, func = "eq_test_r",
                  params = list(
                    data = ".data[processed_data]",
                    col1 = "apathy",
                    col2 = "depression",
                    alpha = 0.05,
                    high_eqbound_r = 0.3,
                    low_eqbound_r = -0.3
                  ),
                  code = eq_test_r,
                  id = "A1")

s <- add_hypothesis(s, "The correlation between the apathy and depression scale is smaller than 0.3", id = "H1")

s <- add_criterion(
  s,
  result = "TOST_p2",
  operator = "<",
  comparator = 0.01,
  analysis_id = "A1",
  hypothesis_id = "H1"
)

study_report(s, "prereg", "pre")

study_save(s, "pre_study.json")



# remove objects from environment to test reloading from json
rm(s)
rm(eq_test_r)
rm(prep_func)

s <- study("pre_study.json")


s <- add_data(s, "original_data/apathy_depression_raw.csv", id = "ad")
s <- data_prep(s)
s <- study_analyze(s)

study_report(s, "postreg", "post")

study_save(s, "post_study.json")
