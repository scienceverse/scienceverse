## code to prepare `DATASET` dataset goes here

## functions ----
roxyDesc <- function(data, dataname, title, desc, vardesc, write = TRUE) {
  # save csv
  filename <-sprintf("data-raw/%s.csv", dataname)
  readr::write_csv(data, filename)

  # save cb
  cb <- codebook(data,
                 name = title,
                 vardesc = vardesc,
                 author = "Lisa DeBruine",
                 description = desc,
                 license = "CC-BY 4.0")
  jfilename <-sprintf("data-raw/%s.json", dataname)
  readr::write_file(cb, jfilename)

  file.copy(filename, "inst/extdata/")
  file.copy(jfilename, "inst/extdata/")

  # create Roxygen description
  itemdesc <- vardesc$description
  items <- paste0("#'    \\item{", names(itemdesc),
                  "}{", itemdesc, "}")

  s <- sprintf("# %s ----\n#' %s\n#'\n#' %s\n#'\n#' @format A data frame with %d rows and %d variables:\n#' \\describe{\n%s\n#' }\n#' \n\"%s\"\n\n",
               dataname, title,
               gsub("\n+", "\n#'\n#' ", desc),
               nrow(data), ncol(data),
               paste(items, collapse = "\n"),
               dataname
  )
  if (!isFALSE(write)) write(s, paste0("R/data_", dataname, ".R"))
}

# stroop ----
set.seed(8675309)
cc <- c("blue", "purple", "green", "red", "brown")
sub <- faux::sim_design(id = "sub_id", dv = "sub_i",
                        mu = 0, sd = 50, n= 50, plot = FALSE)
stroop <- faux::sim_design(
  within = list(word = cc, ink = cc, rep= 1:5),
  id = "sub_id", dv = "rt", long = TRUE,
  n = 50, mu = 650, sd = 100, r = 0.5, plot = FALSE
) %>%
  left_join(sub, by = "sub_id") %>%
  # 5% say the word instead of ink colour
  mutate(ink = as.character(ink),
         word = as.character(word),
         correct = rbinom(nrow(.), 1, .95),
         response = ifelse(correct == 1, ink, word),
         rt = ifelse(rbinom(nrow(.), 1, .05), NA, rt),
         response = ifelse(is.na(rt), NA, response),
         ink_effect = ifelse(ink == word, - 50, 0),
         rt = rt + ink_effect + sub_i) %>%
  select(sub_id, word, ink, response, rt) %>%
  arrange(sub_id)

usethis::use_data(stroop, overwrite = TRUE)

data <- stroop
dataname <- "stroop"
title <- "Stroop Task"
desc <- "50 simulated subjects in a stroop task viewing all combinations of word and ink colours blue, purple, green, red, and brown, 5 times each. Subjects respond with the ink colour. Subjects who do not respond in time have NA for response and rt."

vardesc <- list(
  description = list(
    sub_id = "Subject ID",
    word = "The text of the word",
    ink = "The ink colour of the word",
    response = "The subject's response (should equal the ink colour)",
    rt = "Reaction time (in ms)"
  )
)

roxyDesc(data, dataname, title, desc, vardesc)


## kin ----

kin <- data.frame(
  trust_self  = c(1,2,2,1,1,1,1,1,2,0,2,0,
                  1,2,2,3,2,2,1,1,2,0,0,1),
  trust_other = c(1,2,2,0,1,0,0,0,1,0,1,0,
                  1,1,1,0,1,2,2,0,0,0,2,1),
  recip_self  = c(0,1,3,2,1,1,1,3,3,2,3,1,
                  1,2,3,3,3,1,1,1,3,0,3,1),
  recip_other = c(1,1,2,2,3,2,1,3,3,1,3,0,
                  1,3,3,3,3,0,3,0,1,0,3,2)
)
usethis::use_data(kin, overwrite = TRUE)

data <- kin
dataname <- "kin"
title <- "Kinship"
desc <- "Data from DeBruine (2002) Facial Resemblance Enhances Trust, PRSLB. https://osf.io/ewfhs/"

vardesc <- list(
  description = list(
    trust_self  = "Number of trusting moves towards self-morphs",
    trust_other = "Number of trusting moves towards self-morphs",
    recip_self  = "Number of reciprocating moves towards other-morphs",
    recip_other = "Number of reciprocating moves towards other-morphs"
  ),
  dataType = rep("int", 4) # all variables are integer types
)

roxyDesc(data, dataname, title, desc, vardesc)

## study_demo ----

study_demo <- study(name = "Kinship and Prosocial Behaviour (Demo)",
           description = "A reanalysis of data from DeBruine (2002) Facial Resemblance Enhances Trust, PRSLB.",
           year = 2020) %>%
  add_author(orcid = "0000-0002-7523-5539",
             surname = "DeBruine",
             given = "Lisa M.",
             roles = c("con", "dat", "sof", "dra", "edi"),
             email = "lisa.debruine@glasgow.ac.uk") %>%
  add_author(orcid = "0000-0002-0247-239X",
             surname = "Lakens",
             given = "Daniël",
             roles = c("con", "ana", "dra", "edi")) %>%
  add_hypothesis(id = "self_pref",
                 description = "Cues of kinship will increase prosocial behaviour. Cues of kinship will be manipulated by morphed facial self-resemblance. Prosocial behaviour will be measured by responses in the trust game. The prediction is that the number of trusting AND/OR reciprocating moves will be greater to self morphs than to other morphs.") %>%
  add_analysis(id = "trust",
               code = t.test(kin$trust_self,
                             kin$trust_other,
                             paired = TRUE,
                             conf.level = 0.975)) %>%
  add_analysis(id = "recip",
               code = t.test(kin$recip_self,
                             kin$recip_other,
                             paired = TRUE,
                             conf.level = 0.975)) %>%
  add_criterion("t_lo", "conf.int[1]", ">", 0.0,
                "trust", "self_pref") %>%
  add_criterion("t_hi", "conf.int[2]", ">", 0.2,
                "trust") %>%
  add_criterion("r_lo", "conf.int[1]", ">", 0.0,
                "recip") %>%
  add_criterion("r_hi", "conf.int[2]", ">", 0.2,
                "recip") %>%
  add_eval("corroboration",
           evaluation = "(t_lo & t_hi) | (r_lo & r_hi)",
           description = "The hypothesis is corroborated if the 97.5% CI lower bound is greater than 0 and the 97.5% CI upper bound is greater than 0.2 (the SESOI) for either the trust or reciprocation moves.") %>%
  add_eval("falsification",
           evaluation = "!t_hi & !r_hi",
           description = "The hypothesis is falsified if the 97.5% CI upper bound is smaller than 0.2 (the SESOI) for both trust and reciprocation.") %>%
  add_data(id = "kin",
           data = kin,
           vardesc = desc,
           url = "https://osf.io/ewfhs/") %>%
  study_analyse()

usethis::use_data(study_demo, overwrite = TRUE)
