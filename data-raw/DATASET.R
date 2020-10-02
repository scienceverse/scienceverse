## code to prepare `DATASET` dataset goes here

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

readr::write_csv(stroop, "data-raw/stroop.csv")

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

cb <- codebook(data,
               name = title,
               vardesc = vardesc,
               author = "Lisa DeBruine",
               description = desc,
               license = "CC-BY 4.0")

readr::write_file(cb, paste0("data-raw/", dataname, ".json"))


# create Roxygen description
itemdesc <- vardesc$description
items <- paste0("#'    \\item{", names(itemdesc), "}{", itemdesc, "}")

s <- sprintf("# %s ----\n#' %s\n#'\n#' %s\n#'\n#' @format A data frame with %d rows and %d variables:\n#' \\describe{\n%s\n#' }\n#' \n\"%s\"\n\n",
             dataname, title,
             gsub("\n+", "\n#'\n#' ", desc),
             nrow(data), ncol(data),
             paste(items, collapse = "\n"),
             dataname
)
if (!isFALSE(write)) write(s, paste0("R/data_", dataname, ".R"))

usethis::use_data(stroop, overwrite = TRUE)

file.copy("data-raw/stroop.csv", "inst/extdata/")
file.copy("data-raw/stroop.json", "inst/extdata/")
