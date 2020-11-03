#' Output custom code
#'
#' Output custom code specified in an analysis component
#'
#' @param study A study list object with class scivrs_study
#' @param analysis_id The id or index for the analysis code to output (defaults to index 1)
#' @param prefix Text to prepend to each line (for indenting)
#'
#' @return string of the function definition
#' @export
#'
#' @examples
#'
#' custom <- function() { 1:10 }
#' s <- study() %>%
#'   add_analysis("custom", t.test(rnorm(100))) %>%
#'   output_custom_code()
output_custom_code <- function(study, analysis_id = 1, prefix = "") {
  a_id <- get_idx(study, analysis_id, "analyses")
  code <- study$analyses[[a_id]]$code

  ## strip minimum leading space
  strip_space <- code %>%
    sub("^( *).*$", "\\1", .) %>%
    nchar() %>% min() %>% rep(" ", .) %>%
    paste(collapse = "") %>%
    paste0("^", .)

  code %>%
    gsub(strip_space, "", .) %>%
    paste(collapse= paste0("\n", prefix)) %>%
    paste0(prefix, .)
}



#' Output hypotheses
#'
#' Output hypotheses specified in the json file
#'
#' @param study A study list object with class scivrs_study
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#'
#' @return character string with a human-readable summary of the hypotheses
#'
#' @export

output_hypotheses <- function(study, header_lvl = 2,
                              output = c("md", "html", "text")) {
  header <- rep("#", header_lvl) %>% paste(collapse = "")

  txt <- paste(header, "Hypotheses\n\n")

  if (length(study$hypotheses) == 0) {
    txt <- paste0(txt, "No hypotheses\n\n")
  } else {

    for (i in 1:length(study$hypotheses)) {

      txt <- paste0(txt, header, "# Hypothesis ", i,
          ": ", study$hypotheses[[i]]$id, "\n\n",
          study$hypotheses[[i]]$description, "\n\n", sep = "")

      txt <- paste0(txt, header, "## Criteria\n\n", sep = "")
      criteria <- study$hypotheses[[i]]$criteria

      for (criterion in criteria) {
        txt <- sprintf(
          "%s* `%s` is confirmed if analysis `%s` yields `%s %s %s`  \n",
          txt,
          criterion$id,
          criterion$analysis_id,
          criterion$result,
          criterion$operator,
          criterion$comparator
        )
      }

      txt <- paste0(txt, "\n")

      # explain evaluation
      txt <- paste0(txt, header, "## Evaluation\n\n", sep = "")
      txt <- paste0(txt, header, "### Corroboration\n\n", sep = "")
      txt <- paste0(txt, study$hypotheses[[i]]$corroboration$description, "\n\n")
      txt <- paste0(txt, "```\n", study$hypotheses[[i]]$corroboration$evaluation, "\n```\n\n")
      txt <- paste0(txt, header, "### Falsification\n\n", sep = "")
      txt <- paste0(txt, study$hypotheses[[i]]$falsification$description, "\n\n")
      txt <- paste0(txt, "```\n", study$hypotheses[[i]]$falsification$evaluation, "\n```\n\n")
      txt <- paste0(txt, "All other patterns of results are inconclusive.")

      txt <- paste0(txt, "\n\n\n")
    }
  }

  format_output(txt, output) %>% invisible()
}


#' Output results
#'
#' Output results specified in the json file
#'
#' @param study A study list object with class scivrs_study
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#' @param digits integer indicating the number of decimal places.
#'
#' @return character string with a human-readable summary of the results
#'
#' @export

output_results <- function(study, header_lvl = 2,
                           output = c("md", "html", "text"),
                           digits = 3) {
  header <- rep("#", header_lvl) %>% paste(collapse = "")

  txt <- paste(header, "Results\n\n")
  if (length(study$hypotheses) == 0) {
    txt <- paste0(txt, "No hypotheses\n\n")
  } else {
    for (i in 1:length(study$hypotheses)) {
      h <- study$hypotheses[[i]]

      txt <- paste0(txt, header, "# Hypothesis ", i, ": ", h$id, "\n\n", h$desc, "\n\n", sep = "")

      criteria <- h$criteria
      analysis_ids <- sapply(study$analyses, function(x) {x$id})

      for (criterion in criteria) {
        analysis <- match(criterion$analysis_id, analysis_ids)

        # get result and comparator values from results
        results <- study$analyses[[analysis]]$results
        value <- get_res_value(criterion$result, results)
        comp_value <- get_res_value(criterion$comparator, results)

        conc_color <- ifelse(criterion$conclusion, "green", "red")

        comp_res <- ""
        if (comp_value != criterion$comparator) {
          comp_res <- sprintf("; %s = %s",
                              criterion$comparator,
                              comp_value)
        }

        txt <- sprintf("%s* `%s` is confirmed if analysis [%s](#%s) yields %s %s %s\nThe result was %s = %s%s (<span style=\"color:%s;\">%s</span>)  \n",
                       txt,
                       criterion$id,
                       criterion$analysis_id,
                       criterion$analysis_id,
                       criterion$result,
                       criterion$operator,
                       criterion$comparator,
                       criterion$result,
                       round_char(value, digits, TRUE),
                       comp_res,
                       conc_color,
                       criterion$conclusion
        )
      }
      txt <- paste0(txt, "\n\n")

      # explain evaluation
      cc <- h$corroboration
      ff <- h$falsification
      conc_color <- ifelse(cc$result, "green", "red")
      txt <- paste0(txt, header, "## Corroboration (<span style=\"color:", conc_color, "\">", cc$result, "</span>)\n\n", sep = "")
      txt <- paste0(txt, cc$description, "\n\n")
      txt <- paste0(txt, "```\n", cc$evaluation, "\n```\n\n")

      conc_color <- ifelse(ff$result, "green", "red")
      txt <- paste0(txt, header, "## Falsification (<span style=\"color:", conc_color, "\">", ff$result, "</span>)\n\n", sep = "")
      txt <- paste0(txt, ff$description, "\n\n")
      txt <- paste0(txt, "```\n", ff$evaluation, "\n```\n\n")

      # conclusion
      if (is.null(h$conclusion)) {
        txt <- paste0(txt, "<span style=\"color: blue\">")
        txt <- paste0(txt, "**No conclusion.**")
      } else if (h$conclusion == "corroborate") {
        txt <- paste0(txt, "<span style=\"color: green\">")
        txt <- paste0(txt, "**All criteria were met for corroboration.**")
      } else if (h$conclusion == "falsify") {
        txt <- paste0(txt, "<span style=\"color: red\">")
        txt <- paste0(txt, "**All criteria were met for falsification.**")
      } else {
        txt <- paste0(txt, "<span style=\"color: blue\">")
        txt <- paste0(txt, "**Neither the corroboration nor falsification criteria were met.**")
      }

      txt <- paste0(txt, "</span>\n\n")
    }
  }

  format_output(txt, output) %>% invisible()
}


#' Output analyses
#'
#' Output analysis plan specified in the json file
#'
#' @param study A study list object with class scivrs_study
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#' @param results whether to include results (defaults to TRUE)
#'
#' @return character string with a human-readable summary of the analyses
#'
#' @export

output_analyses <- function(study, header_lvl = 2,
                            output = c("md", "html", "text"), results = TRUE) {
  header <- rep("#", header_lvl) %>% paste(collapse = "")

  txt <- paste(header, "Analyses\n\n")

  if (length(study$analyses) == 0) {
    txt <- paste0(txt, "No analyses\n\n")
  } else {
    for (i in 1:length(study$analyses)) {
      a <- study$analyses[[i]]
      txt <- paste0(txt, header, "# Analysis ", i, ": ", a$id,
          " {#", a$id, "}\n\n", sep = "")

      txt <- output_custom_code(study, i) %>%
        paste0() %>%
        paste0(txt, "<pre>", ., "</pre>\n\n", sep = "")

      # show each analysis results if available
      if (length(a$results) > 0 & results) {
        txt <- paste0(txt, nested_list(a$results, quote = "`"), "\n\n")
      }
    }
  }

  format_output(txt, output) %>% invisible()
}


#' Output data
#'
#' Output data specified in the json file
#'
#' @param study A study list object with class scivrs_study
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#'
#' @return character string with a human-readable summary of the data
#'
#' @export

output_data <- function(study, header_lvl = 2,
                        output = c("md", "html", "text")) {
  header <- rep("#", header_lvl) %>% paste(collapse = "")

  if (length(study$data) == 0) {
    ds <- "No data\n\n"
  } else {
    ds <- sapply(study$data, function(d) {
      cb <- scienceverse:::cb_vars(d$codebook)
      desc <- if_nowt(d$codebook$description)

      sprintf("%s# %s\n\n%s\n\n%s",
              header, d$id, desc, cb)
    }) %>%
      paste0(collapse = "\n\n")
  }

  txt <- paste0(header, " Data\n\n", ds)

  format_output(txt, output) %>% invisible()
}


#' Output study info
#'
#' Output study info specified in the json file
#'
#' @param study A study list object with class scivrs_study
#' @param header_lvl The starting header level for the section (defaults to 2)
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#'
#' @return character string with a human-readable summary of the study info
#'
#' @export

output_info <- function(study, header_lvl = 2,
                        output = c("md", "html", "text")) {
  header <- rep("#", header_lvl) %>% paste(collapse = "")

  txt <- paste0(header, " ", study$name, "\n\n")

  if ("description" %in% names(study$info)) {
    txt <- paste0(txt, study$info$description, "\n\n")
    study$info$description <- NULL
  }

  if (length(study$info) > 0) {
    txt <- paste0(txt, nested_list(study$info), "\n\n")
  }

  if ("authors" %in% names(study)) {
    txt <- paste0(txt, header, "# Authors\n\n")
    for (a in study$authors) {
      olink <-  paste0(" ([", a$orcid ,"](https://orcid.org/",
                       a$orcid, "))")
      roles <- paste(":", paste(a$roles, collapse = ", "))
      txt <- sprintf("%s* **%s, %s**%s%s\n",
                     txt, a$name$surname, a$name$given,
                     ifelse(!isFALSE(a$orcid), olink, ""),
                     ifelse(length(a$roles)>0, roles, ""))

      # check for other attributes
      a$orcid <- NULL
      a$name <- NULL
      a$roles <- NULL
      if (length(a) > 0) {
        txt <- sprintf("%s\n%s\n", txt, nested_list(a, pre = "  "))
      }
    }
    txt <- paste0(txt, "\n\n")
  }

  format_output(txt, output) %>% invisible()
}

#' Output as md, html or text
#'
#' @param txt the md output of output_**** functions
#' @param output whether the output should be markdown, html, or plain text (defaults to md)
#'
#' @return character string in the specified format
#' @keywords internal

format_output <- function(txt, output = c("md", "html", "text")) {
  output <- match.arg(output)
  if (output == "md") {
    if (scienceverse_options("verbose")) cat(txt)
    return(txt)
  } else if (output == "html") {
    html <- txt %>% # deal with headers > 6
      gsub("^#{7,}\\s*([^\n]*)\n", "**\\1**\n", .) %>%
      gsub("\n#{7,}\\s*([^\n]*)\n", "\n**\\1**\n", .) %>%
      markdown::renderMarkdown(text = .)

    if (scienceverse_options("verbose")) cat(html)
    return(html)
  } else if (output == "text") {
    text <- txt %>%
      gsub("^#+\\s+", "", .) %>% # get rid of first #
      gsub("\n#+\\s+", "\n", .) %>% # get rid of subsequent #
      gsub("\\{#\\S*\\}", "", .) # get rid of {#ID} tags

    if (scienceverse_options("verbose")) cat(text)
    return(text)
  }
}

