## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(scienceverse)
library(dplyr)
library(tidyr)
library(shiny.i18n)
options("scipen"=10, "digits"=4)
source("R/utils.R")
source("i18n/trans.R")

## Interface Tab Items ----
source("tabs/study.R")
source("tabs/hyp.R")
source("tabs/ana.R")
source("tabs/others.R") # data, met, json, output


## UI ----
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Scienceverse"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Study Info", tabName = "study_tab"),
      menuItem("Hypotheses", tabName = "hyp_tab"),
      #menuItem("Methods", tabName = "met_tab"),
      menuItem("Data", tabName = "dat_tab"),
      menuItem("Analysis", tabName = "ana_tab"),
      menuItem("Human-Readable Summary", tabName = "output_tab"),
      menuItem("JSON", tabName = "json_tab")
    ),
    selectInput("lang", "Change language",
                choices = c(English = "en", Dutch = "nl"),
                selected = "en"),
    actionButton("study_analyse", "Analyse Data"),
    actionButton("reset_study", "Reset Study")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      study_tab,
      hyp_tab,
      #met_tab,
      dat_tab,
      ana_tab,
      json_tab,
      output_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  ## translation ----
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # trigger input label changes on language change
  observeEvent(input$lang, {
    for (func in names(trans_labels)) {
      for (nm in names(trans_labels[[func]])) {
        args <- list(
          session = session,
          inputId = nm,
          label = i18n()$t(trans_labels[[func]][[nm]])
        )
        do.call(func, args)
      }
    }
  })

  # reactive Vals ----
  myStudy <- reactiveVal( study(name = "", description = "") )
  criteria <- reactiveVal(list())
  return_list <- reactiveVal(list())
  authors <- reactiveVal(list())

  observeEvent(input$reset_study, {
    updateTextInput(session, "study_name", value = "")
    updateTextAreaInput(session, "study_description", value = "")

    s <- study(name = input$study_name,
               description = input$study_description)
    myStudy(s)
  })

  observeEvent(input$study_analyse, {
    s <- myStudy()

    tryCatch(s <- study_analyse(s),
             error = function(e) {
               js <- sprintf('alert("%s");', e$message)
               runjs(js)
    })
    myStudy(s)
  })

  observe({
    s <- myStudy()
    s$name <- input$study_name
    s$info$description <- input$study_description

    myStudy(s)
  })

  # add_hypothesis ----
  observeEvent(input$add_hypothesis, {
    s <- myStudy() %>%
      add_hypothesis(input$hyp_id,
                     input$hyp_description)

    # add criteria
    for (c in criteria()) {
      s <- add_criterion(s,
                         c$id,
                         c$result,
                         c$operator,
                         c$comparator,
                         input$hyp_id,
                         c$analysis_id)
    }

    # add evaluations
    # FIX: tryCatch for input errors and messages
    s <- add_eval(s, "c", input$eval_cor_eval,
                  input$eval_cor_desc, input$hyp_id)
    s <- add_eval(s, "f", input$eval_fal_eval,
                  input$eval_fal_desc, input$hyp_id)

    myStudy(s)

    # reset for new hypothesis
    criteria(list())
  }, ignoreNULL = TRUE)

  # add_analysis ----
  observeEvent(input$add_analysis, {
    # add analysis
    if (trimws(input$ana_id) == "") {
      showNotification("Analyses require an ID")
      return(FALSE)
    }
    if (trimws(input$ana_code) == "") {
      showNotification("Analyses require code")
      return(FALSE)
    }

    s <- myStudy()

    s <- tryCatch({add_analysis(s, input$ana_id,
                               input$ana_code,
                               return_list(),
                               type = "text")},
             error = function(e) {
               js <- sprintf('alert("%s");', e$message)
               runjs(js)
               return(FALSE)
             })

    # reset inputs
    if (!isFALSE(s)) {
      myStudy(s)

      updateTextInput(session, "ana_id", value = "")
      updateTextAreaInput(session, "ana_code", value = "")
      updateTextInput(session, "ana_return_name", value = "")
      updateTextInput(session, "ana_return_object", value = "")
      return_list(list())
    }
  }, ignoreNULL = TRUE)

  output$analysis_list <- renderUI({
    s <- myStudy()

    # update crit dropdown
    a <- sapply(s$analyses, function(x) x$id)
    updateSelectInput(session, "crit_ana_id", choices = a)

    a %>%
      sapply(function(x) {
        sprintf("1. [<a class='analysis_edit' analysis_id='%s'>edit</a>] [<a class='analysis_delete' analysis_id='%s'>delete</a>] %s\n\n    ```\n%s\n    ```",
                x, x, x,
                output_custom_code(s, x, "    "))
      }) %>%
      paste0(collapse = "\n") %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()
  })

  # add_data ----
  observeEvent(input$add_data, {
    s <- myStudy() %>%
      add_data(input$dat_id, loadedData())

    myStudy(s)
  }, ignoreNULL = TRUE)

  # add_criterion ----
  observeEvent(input$crit_id, {
    id <- input$crit_id
    newid <- id %>%
      gsub("^[^A-Za-z]", "", .) %>%
      gsub("[^[:alnum:]_\\.]", "", .)

    if (newid != id) {
      showNotification("Criterion IDs must start with a letter and only contain letters, numbers, underscores, and full stops.")
      updateTextInput(session, "crit_id", value = newid)
    }
  })

  observeEvent(input$add_criterion, {
    crit <- criteria()

    # check and coerce comparator value
    bool_vals <- list("TRUE", "FALSE", "true", "false")
    comp <- input$crit_comparator # always a text value
    if (comp %in% bool_vals) {
      comp <- as.logical(comp)
    } else {
      num <- suppressWarnings(as.numeric(comp))
      if (!is.na(num)) comp <- num
    }

    crit[[input$crit_id]] <- list(
      id = input$crit_id,
      result = input$crit_result,
      operator = input$crit_operator,
      comparator = comp,
      analysis_id = input$crit_ana_id
    )

    criteria(crit)

    # reset criteria
    updateTextInput(session, "crit_id", value = "")
    updateTextInput(session, "crit_result", value = "")
    updateTextInput(session, "crit_comparator", value = "")
  }, ignoreNULL = TRUE)

  output$criteria_table <- renderTable({
    crit <- do.call(rbind, criteria())
    if (is.null(crit)) return(data.frame())
    as.data.frame(crit)
  })

  output$criteria_warning <- renderText({
    s <- myStudy()
    if (length(s$analyses) > 0) {
      show("add_criterion")
      ""
    } else {
      hide("add_criterion")
      i18n()$t("Add an analysis first to be able to add criteria.")
    }
  })

  # add_return ----
  observeEvent(input$add_return, {
    r <- return_list()
    r[[input$ana_return_name]] <- input$ana_return_object
    return_list(r)
  }, ignoreNULL = TRUE)

  output$ana_return_list <- renderTable({
    r <- return_list()
    data.frame(
      name = names(r),
      object = unlist(r)
    )
  })

  # loadedData ----
  loadedData <- reactive({
    req(input$dat_file)

    if (input$dat_id == "") {
      input$dat_file$datapath %>%
        tools::file_ext() %>%
        paste0("." , .) %>%
        gsub("", input$dat_file$name) %>%
        updateTextInput(session, "dat_id", value = .)
    }

    rio::import(input$dat_file$datapath)
  })

  ### output$dat_table ----
  output$dat_table <- renderTable({
    loadedData()
  })

  ### output$json_text  ----
  output$json_text <- renderText({
    myStudy() %>% study_to_json()
  })

  ## output$human_readable ----
  output$human_readable <- renderUI({
    s <- myStudy()
    lvl <- 3
    i <- output_info(s, lvl, "html")
    h <- output_hypotheses(s, lvl, "html")
    a <- output_analyses(s, lvl, "html")
    r <- output_results(s, lvl, "html")

    paste(i, h, a, r, sep = "\n\n") %>% HTML()
  })

  # . authors ----
  output$credit_roles <- renderUI({
    x <- utils::capture.output(credit_roles())
    x %>%
      gsub("^\\[\\S+\\] ", "* **", .) %>%
      gsub(": ", "**: ", .) %>%
      paste(collapse = "\n") %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()
  })

  observeEvent(input$add_author, {
    problems <- FALSE

    # check orcid
    orcid <- check_orcid(input$orcid)
    if (isFALSE(orcid) & input$orcid != "") {
      problems <- TRUE
      updateTextInput(session, "orcid",
                      label = "ORCiD is not valid" %>% i18n()$t())
      shinyjs::addClass("orcid", "warning")
    }

    # check names
    for (nm in c("surname", "given")) {
      if (trimws(input[[nm]]) == "") {
        problems <- TRUE
        label <- ifelse(nm == "given",
                        "Given name",
                        "Last name") %>%
          paste("is missing") %>% i18n()$t()
        updateTextInput(session, nm, label = label)
        shinyjs::addClass(nm, "warning")
      }
    }

    if (!problems) {
      # add author
      a <- list(surname = trimws(input$surname),
                given = trimws(input$given),
                orcid = orcid,
                roles = input$roles,
                email = trimws(input$email))
      aa <- authors()
      aa[[input$author_n]] <- a
      authors(aa)

      # reset values
      updateTextInput(session, "author_n", value = length(aa)+1)
      updateTextInput(session, "given", value = "",
                      label = "Given Name(s) including initials" %>% i18n()$t())
      updateTextInput(session, "surname", value = "",
                      label = "Last Name(s)" %>% i18n()$t())
      updateTextInput(session, "email", value = "",
                      label = "Email" %>% i18n()$t())
      updateTextInput(session, "orcid", value = "",
                      label = "ORCiD" %>% i18n()$t())
      updateCheckboxGroupInput(session, "roles", selected = character(0))
      shinyjs::removeClass("given", "warning")
      shinyjs::removeClass("surname", "warning")
      shinyjs::removeClass("email", "warning")
      shinyjs::removeClass("orcid", "warning")
    }
  }, ignoreNULL = TRUE)

  # . . author_list ----
  output$author_list <- renderUI({
    a <- authors()
    if (length(a) > 1) {
      shinyjs::show("author_reorder")
    } else {
      shinyjs::hide("author_reorder")
    }
    make_author_list(a)
  })

  # . . author_reorder ----
  observeEvent(input$author_reorder, {
    ord <- strsplit(input$author_order, ",")[[1]] %>% as.integer()
    if (length(unique(ord)) != length(ord)) {
      js <- sprintf('alert("%s");',
                    i18n()$t("Each author must have a unique order"))
      runjs(js)
    } else {
      a <- authors()
      authors(a[ord])
    }
  }, ignoreNULL = TRUE)

  # . . author_edit ----
  observeEvent(input$author_edit, {
    to_edit <- as.integer(input$author_edit)
    message(to_edit)
    a <- authors()[[to_edit]]
    updateTextInput(session, "author_n", value = to_edit)
    updateTextInput(session, "given", value = a$given,
                    label = "Given Name(s) including initials" %>% i18n()$t())
    updateTextInput(session, "surname", value = a$surname,
                    label = "Last Name(s)" %>% i18n()$t())
    updateTextInput(session, "email", value = a$email,
                    label = "Email" %>% i18n()$t())
    updateTextInput(session, "orcid",
                    value = ifelse(isFALSE(a$orcid), "", a$orcid),
                    label = "ORCiD" %>% i18n()$t())
    updateCheckboxGroupInput(session, "roles", selected = a$roles)
    shinyjs::removeClass("given", "warning")
    shinyjs::removeClass("surname", "warning")
    shinyjs::removeClass("email", "warning")
    shinyjs::removeClass("orcid", "warning")
  }, ignoreNULL = TRUE)

  # . . author_delete ----
  observeEvent(input$author_delete, {
    to_del <- as.integer(input$author_delete)
    a <- authors()
    a[to_del] <- NULL
    authors(a)
  }, ignoreNULL = TRUE)

  observe({
    # update study$authors whenever authors() changes
    s <- myStudy()
    s$authors <- list()
    for (a in authors()) {
      if (a$email == "") a$email <- NULL
      s <- do.call(add_author, c(list(study = s), a))
    }
    myStudy(s)
  })

} # end server()

shinyApp(ui, server)
