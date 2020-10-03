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
source("R/demo.R")
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
      id = "tabs",
      menuItem("Study Info", tabName = "study_tab"),
      menuItem("Authors", tabName = "aut_tab"),
      menuItem("Hypotheses", tabName = "hyp_tab"),
      #menuItem("Methods", tabName = "met_tab"),
      menuItem("Data", tabName = "dat_tab"),
      menuItem("Analysis", tabName = "ana_tab"),
      menuItem("Summaries", tabName = "output_tab")
    ),
    actionButton("demo", "Load Demo Study"),
    actionButton("study_analyse", "Analyse Data"),
    actionButton("reset_study", "Reset Study"),

    selectInput("lang", "Change language",
                choices = c(English = "en", Dutch = "nl"),
                selected = "en"),
    p("Most of the phrases have not been translated; this is just a proof of concept.")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      study_tab,
      aut_tab,
      hyp_tab,
      #met_tab,
      dat_tab,
      ana_tab,
      output_tab
    )
  )
)

## save translations ----
is_local <- Sys.getenv('SHINY_PORT') == ""
if (is_local) {
  all_text <- c(trans_text, trans_labels) %>%
    unname() %>%
    unlist() %>%
    unique() %>%
    sort()

  j <- jsonlite::read_json("i18n/translation.json")
  cur_text <- sapply(j$translation, "[[", "en")
  new_text <- setdiff(all_text, cur_text) %>%
    lapply(function(x) { list("en" = x) })

  if (length(new_text)) {
    j$translation <- c(j$translation, new_text)

    # save to new_translation so user can choose to update
    jsonlite::toJSON(j, auto_unbox = TRUE) %>%
      jsonlite::prettify(2) %>%
      write("i18n/new_translation.json")
  }
}


## server ----
server <- function(input, output, session) {
  ## translation ----

  ## . . create translator ----
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$languages) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # . . on language change ----
  observeEvent(input$lang, {
    # text changes (h3, h4, p)
    for (h in trans_text) {
      js <- sprintf("$('*[en=\"%s\"]').text(\"%s\");",
                    h, i18n()$t(h))
      shinyjs::runjs(js)
    }

    # input label changes
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
  my_study <- reactiveVal( study(name = "", description = "") )
  criteria <- reactiveVal(list())
  return_list <- reactiveVal(list())
  authors <- reactiveVal(list())
  author_info <- reactiveVal(list())
  loaded_data <- reactiveVal(data.frame())
  custom_info <- reactiveVal(list())

  # functions ----
  section_delete <- function(section, idx) {
    s <- my_study()
    idx <- as.integer(idx)
    s[[section]][idx] <- NULL
    my_study(s)
  }

  # actions ----
  # . . reset_study ----
  observeEvent(input$reset_study, {
    c("study_name", "given", "surname", "orcid",
      "eval_cor_eval", "eval_fal_eval") %>%
      lapply(updateTextInput,
             session = session,
             value = "")

    c("study_description", "hyp_description")%>%
      lapply(updateTextAreaInput,
             session = session,
             value = "")

    authors(list())

    s <- study(name = input$study_name,
               description = input$study_description)
    my_study(s)
  })

  # . . study_analyse ----
  observeEvent(input$study_analyse, {
    s <- my_study()

    if (length(s$analyses) == 0) {
      i18n()$t("No analyses have been specified") %>%
        sprintf("alert('%s')", .) %>%
        runjs()
    } else {
      tryCatch(s <- study_analyse(s),
               error = function(e) {
                 js <- sprintf('alert("%s");', e$message)
                 runjs(js)
      })
      updateTabItems(session, "tabs", "output_tab")
      my_study(s)
    }
  })





  # study ----
  observe({
    s <- my_study()
    s$name <- input$study_name
    s$info <- c(list(description = input$study_description),
                custom_info())

    my_study(s)
  })

  # . . add_custom_info ----
  observeEvent(input$add_custom_info, {
    # check for blanks
    v <- trimws(input$custom_info_value)
    n <- trimws(input$custom_info_name)
    if (n == "" | v == "") return(FALSE)

    # add new info
    ci <- custom_info()
    ci[n] <- v
    custom_info(ci)

    # reset inputs
    shinyjs::reset("custom_info_name")
    shinyjs::reset("custom_info_value")
  })

  # . . custom_info_list ----
  output$custom_info_list <- renderUI({
    ci <- custom_info()
    if (length(ci) == 0) return("")

    section <- "custom_info"
    mapply(function(x, i, n) {
      sprintf("1. [<a class='section_edit' section='%s' section_idx='%d'>%s</a>] [<a class='section_delete' section='%s' section_idx='%d'>%s</a>] %s: %s\n\n",
              section, i, i18n()$t("edit"),
              section, i, i18n()$t("delete"),
              n, x)
    }, ci, 1:length(ci), names(ci)) %>%
      paste0(collapse = "\n") %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()
  })

  ## . . custom_info_edit ----
  observeEvent(input$custom_info_edit, {
    idx <- as.integer(input$custom_info_edit)
    name <- names(custom_info())[[idx]]
    value <- custom_info()[[idx]]

    updateTextInput(session, "custom_info_name", value = name)
    updateTextAreaInput(session, "custom_info_value", value = value)
  }, ignoreNULL = TRUE)

  ## . . custom_info_delete ----
  observeEvent(input$custom_info_delete, {
    idx <- as.integer(input$custom_info_delete)
    ci <- custom_info()
    ci[[idx]] <- NULL
    custom_info(ci)
  }, ignoreNULL = TRUE)


  # authors ----

  # . . get_orcid ----
  observeEvent(input$get_orcid, {
    o <- get_orcid(input$surname, input$given)
    n <- length(o)
    if (n == 1) {
      updateTextInput(session, "orcid", value = o)
    } else {
      i18n()$t("%d ORCiDs found") %>%
        sprintf(n) %>%
        sprintf('alert("%s");', .) %>%
        runjs()
    }
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
                roles = input$roles)
      aa <- authors()
      aa[[input$author_n]] <- a
      authors(aa)

      # reset values
      updateTextInput(session, "author_n", value = length(aa)+1)
      updateTextInput(session, "given", value = "",
                      label = "Given Name(s) including initials" %>% i18n()$t())
      updateTextInput(session, "surname", value = "",
                      label = "Last Name(s)" %>% i18n()$t())
      updateTextInput(session, "orcid", value = "",
                      label = "ORCiD" %>% i18n()$t())
      updateCheckboxGroupInput(session, "roles", selected = character(0))
      shinyjs::removeClass("given", "warning")
      shinyjs::removeClass("surname", "warning")
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
      i18n()$t("Each author must have a unique order") %>%
        sprintf('alert("%s");', .) %>%
        runjs()
    } else {
      a <- authors()
      authors(a[ord])
    }
  }, ignoreNULL = TRUE)

  # . . add_author_info ----
  observeEvent(input$add_author_info, {
    # check for blanks
    v <- trimws(input$author_info_value)
    n <- trimws(input$author_info_name)
    if (n == "" | v == "") return(FALSE)

    # add new info
    ci <- author_info()
    ci[n] <- v
    author_info(ci)

    # reset inputs
    shinyjs::reset("author_info_name")
    shinyjs::reset("author_info_value")
  })

  # . . author_info_list ----
  output$author_info_list <- renderUI({
    section <- "author_info"
    info <- author_info()
    if (length(info) == 0) return("")

    mapply(function(x, i, n) {
      sprintf("1. [<a class='section_edit' section='%s' section_idx='%d'>%s</a>] [<a class='section_delete' section='%s' section_idx='%d'>%s</a>] %s: %s\n\n",
              section, i, i18n()$t("edit"),
              section, i, i18n()$t("delete"),
              n, x)
    }, info, 1:length(info), names(info)) %>%
      paste0(collapse = "\n") %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()
  })

  ## . . author_info_edit ----
  observeEvent(input$author_info_edit, {
    idx <- as.integer(input$author_info_edit)
    name <- names(author_info())[[idx]]
    value <- author_info()[[idx]]

    updateTextInput(session, "author_info_name", value = name)
    updateTextAreaInput(session, "author_info_value", value = value)
  }, ignoreNULL = TRUE)

  ## . . author_info_delete ----
  observeEvent(input$author_info_delete, {
    idx <- as.integer(input$author_info_delete)
    ci <- author_info()
    ci[[idx]] <- NULL
    author_info(ci)
  }, ignoreNULL = TRUE)

  # . . author_edit ----
  observeEvent(input$author_edit, {
    idx <- as.integer(input$author_edit)
    a <- authors()[[idx]]
    updateTextInput(session, "author_n", value = idx)
    updateTextInput(session, "given", value = a$given,
                    label = "Given Name(s) including initials" %>% i18n()$t())
    updateTextInput(session, "surname", value = a$surname,
                    label = "Last Name(s)" %>% i18n()$t())
    updateTextInput(session, "orcid",
                    value = ifelse(isFALSE(a$orcid), "", a$orcid),
                    label = "ORCiD" %>% i18n()$t())
    updateCheckboxGroupInput(session, "roles", selected = a$roles)

    # update custom author info
    shinyjs::reset("author_info_name")
    shinyjs::reset("author_info_value")
    a$given <- NULL
    a$surname <- NULL
    a$orcid <- NULL
    a$roles <- NULL
    author_info(a)

    shinyjs::removeClass("given", "warning")
    shinyjs::removeClass("surname", "warning")
    shinyjs::removeClass("orcid", "warning")
  }, ignoreNULL = TRUE)

  # . . author_delete ----
  observeEvent(input$author_delete, {
    a <- authors()
    idx <- as.integer(input$author_delete)
    a[idx] <- NULL
    authors(a)
  }, ignoreNULL = TRUE)

  # . . add_author ----
  observe({
    # update study$authors whenever authors() changes
    s <- my_study()
    s$authors <- list()
    for (a in authors()) {
      s <- do.call(add_author, c(list(study = s), a))
    }
    my_study(s)
  })

  # . . credit_roles ----
  output$credit_roles <- renderUI( get_credit_roles() )

  # . . jats_text  ----
  output$jats_text <- renderText({
    author_jats(my_study())
  })

  # . . download_jats ----
  output$download_jats <- downloadHandler(
    filename = function() {
      paste0(input$study_name, "_credit.xml")
    },
    content = function(file) {
      j <- author_jats(my_study())
      write(j, file)
    }
  )

  # hypotheses ----

  # . . add_hypothesis ----
  observeEvent(input$add_hypothesis, {
    s <- my_study() %>%
      add_hypothesis(input$hyp_id,
                     input$hyp_description)

    # add criteria
    for (c in criteria()) {
      s <- add_criterion(
        study = s,
        id = c$id,
        result = c$result,
        operator = c$operator,
        comparator = c$comparator,
        analysis_id = c$analysis_id,
        hypothesis_id = input$hyp_id
      )
    }

    # add evaluations
    s <- tryCatch({add_eval(s, "c", input$eval_cor_eval,
                  "", input$hyp_id)},
                  message = function(e) {
                    sprintf('alert("%s");', e$message) %>%
                      runjs()
                    return(FALSE)
                  },
                  warning = function(e) {
                    sprintf('alert("%s");', e$message) %>%
                      runjs()
                    return(FALSE)
                  },
                  error = function(e) {
                    sprintf('alert("%s");', e$message) %>%
                      runjs()
                    return(FALSE)
                  })
    if (isFALSE(s)) return(FALSE)
    s <- tryCatch({add_eval(s, "f", input$eval_fal_eval,
                  "", input$hyp_id)},
                  message = function(e) {
                    sprintf('alert("%s");', e$message) %>%
                      runjs()
                    return(FALSE)
                  },
                  warning = function(e) {
                    sprintf('alert("%s");', e$message) %>%
                      runjs()
                    return(FALSE)
                  },
                  error = function(e) {
                    sprintf('alert("%s");', e$warning) %>%
                      runjs()
                    return(FALSE)
                  })
    if (isFALSE(s)) return(FALSE)

    my_study(s)

    # reset for new hypothesis
    shinyjs::reset("hyp_id")
    shinyjs::reset("hyp_description")
    shinyjs::reset("eval_cor_eval")
    shinyjs::reset("eval_fal_eval")
    criteria(list())
  }, ignoreNULL = TRUE)

  # . . hypothesis_list ----
  output$hypotheses_list <- renderUI({
    make_section_list(my_study(), "hypotheses")
  })

  ## . . hypotheses_edit ----
  observeEvent(input$hypotheses_edit, {
    s <- my_study()
    idx <- as.integer(input$hypotheses_edit)
    h <- s$hypotheses[[idx]]

    criteria(h$criteria)

    updateTextInput(session, "hyp_id", value = h$id)
    updateTextAreaInput(session, "hyp_description",
                        value = h$description)
    updateTextInput(session, "eval_cor_eval",
                    value = h$corroboration$evaluation)
    updateTextInput(session, "eval_fal_eval",
                    value = h$falsification$evaluation)

  }, ignoreNULL = TRUE)

  ## . . hypotheses_delete ----
  observeEvent(input$hypotheses_delete, {
    section_delete("hypotheses", input$hypotheses_delete)
  }, ignoreNULL = TRUE)

  # critera ----

  # . . add_criterion ----
  observeEvent(input$add_criterion, {
    if (input$crit_id == "" |
        input$crit_result == "" |
        input$crit_comparator == "" |
        input$crit_ana_id == "") {
      return(FALSE)
    }

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

  # . . crit_id ----
  observeEvent(input$crit_id, {
    id <- input$crit_id
    newid <- id %>%
      gsub("^[^A-Za-z]", "", .) %>%
      gsub("[^[:alnum:]_\\.]", "", .)

    if (newid != id) {
      i18n()$t("Criterion IDs must start with a letter and only contain letters, numbers, underscores, and full stops.") %>%
        showNotification()
      updateTextInput(session, "crit_id", value = newid)
    }
  })

  # . . criteria_table ----
  output$criteria_table <- renderTable({
    crit <- do.call(rbind, criteria())
    if (is.null(crit)) return(data.frame())
    as.data.frame(crit)
  })

  # . . criteria_warning ----
  output$criteria_warning <- renderText({
    s <- my_study()
    if (length(s$analyses) > 0) {
      show("add_criterion")
      ""
    } else {
      hide("add_criterion")
      i18n()$t("Add an analysis first to be able to add criteria.")
    }
  })

  # ana_return_list ----
  output$ana_return_list <- renderTable({
    r <- return_list()
    data.frame(
      name = names(r),
      object = unlist(r)
    )
  })

  # . . add_return ----
  observeEvent(input$add_return, {
    r <- return_list()
    r[[input$ana_return_name]] <- input$ana_return_object
    return_list(r)
  }, ignoreNULL = TRUE)

  # analyses ----

  ## . . add_analysis ----
  observeEvent(input$add_analysis, {
    # add analysis
    if (trimws(input$ana_id) == "") {
      i18n()$t("Analyses require an ID") %>%
        showNotification()
      return(FALSE)
    }

    s <- my_study()

    r <- return_list()
    if (length(r) == 0) r <- ""

    if (trimws(input$ana_code) == "" & r == "") {
      i18n()$t("Analyses require code or a return list of constants") %>%
        showNotification()
      return(FALSE)
    }

    s <- tryCatch({add_analysis(
      s, input$ana_id, input$ana_code,
      return = r, type = "text")},
             error = function(e) {
               js <- sprintf('alert("%s");', e$message)
               runjs(js)
               return(FALSE)
             })

    # reset inputs
    if (!isFALSE(s)) {
      my_study(s)

      updateTextInput(session, "ana_id", value = "")
      updateTextAreaInput(session, "ana_code", value = "")
      updateTextInput(session, "ana_return_name", value = "")
      updateTextInput(session, "ana_return_object", value = "")
      return_list(list())
    }
  }, ignoreNULL = TRUE)

  ## . . analyses_list ----
  output$analyses_list <- renderUI({
    s <- my_study()

    # update crit dropdown
    a <- sapply(s$analyses, "[[", "id")
    updateSelectInput(session, "crit_ana_id", choices = a)

    ## update analysis list
    make_section_list(s, "analyses")
  })

  # . . analyses_edit ----
  observeEvent(input$analyses_edit, {
    s <- my_study()
    idx <- as.integer(input$analyses_edit)
    a <- s$analyses[[idx]]

    updateTextInput(session, "ana_id", value = a$id)
    code <- output_custom_code(s, idx)
    updateTextAreaInput(session, "ana_code", value = code)
  }, ignoreNULL = TRUE)

  ## . . analyses_delete ----
  observeEvent(input$analyses_delete, {
    section_delete("analyses", input$analyses_delete)
  }, ignoreNULL = TRUE)

  # data ----

  ## . . add_data ----
  observeEvent(input$add_data, {
    if (is.null(input$dat_id) |
        nrow(loaded_data()) == 0) return(FALSE)

    s <- my_study() %>%
      add_data(input$dat_id, loaded_data())
    my_study(s)

    ## reset add data interface
    loaded_data(data.frame())
    shinyjs::reset("dat_id")
    shinyjs::reset("dat_file")
    output$codebook_json <- renderText("")
  }, ignoreNULL = TRUE)

  # . . dat_file ----
  observeEvent(input$dat_file, {
    req(input$dat_file)

    if (input$dat_id == "") {
      input$dat_file$datapath %>%
        tools::file_ext() %>%
        paste0("." , .) %>%
        gsub("", input$dat_file$name) %>%
        updateTextInput(session, "dat_id", value = .)
    }

    d <- rio::import(input$dat_file$datapath)
    loaded_data(d)

    output$codebook_json <- renderText({
      codebook(d, name = input$dat_id)
    })

    output$var_list <- renderUI({
      varnames <- names(loaded_data())
      if (length(varnames)==0) return("")

      paste0("<button>", varnames, "</button>") %>%
        paste(collapse = "\n    ") %>%
        HTML()
    })
  })

  # . . dat_table ----
  output$dat_table <- renderDataTable({
    loaded_data()
  })

  # . . data_list ----
  output$data_list <- renderUI({
    make_section_list(my_study(), "data")
  })

  # . . data_edit ----
  observeEvent(input$data_edit, {
    s <- my_study()
    idx <- as.integer(input$data_edit)
    d <- s$data[[idx]]

    updateTextInput(session, "dat_id", value = d$id)
    loaded_data(d$data)
    shinyjs::reset("dat_file")
    output$codebook_json <- renderText({
      get_codebook(s, data_id = idx, as_json = TRUE)
    })

  }, ignoreNULL = TRUE)

  # . . data_delete ----
  observeEvent(input$data_delete, {
    section_delete("data", input$data_delete)
  }, ignoreNULL = TRUE)

  # . . var_selected ----
  observeEvent(input$var_selected, {
    cb <- get_codebook(my_study(), input$dat_id)
    vm <- cb$variableMeasured
    names(vm) <- nm <- sapply(vm, "[[", "name")
    v <- vm[[input$var_selected]]

    updateTextInput(session, "var_desc", value = v$description)
    updateSelectInput(session, "var_type", value = v$dataType)
  })

  # . . download_cb ----
  output$download_cb <- downloadHandler(
    filename = function() {
      paste0("data_", input$dat_id, ".json")
    },
    content = function(file) {
      d <- loaded_data()
      j <- codebook(d, name = input$dat_id)
      write(j, file)
    }
  )

  # outputs ----

  # . . json_text  ----
  output$json_text <- renderText({
    my_study() %>% study_to_json()
  })

  # . . download_json ----
  output$download_json <- downloadHandler(
    filename = function() {
      paste0(input$study_name, ".json")
    },
    content = function(file) {
      j <- my_study() %>% study_to_json()
      write(j, file)
    }
  )

  ## . . human_readable ----
  output$human_readable <- renderUI({
    s <- my_study()
    lvl <- 3
    i <- output_info(s, lvl, "html")
    h <- output_hypotheses(s, lvl, "html")
    a <- output_analyses(s, lvl, "html")
    r <- output_results(s, lvl, "html")

    if (length(s$data) == 0) {
      ds <- "No data"
    } else {
      ds <- sapply(s$data, "[[", "id") %>%
        paste("*", . ) %>%
        paste(collapse = "\n")
    }
    d <- sprintf("<h%d>Data</h%d>\n\n%s",
                 lvl, lvl, ds)

    paste(i, h, d, a, r, sep = "\n\n") %>% HTML()
  })

  # demo ----
  update_from_study <- function(study) {
    a <- lapply(study$author, function(x) {
      x$surname <- x$name$surname
      x$given <- x$name$given
      x$name <- NULL
      x
    })
    authors(a)

    # custom info
    ci <- study$info
    ci$description <- NULL
    custom_info(ci)

    # update crit dropdown
    a <- sapply(study$analyses, "[[", "id")
    updateSelectInput(session, "crit_ana_id", choices = a)
  }

  observeEvent(input$demo, {
    s <- kin_demo()
    my_study(s)
    update_from_study(s)

    updateTextInput(session, "study_name", value = s$name)
    updateTextAreaInput(session, "study_description",
                        value = s$info$description)

  })


} # end server()

shinyApp(ui, server)
