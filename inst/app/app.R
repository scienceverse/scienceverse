## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(scienceverse)
library(dplyr)
library(tidyr)
library(shiny.i18n)
options("scipen" = 10,
        "digits" = 4,
        "DT.autoHideNavigation" = TRUE)
source("R/utils.R")
source("R/demo.R")
source("i18n/trans.R")

## Interface Tab Items ----
source("tabs/study.R")
source("tabs/hyp.R")
source("tabs/ana.R")
source("tabs/dat.r")
source("tabs/others.R") # met, aut, output


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
    actionButton("reset_study", "Reset Study"),

    selectInput("lang", "Change language",
                choices = c(English = "en", Dutch = "nl"),
                selected = "en"),
    p("Most of the phrases have not been translated; this is just a proof of concept.", style="margin: 0 1em;")
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


## server ----
server <- function(input, output, session) {
  ## translation ----

  ## . . create translator ----
  i18n <- reactive({
    selected <- input$lang
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # . . on language change ----
  observeEvent(input$lang, {
    # text changes (h3, h4, p)
    for (h in trans_text) {
      suppressWarnings(tt <- i18n()$t(h))

      js <- sprintf("$('*[en=\"%s\"]').text(\"%s\");",
                    gsub("'", "\\\\'", h), tt)
      shinyjs::runjs(js)
    }

    # input label changes
    for (func in names(trans_labels)) {
      for (nm in names(trans_labels[[func]])) {
        l <- trans_labels[[func]][[nm]]
        tl <- suppressWarnings(
          i18n()$t(l)
        )

        args <- list(
          session = session,
          inputId = nm,
          label = tl
        )
        do.call(func, args)
      }
    }
  })

  # constants ----
  dt_options <- list(
    info = FALSE,
    lengthChange = FALSE,
    paging = FALSE,
    ordering = FALSE,
    searching = FALSE,
    pageLength = 500
  )

  # reactive Vals ----
  hyp_clear <- makeReactiveTrigger()
  dat_clear <- makeReactiveTrigger()
  ana_clear <- makeReactiveTrigger()
  my_study <- reactiveVal( study(name = "", description = "") )
  criteria <- reactiveVal(list())
  return_list <- reactiveVal(list())
  authors <- reactiveVal(list())
  author_info <- reactiveVal(list())
  loaded_data <- reactiveVal(data.frame())
  custom_info <- reactiveVal(list())
  cb <- reactiveVal(codebook(data.frame(), "", return = "list"))
  level_list <- reactiveVal(list())
  sim <- reactiveValues(
    w_cells = c("y"),
    b_cells = c("y"),
    cell_names = c("y"),
    within = list(),
    between = list(),
    dv = list(y = "value"),
    id = list(id = "id"),
    n = 100,
    mu = 0,
    sd = 1,
    r = 0,
    vardesc = list(description = list())
  )

  # on startup ----
  shinyjs::hide("hyp_delete")
  shinyjs::hide("dat_delete")
  shinyjs::hide("ana_delete")
  shinyjs::hide("study_analyse")

  # functions ----
  section_delete <- function(section, idx) {
    s <- my_study()
    idx <- as.integer(idx)
    s[[section]][idx] <- NULL
    my_study(s)
  }

  # disable/enable buttons depending on presence of inputs
  buttonable <- function(button, ...) {
    blanks <- list(...) %>% trimws() == ""

    if (any(blanks)) {
      shinyjs::disable(button)
    } else {
      shinyjs::enable(button)
    }
  }

  # actions ----
  # . . reset_study ----
  observeEvent(input$reset_study, {
    # reset all IDs from trans_labels
    trans_labels %>%
      unname() %>%
      unlist() %>%
      names() %>%
      lapply(shinyjs::reset)

    # not sure why these aren't captured above
    c("author_info_name", "author_info_value",
      "ana_return_name", "ana_return_object") %>%
      lapply(shinyjs::reset)

    criteria(list())
    return_list(list())
    authors(list())
    author_info(list())
    loaded_data(data.frame())
    custom_info (list())
    cb(codebook(data.frame(), "", return = "list"))
    shinyjs::click("sim_clear")
    shinyjs::hide("study_analyse")

    s <- study(name = input$study_name,
               description = input$study_description)
    my_study(s)
  })

  # . . study_analyse ----
  observeEvent(input$study_analyse, {
    s <- my_study()

    if (length(s$analyses) == 0) {
      i18n()$t("No analyses have been specified") %>%
        shinyjs::alert()
    } else {
      tryCatch(s <- study_analyse(s),
               error = function(e) {
                 shinyjs::alert(e$message)
      })
      updateTabItems(session, "tabs", "ana_tab")
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

  # . . custom_info_add ----
  observe({
    buttonable("custom_info_add",
               input$custom_info_value,
               input$custom_info_name)
  })

  observeEvent(input$custom_info_add, {
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
    o <- tryCatch({
      get_orcid(input$surname, input$given)
    }, error = function(e) {
      return(NULL)
    })

    n <- length(o)
    if (n == 1) {
      updateTextInput(session, "orcid", value = o)
    } else {
      i18n()$t("%d ORCiDs found") %>%
        sprintf(n) %>%
        shinyjs::alert()
    }
  })

  # . . aut_add ----
  observe({
    buttonable("aut_add",
               input$surname,
               input$given)
  })

  observeEvent(input$aut_add, {
    problems <- FALSE

    # check orcid
    if (input$orcid != "") {
      orcid <- check_orcid(input$orcid)
      if (isFALSE(orcid)) {
        problems <- TRUE
        updateTextInput(session, "orcid",
                        label = "ORCiD is not valid" %>% i18n()$t())
        shinyjs::addClass("orcid", "warning")
      }
    } else {
      orcid <- ""
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
      a <- c(a, author_info())
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
      author_info(list())
      shinyjs::reset("author_info_name")
      shinyjs::reset("author_info_value")
      shinyjs::removeClass("given", "warning")
      shinyjs::removeClass("surname", "warning")
      shinyjs::removeClass("orcid", "warning")
    }
  }, ignoreNULL = TRUE)

  # . . add_author ----
  observe({
    # update study$authors whenever authors() changes
    s <- my_study()
    s$authors <- list()
    for (a in authors()) {
      s <- tryCatch(
        do.call(add_author, c(list(study = s), a)),
        error = function(e) { message(e); return(s) }
      )
    }
    my_study(s)
  })

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
        shinyjs::alert()
    } else {
      a <- authors()
      authors(a[ord])
    }
  }, ignoreNULL = TRUE)


  # . . author_info_add ----
  observe({
    buttonable("author_info_add",
               input$author_info_value,
               input$author_info_name)
  })

  observeEvent(input$author_info_add, {
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

  # . . hyp_add ----
  observe({
    buttonable("hyp_add",
               input$hyp_id,
               input$hyp_desc)
  })

  observeEvent(input$hyp_add, {
    s <- my_study() %>%
      add_hypothesis(input$hyp_id,
                     input$hyp_desc)

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

    # add evaluations if they exist
    if (trimws(input$eval_cor_eval) != "") {
      s <- tryCatch({
        add_eval(s, "corroboration", input$eval_cor_eval,
                 "", input$hyp_id)
        },
        message = function(e) {
          shinyjs::alert(e$message)
        },
        warning = function(e) {
          shinyjs::alert(e$message)
        },
        error = function(e) {
          shinyjs::alert(e$message)
          return(FALSE)
        }
      )
      if (isFALSE(s)) return(FALSE)
    }
    if (trimws(input$eval_fal_eval) != "") {
      s <- tryCatch({
        add_eval(s, "falsification", input$eval_fal_eval,
                 "", input$hyp_id)
        },
        message = function(e) {
          shinyjs::alert(e$message)
        },
        warning = function(e) {
          shinyjs::alert(e$message)
        },
        error = function(e) {
          shinyjs::alert(e$message)
          return(FALSE)
        }
      )
      if (isFALSE(s)) return(FALSE)
    }

    my_study(s)

    # reset for new hypothesis
    shinyjs::click("hyp_clear")
  }, ignoreNULL = TRUE)

  # . . hyp_table ----
  output$hyp_table <- renderDT({
    hyp_clear$depend()
    h <- my_study()$hypotheses
    make_hyp_list(h)
  },  escape = 1:4,
      selection = "single",
      style = 'bootstrap',
      class = 'table-bordered table-condensed',
      rownames = FALSE,
      options = dt_options
  )

  # . . hyp_clear ----
  observeEvent(input$hyp_clear, {
    c("hyp_id", "hyp_desc", "crit_id", "crit_ana_id",
      "crit_result", "crit_operator", "crit_comparator",
      "eval_cor_eval", "eval_fal_eval") %>%
      lapply(shinyjs::reset)

    shinyjs::hide("hyp_delete")
    updateActionButton(session, "hyp_add", i18n()$t("Add Hypothesis"))
    criteria(list())

    hyp_clear$trigger() # fix interface jitter
  })

  ## . . hyp_edit ----
  observeEvent(input$hyp_table_rows_selected, {
    idx <- input$hyp_table_rows_selected
    if (length(idx) == 0) {
      shinyjs::click("hyp_clear")
      return()
    }
    shinyjs::show("hyp_delete")

    s <- my_study()
    h <- s$hypotheses[[idx]]

    crit <- h$criteria
    names(crit) <- sapply(crit, `[[`, "id")
    criteria(crit)

    updateActionButton(session, "hyp_add", i18n()$t("Update Hypothesis"))
    updateTextInput(session, "hyp_id", value = h$id)
    updateTextAreaInput(session, "hyp_desc",
                        value = h$description)
    updateTextInput(session, "eval_cor_eval",
                    value = h$corroboration$evaluation)
    updateTextInput(session, "eval_fal_eval",
                    value = h$falsification$evaluation)

  })

  ## . . hyp_delete ----
  observeEvent(input$hyp_delete, {
    idx <- input$hyp_table_rows_selected[1]
    if (length(idx) == 0) return()

    s <- my_study()
    s$hypotheses[[idx]] <- NULL
    my_study(s)
    shinyjs::click("hyp_clear")
  }, ignoreNULL = TRUE)

  # critera ----

  # . . crit_add ----
  observe({
    buttonable("crit_add",
               input$crit_id,
               input$crit_result,
               input$crit_comparator,
               input$crit_ana_id)
  })

  observeEvent(input$crit_add, {
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
    c("crit_id", "crit_result", "crit_comparator") %>%
      lapply(shinyjs::reset)
  }, ignoreNULL = TRUE)

  # . . crit_delete ----
  observeEvent(input$crit_delete, {
    crit <- criteria()
    idx <- as.integer(input$crit_delete)
    crit[idx] <- NULL
    criteria(crit)
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

  ## . . edit selected criterion ----
  observeEvent(input$crit_table_rows_selected, {
    crit <- criteria()
    cc <- crit[[input$crit_table_rows_selected]]
    updateTextInput(session, "crit_id", value = cc$id)
    updateSelectInput(session, "crit_ana_id", selected = cc$analysis_id)
    updateTextInput(session, "crit_result", value = cc$result)
    updateSelectInput(session, "crit_operator", selected = cc$operator)
    updateTextInput(session, "crit_comparator", value = cc$comparator)
  })

  # . . crit_table ----
  output$crit_table <- renderDT({
    make_crit_list(criteria())
  },  escape = 1:5,
      selection = "single",
      style = 'bootstrap',
      class = 'table-bordered table-condensed',
      rownames = FALSE,
      options = dt_options
  )

  # . . criteria_warning ----
  output$criteria_warning <- renderText({
    s <- my_study()
    if (length(s$analyses) > 0) {
      show("crit_add")
      ""
    } else {
      hide("crit_add")
      i18n()$t("Add an analysis first to be able to add criteria.")
    }
  })

  # . . ana_return_list ----
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

  ## . . ana_add ----
  observe({
    # either return_list or ana_code need to be present
    r <- ifelse(return_list() %>% length == 0, "", "r")
    code <- paste0(input$ana_code, r)
    buttonable("ana_add", input$ana_id, code)
  })
  observeEvent(input$ana_add, {
    # add analysis
    s <- my_study()
    r <- return_list()
    if (length(r) == 0) r <- ""

    s <- tryCatch({add_analysis(
      s, input$ana_id, input$ana_code,
      return = r, type = "text")},
             error = function(e) {
               shinyjs::alert(e$message)
               return(FALSE)
             })

    # reset inputs
    if (!isFALSE(s)) {
      my_study(s)
      shinyjs::show("study_analyse")
      shinyjs::click("ana_clear")
    }
  }, ignoreNULL = TRUE)

  ## . . ana_clear ----
  observeEvent(input$ana_clear, {
    c("ana_id", "ana_code", "ana_return_name", "ana_return_object") %>%
      lapply(shinyjs::reset)

    shinyjs::hide("ana_delete")
    updateActionButton(session, "ana_add", i18n()$t("Add Analysis"))

    return_list(list())

    ana_clear$trigger() # fix interface jitter
  })

  # . . ana_table ----
  output$ana_table <- renderDT({
    ana_clear$depend()
    make_ana_list(my_study())
  },  escape = FALSE,
      selection = "single",
      style = 'bootstrap',
      class = 'table-bordered table-condensed',
      rownames = FALSE,
      options = dt_options
  )


  ## . . analyses_list ----
  output$analyses_list <- renderUI({
    s <- my_study()

    # update crit dropdown
    a <- sapply(s$analyses, "[[", "id")
    updateSelectInput(session, "crit_ana_id", choices = a)

    ## update analysis list
    make_section_list(s, "analyses")
  })

  ## . . ana_edit ----
  observeEvent(input$ana_table_rows_selected, {
    idx <- input$ana_table_rows_selected
    if (length(idx) == 0) {
      shinyjs::click("ana_clear")
      return()
    }
    shinyjs::show("ana_delete")

    s <- my_study()
    a <- s$analyses[[idx]]

    updateTextInput(session, "ana_id", value = a$id)
    code <- output_custom_code(s, idx)
    updateTextAreaInput(session, "ana_code", value = code)
    updateActionButton(session, "ana_add", i18n()$t("Update Analysis"))
  })

  ## . . ana_results ----
  output$ana_results <- renderUI({
    if (is_nowt(input$ana_id)) return()

    s <- my_study()
    a_ids <- sapply(s$analyses, `[[`, "id")
    idx <- match(input$ana_id, a_ids)

    s$analyses[[idx]]$results %>%
      nested_list() %>%
      markdown::renderMarkdown(text = .) %>%
      HTML()
  })

  ## . . ana_delete ----
  observeEvent(input$ana_delete, {
    idx <- input$ana_table_rows_selected[1]
    if (length(idx) == 0) return()

    s <- my_study()
    s$analyses[[idx]] <- NULL
    if (length(s$analyses) == 0) {
      shinyjs::hide("study_analyse")
    }
    my_study(s)
    shinyjs::click("ana_clear")
  }, ignoreNULL = TRUE)

  # data ----

  ## . . dat_add ----
  observe({
    buttonable("dat_add",
               input$dat_id)
  })
  observeEvent(input$dat_add, {
    data <- loaded_data()
    nm <- trimws(input$dat_id)
    if (nm == "" | nrow(data) == 0) return(FALSE)

    s <- my_study() %>%
      add_data(nm, data, design = attr(data, "design"),
               description = input$data_desc)

    ## add codebook
    idx <- match(input$dat_id, sapply(s$data, "[[", "id"))[1]
    if (!is.na(idx)) s$data[[idx]]$codebook <- cb()

    my_study(s)

    ## reset add data interface
    shinyjs::click("dat_clear")
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
    cb(codebook(d, name = input$dat_id, return = "list"))
  })

  # . . update cb name ----
  observe({
    cb <- cb()
    if (length(cb)) {
      cb$name <- input$dat_id
      cb(cb)
    }
  })

  # . . data_table ----
  output$data_table <- renderDT(
    loaded_data(), rownames = FALSE,
    style = 'bootstrap',
    class = 'table-bordered table-condensed'
  )

  # . . dat_table ----
  output$dat_table <- renderDT({
    dat_clear$depend()
    d <- my_study()$data
    make_dat_list(d)
  },  escape = TRUE,
      selection = "single",
      style = 'bootstrap',
      class = 'table-bordered table-condensed',
      rownames = FALSE,
      options = dt_options
  )

  # . . dat_edit ----
  observeEvent(input$dat_table_rows_selected, {
    idx <- input$dat_table_rows_selected
    if (length(idx) == 0) {
      shinyjs::click("dat_clear")
      return()
    }

    s <- my_study()
    d <- s$data[[idx]]
    desc <- if_nowt(d$description)

    updateTextInput(session, "dat_id", value = d$id)
    updateTextAreaInput(session, "dat_desc", value = desc)
    loaded_data(d$data)
    cb(get_codebook(s, data_id = idx))

    updateActionButton(session, "dat_add", i18n()$t("Update Data"))
    shinyjs::show("dat_delete")
  }, ignoreNULL = TRUE)

  # . . codebook_json ----
  output$codebook_json <- renderText({
    cb() %>%
      jsonlite::toJSON(auto_unbox = TRUE) %>%
      jsonlite::prettify(4)
  })

  ## . . dat_delete ----
  observeEvent(input$dat_delete, {
    idx <- input$dat_table_rows_selected[1]
    if (length(idx) == 0) return()

    s <- my_study()
    s$data[[idx]] <- NULL
    my_study(s)
    shinyjs::click("dat_clear")
  }, ignoreNULL = TRUE)

  # . . dat_clear ----
  observeEvent(input$dat_clear, {
    shinyjs::hide("dat_delete")
    updateActionButton(session, "dat_add", i18n()$t("Add Data"))
    loaded_data(data.frame())
    cb(codebook(loaded_data(), "", return = "list"))
    c("dat_id", "dat_desc", "dat_file", "var_name", "var_desc", "var_type") %>%
      lapply(shinyjs::reset)

    dat_clear$trigger() # fix interface jitter
  }, ignoreNULL = TRUE)

  # . . cb() ----
  reactive({
    cb <- tryCatch({
      get_codebook(my_study(), input$dat_id)
    }, error = function(e) {
      message("caught cb():\n", e)
      return(codebook(loaded_data(),
                      input$dat_id,
                      return = "list"))
    })

    cb(cb)
  })

  # . . var_list ----
  output$var_list <- renderUI({
    varnames <- names(loaded_data())
    if (length(varnames)==0) return("")

    paste0("<button>", varnames, "</button>") %>%
      paste(collapse = "\n    ") %>%
      HTML()
  })

  # . . var_selected ----
  observeEvent(input$var_selected, {
    v <- tryCatch({
        vm <- cb()$variableMeasured
        names(vm) <- sapply(vm, "[[", "name")
        vm[[input$var_selected]]
      },
      error = function(e) {
        message("caught var_selected v:\n", e)
        return(list(name = "",
                    description = "",
                    dataType = "string"))
      }
    )

    updateTextInput(session, "var_name",
                    value = v$name)
    updateTextInput(session, "var_desc",
                    value = v$description)
    updateSelectInput(session, "var_type",
                      selected = v$dataType)
  })

  # . . var_update ----
  observeEvent(input$var_update, {
    cb <- cb()
    vm <- cb$variableMeasured

    if (length(vm)) {
      nm <- sapply(vm, "[[", "name")
      idx <- match(input$var_name, nm)
      vm[[idx]]["description"] <- input$var_desc
      vm[[idx]]["dataType"] <- input$var_type
      cb$variableMeasured <- vm
      cb(cb)
    }
  })

  # . . download_data ----
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("data_", input$dat_id, ".csv")
    },
    content = function(file) {
      readr::write_csv(loaded_data(), file)
    }
  )

  # . . download_cb ----
  output$download_cb <- downloadHandler(
    filename = function() {
      paste0("data_", input$dat_id, ".json")
    },
    content = function(file) {
      cb() %>%
        jsonlite::toJSON(auto_unbox = TRUE) %>%
        jsonlite::prettify(4) %>%
        write(file)
    }
  )

  # sim_data ----
  observeEvent(input$sim_data, {
    d <- tryCatch(faux::sim_design(
      design = sim$design,
      empirical = (input$empirical == "TRUE"),
      long = (input$long == "TRUE")
    ), error = function(e) {
      message(e)
      #showNotification(e)
      return(FALSE)
    })

    if (isFALSE(d)) return()

    #des <- attr(d, "design")
    loaded_data(d)

    cb <- tryCatch({
      codebook(d, vardesc = sim$vardesc, return = "list")
    }, error = function(e) {
      return(list())
    })
    cb(cb)

  }, ignoreNULL = TRUE)

  ## . . sim_clear ----
  observeEvent(input$sim_clear, {
    c("dv_name", "dv_def", "id_name", "id_def",
    "n", "mu", "sd", "r",
    "factor_name", "factor_desc", "factor_levels") %>%
      lapply(shinyjs::reset)

    sim$within <- list()
    sim$between = list()
    sim$vardesc = list(description = list())
    level_list(list("A1" = "A1 Description",
                    "A2" = "A2 Description"))
  }, ignoreNULL = TRUE)

  ## . . sim_demo ----
  observeEvent(input$sim_demo, {
    loaded_data(data.frame())
    updateTextInput(session, "n", value = "50, 30")
    updateTextInput(session, "mu", value = "10, 15, 15, 20")
    updateTextInput(session, "sd", value = "5")
    updateTextInput(session, "r", value = "0.5")
    updateTextInput(session, "dv_name", value = "score")
    updateTextInput(session, "dv_desc", value = "Test Score")
    updateTextInput(session, "id_name", value = "id")
    updateTextInput(session, "id_desc", value = "Pet ID")

    sim$between <- list(pet = c(cat = "Kitty!", dog = "Woof!"))
    sim$within  <- list(time = c(am = "morning", pm = "night"))
    sim$vardesc <- list(description = list(pet = "Type of Pet",
                                           time = "Time of Day"))
  }, ignoreNULL = TRUE)

  # . . dv ----
  observe({
    dv <- list(input$dv_def)
    names(dv) <- input$dv_name
    sim$dv <- dv
  })

  # . . id ----
  observe({
    id <- list(input$id_def)
    names(id) <- input$id_name
    sim$id <- id
  })

  # . . b_cells ----
  observe({
    sim$b_cells <- faux:::cell_combos(sim$between, names(sim$dv))
  })

  # . . w_cells ----
  observe({
    sim$w_cells <- faux:::cell_combos(sim$within, names(sim$dv))
    if (length(sim$w_cells) > 1) {
      shinyjs::show("r")
    } else {
      shinyjs::hide("r")
    }
  })

  # . . cell_names ----
  observe({
    # calculate cell combo names
    lb <- length(sim$b_cells)
    lw <- length(sim$w_cells)
    lr <- lw*(lw-1)/2

    if (lb == 1) {
      sim$cell_names <- sim$w_cells
    } else if (lw == 1) {
      sim$cell_names <- sim$b_cells
    } else {
      sim$cell_names <- list(sim$b_cells, sim$w_cells) %>%
        rev() %>%
        do.call(expand.grid, .) %>%
        dplyr::mutate(nm = paste0(Var1, "_", Var2)) %>%
        dplyr::pull(nm)
    }

    ## n
    lab <- paste0("n (", lb, ")")
    updateTextInput(session, "n", lab)

    ## mu
    lab <- paste0("mu (", lb*lw, ")")
    updateTextInput(session, "mu", lab)

    ## sd
    lab <- paste0("sd (", lb*lw, ")")
    updateTextInput(session, "sd", lab)

    ## r
    lab <- paste0("r (", lr, ")")
    updateTextInput(session, "r", lab)
  })

  # . . n ----
  observeEvent(c(input$n, sim$b_cells), {
    spl <- strsplit(input$n, "(,|;| )")[[1]] %>%
      `[`(., grep(".+", .)) # get rid of blanks
    comp <- as.integer(spl)
    if (length(comp) == 0) {
      shinyjs::addClass("n", "warning")
      return()
    }

    if (isTRUE(all(comp == spl))) {
      sim$n <- comp
      lb <- length(sim$b_cells)
      if (length(sim$n) == 1) sim$n <- rep(sim$n, lb)
      if (lb == length(sim$n)) {
        shinyjs::removeClass("n", "warning")
      } else {
        shinyjs::addClass("n", "warning")
        # trim or extend to length = lb
        sim$n <- sim$n[1:lb]
        if (length(is.na(sim$n)) > 0 ) {
          # replace NAs with existing values
          suppressWarnings(
            sim$n[is.na(sim$n)] <- sim$n[!is.na(sim$n)]
          )
        }
      }
      names(sim$n) <- sim$b_cells
      if (length(sim$n) == 1) names(sim$n) <- NULL
    } else {
      "All inputs for n must be integers" %>%
        i18n()$t() %>%
        shinyjs::alert()
      shinyjs::addClass("n", "warning")
    }
  })

  # . . mu ----
  observeEvent(c(input$mu, sim$cell_names), {
    spl <- strsplit(input$mu, "(,|;| )")[[1]] %>%
      `[`(., grep(".+", .)) %>% # get rid of blanks
      `[`(., grep("[^-|\\.]$", .)) # remove "-", ".", "-."
    comp <- as.numeric(spl)
    if (length(comp) == 0) {
      shinyjs::addClass("mu", "warning")
      return()
    }

    if (isTRUE(all(!is.na(comp)))) {
      sim$mu <- comp
      lc <- length(sim$cell_names)
      if (length(sim$mu) == 1) sim$mu <- rep(sim$mu, lc)
      if (lc == length(sim$mu)) {
        shinyjs::removeClass("mu", "warning")
      } else {
        shinyjs::addClass("mu", "warning")
        # trim or extend to length = lc
        sim$mu <- sim$mu[1:lc]
        if (length(is.na(sim$mu)) > 0 ) {
          # replace NAs with existing values
          suppressWarnings(
            sim$mu[is.na(sim$mu)] <- sim$mu[!is.na(sim$mu)]
          )
        }
      }
      names(sim$mu) <- sim$cell_names
      if (length(sim$mu) == 1) names(sim$mu) <- NULL
    } else {
      "All inputs for mu must be numbers" %>%
        i18n()$t() %>%
        shinyjs::alert()
      shinyjs::addClass("mu", "warning")
    }
  })

  # . . sd ----
  observeEvent(c(input$sd, sim$cell_names), {
    spl <- strsplit(input$sd, "(,|;| )")[[1]] %>%
      `[`(., grep("-?\\.?.+", .)) %>% # get rid of blanks
      `[`(., grep("[^\\.]$", .)) # remove "."
    comp <- as.numeric(spl)
    if (length(comp) == 0) {
      shinyjs::addClass("sd", "warning")
      return()
    }

    if (isTRUE(all(!is.na(comp))) &
        isTRUE(all(comp > 0))) {
      sim$sd <- comp
      lc <- length(sim$cell_names)
      if (length(sim$sd) == 1) sim$sd <- rep(sim$sd, lc)
      if (lc == length(sim$sd)) {
        shinyjs::removeClass("sd", "warning")
      } else {
        shinyjs::addClass("sd", "warning")
        # trim or extend to length = lc
        sim$sd <- sim$sd[1:lc]
        if (length(is.na(sim$sd)) > 0 ) {
          # replace NAs with existing values
          suppressWarnings(
            sim$sd[is.na(sim$sd)] <- sim$sd[!is.na(sim$sd)]
          )
        }
      }
      names(sim$sd) <- sim$cell_names
      if (length(sim$sd) == 1) names(sim$sd) <- NULL
    } else {
      "All inputs for sd must be positive numbers" %>%
        i18n()$t() %>%
        shinyjs::alert()
      shinyjs::addClass("sd", "warning")
    }
  })

  # . . r ----
  observeEvent(c(input$r, sim$w_cells, sim$b_cells), {
    # return if no within factors
    if (length(sim$w_cells) < 2) return()

    spl <- strsplit(input$r, "(,|;| )")[[1]] %>%
      `[`(., grep(".+", .)) %>% # get rid of blanks
      `[`(., grep("[^-|\\.]$", .)) # remove "-", ".", "-."
    comp <- as.numeric(spl)
    if (length(comp) == 0) {
      shinyjs::addClass("r", "warning")
      return()
    }

    # how many for upper right triangle
    r_pairs <- faux:::unique_pairs(sim$w_cells)
    lb <- length(sim$b_cells)
    lr <- length(r_pairs)

    if (isTRUE(all(!is.na(comp))) &
        isTRUE(all(abs(comp) <= 1))) {
      shinyjs::removeClass("r", "warning")

      # only 1 number, repeat lr times
      if (length(comp) == 1) comp <- rep(comp, lr)

      # <lr numbers, extend to length = lr
      if (length(comp) < lr) {
        shinyjs::addClass("r", "warning")
        comp <- comp[1:lr]
        comp[is.na(comp)] <- 0
      }

      # lr numbers, repeat for each between cell
      if (length(comp) == lr) {
        comp <- rep(comp, lb)
      }

      # !lr*lb numbers, trim or extend to length = lr*lb
      if (length(comp) != lr*lb) {
        shinyjs::addClass("r", "warning")
        comp <- comp[1:(lr*lb)]
        if (length(is.na(comp)) > 0) {
          comp[is.na(comp)] <- 0
        }
      }

      # set sim$r from comp (now lr*lb-length vector)
      sim$r <- matrix(comp, nrow = lr) %>%
        as.data.frame() %>%
        as.list() %>%
        lapply(function(x) { names(x) <- r_pairs; x })
      names(sim$r) <- sim$b_cells

      message("r:\n", nested_list(sim$r))
    } else {
      "All inputs for r must be numbers between -1 and 1" %>%
        i18n()$t() %>%
        shinyjs::alert()
      shinyjs::addClass("r", "warning")
    }
  })

  # . . factor_levels ----
  observeEvent(input$factor_levels, {
    ll <- level_list()

    if (length(ll) < input$factor_levels) {
      start <- length(ll) + 1
      end <- input$factor_levels
      for (i in start:end) {
        nm <- paste0(input$factor_name, i)
        ll[nm] <- paste(input$factor_name, i, "Description")
      }
    }
    # truncate if needed
    ll <- ll[1:input$factor_levels]
    level_list(ll)
  })

  observeEvent(input$factor_name, {
    if (input$factor_name == "") {
      hide("level_list_display")
    } else {
      # change default-looking names
      ll <- level_list()
      prefixes <- gsub("\\_?\\d+$", "", names(ll))
      descs <- gsub("\\_?\\d+ Description$", "", ll)
      if (unique(prefixes) %>% length() == 1 &
          unique(descs) %>% length() == 1) {
        n <- length(ll)
        ll <- paste0(input$factor_name, 1:n, " Description")
        names(ll) <- paste0(input$factor_name, 1:n)
        level_list(ll)
      }
      show("level_list_display")
    }
  })

  # . . level_list_display ----
  output$level_list_display <- renderUI({
    ll <- level_list()

    x <- lapply(1:length(ll), function(i) {
      nm <- paste0("level_name_", i)
      dc <- paste0("level_desc_", i)
      fluidRow(column(3, paste("Level", i)),
               column(3, textInput(nm, NULL, names(ll)[[i]])),
               column(6, textInput(dc, NULL, ll[[i]])))
    })
    x$width = 12

    do.call(box, x)
  })

  # . . level_list_update ----
  observeEvent(input$level_list_update, {
    nm <- names(input)
    lnames <- grep("^level_name_\\d+", nm) %>% `[`(nm, .) %>% sort()
    ldescs <- grep("^level_desc_\\d+", nm) %>% `[`(nm, .)%>% sort()

    ll <- lapply(ldescs, function(x) input[[x]])
    ln <- lapply(lnames, function(x) input[[x]])
    names(ll) <- ln

    # warn about duplicates
    mapply(function(id, dupe) {
      if (dupe) {
        shinyjs::addClass(id, "warning")
      } else {
        shinyjs::removeClass(id, "warning")
      }
    }, lnames, duplicated(ln))

    level_list(ll)
  }, ignoreNULL = TRUE)



  # . . design_summary ----
  output$design_summary <- renderDT({
    sim$design <- tryCatch(faux::check_design(
      within = sim$within,
      between = sim$between,
      n = sim$n,
      mu = sim$mu,
      sd = sim$sd,
      r = sim$r,
      dv = sim$dv,
      id = sim$id,
      vardesc = sim$vardesc,
      plot = FALSE
    ), error = function(e) {
      message(e)
      return(faux::check_design(plot = FALSE))
    })

    w <- names(sim$design$within)
    b <- names(sim$design$between)
    p <- sim$design$params
    # fix for problem with 1-factor params table in faux
    if (names(p)[1] == ".") names(p)[1] <- c(w, b)[1]
    vars <- length(w) + length(b)
    if (vars > 1) {
      # add [w] or [b] to var names
      n <- names(p)[1:vars]
      n[n %in% w] <- paste(n[n %in% w], "[w]")
      n[n %in% b] <- paste(n[n %in% b], "[b]")
      names(p)[1:vars] <- n
    }
    if (length(w) > 0) {
      # add [r] to correlation columns
      a <- vars+1
      b <- vars+length(sim$w_cells)
      n <- names(p)[a:b]
      n <- paste(n, "[r]")
      names(p)[a:b] <- n
    }

    p
  }, style = 'bootstrap',
     class = 'table-bordered table-condensed',
     rownames = FALSE,
     options = dt_options
  )

  # . . factor_add ----
  observe({
    buttonable("factor_add",
               input$factor_name)
  })
  observeEvent(input$factor_add, {
    sim[[input$factor_type]][[input$factor_name]] <- level_list()
    sim$vardesc$description[[input$factor_name]] <-
      if_nowt(input$factor_desc, input$factor_name)

    # reset inputs
    shinyjs::reset("factor_name")
    shinyjs::reset("factor_desc")
    shinyjs::reset("factor_levels")
  }, ignoreNULL = TRUE)

  ## . . sim_plot ----
  output$sim_plot <- renderPlot({
    tryCatch(plot(sim$design),
    error = function(e) {
      message(e)
      return(ggplot())
    })
  })

  # # . . level_table ----
  # observeEvent(input$factor_levels, {
  #   # get current list and remove extra rows
  #   orig <- level_list() %>%
  #     filter(row_number() <= input$factor_levels)
  #
  #   # add extra rows
  #   to_add <- data.frame(
  #     Name = paste0("level_", 1:input$factor_levels),
  #     Description = paste("Level", 1:input$factor_levels)
  #   ) %>%
  #     filter(row_number() > nrow(orig))
  #
  #   bind_rows(orig, to_add) %>% level_list()
  # })
  #
  # output$level_table <- renderDT(
  #   level_list(), editable = TRUE,
  #   server = TRUE, rownames = FALSE
  # )
  #
  # observeEvent(input$level_table_cell_edit, {
  #   level_list() %>%
  #     editData(input$level_table_cell_edit,
  #              'level_table', rownames = FALSE) %>%
  #     level_list()
  # }, ignoreNULL = TRUE)

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

  # . . download_pre ----
  output$download_pre <- downloadHandler(
    filename = function() {
      paste0(input$study_name, "_prereg.html")
    },
    content = function(file) {
      study_save(my_study(), file, format = "prereg")
    }
  )

  # . . download_post ----
  output$download_post <- downloadHandler(
    filename = function() {
      paste0(input$study_name, "_postreg.html")
    },
    content = function(file) {
      study_save(my_study(), file, format = "postreg")
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

  # inputs ----
  # . . load_json ----
  observeEvent(input$load_json, {
    req(input$load_json)

    tryCatch({
      s <- study(input$load_json$datapath)
      update_from_study(s)
    }, error = function(e) {
      shinyjs::alert(e$message)
    })
  })

  # . . update_from_study ----
  update_from_study <- function(study) {
    my_study(study)

    a <- lapply(study$author, function(x) {
      x$surname <- x$name$surname
      x$given <- x$name$given
      x$name <- NULL
      x
    })
    authors(a)

    updateTextInput(session, "study_name",
                    value = study$name)
    updateTextAreaInput(session, "study_description",
                        value = study$info$description)

    # custom info
    ci <- study$info
    ci$description <- NULL
    custom_info(ci)

    # update crit dropdown
    a <- sapply(study$analyses, "[[", "id")
    updateSelectInput(session, "crit_ana_id", choices = a)
    if (length(study$analyses) > 0) {
      shinyjs::show("study_analyse")
    }
  }

  # . . demo ----
  observeEvent(input$demo, {
    s <- kin_demo()
    update_from_study(s)
  })

  save_trans(trans_text, trans_labels)

} # end server()

shinyApp(ui, server)

