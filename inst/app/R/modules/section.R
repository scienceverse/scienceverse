sectionUI <- function(id, title = "Section") {
  ns <- NS(id)

  box(id = ns("box"),
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      solidHeader = TRUE,
      title = title,

      dataTableOutput(ns("table")),
      actionButton(ns("add"), "Add", icon("plus")),
      actionButton(ns("delete"), "Delete", icon("trash")),
      actionButton(ns("clear"), "Clear", icon("times"))
  )
}

sectionServer <- function(id, section, reset_ids = c(),
                          func = function(s, i) { s }) {
  moduleServer(id, function(input, output, session) {
    shinyjs::hide("delete") # on startup
    clear <- makeReactiveTrigger()

    # table ----
    output$table <- renderDT({
      debug_msg(id, "-table")

      clear$depend()

      s <- do.call(my_study, list(), envir = session)

      x <- s[[section]]
      ids <- sapply(section, `[[`, "id")
      data.frame(id = ids)
    }, selection = "single",
       rownames = FALSE,
       options = dt_options
    )

    observeEvent(input$add, {
      debug_msg(id, "-add")

      s <- do.call(my_study, list(), envir = session) %>%
      s <- func(s, NULL)
      do.call(my_study, list(s), envir = session)

      shinyjs::click("clear")
    }, ignoreNULL = TRUE)

    # clear ----
    observeEvent(input$clear, {
      debug_msg(id, "-clear")

      lapply(reset_ids, shinyjs::reset)

      cinfo <- section %>%
        substr(1, 3) %>%
        paste0("_cinfo") %>%
        get0()

      cinfo$info(NULL)

      shinyjs::hide("delete")
      updateActionButton(session, "add", i18n()$t("Add"))

      clear$trigger() # fix interface jitter
    })

    ## edit ----
    observeEvent(input$table_rows_selected, {
      debug_msg(id, "-edit")
      idx <- input$table_rows_selected
      if (length(idx) == 0) {
        shinyjs::click("clear")
        return()
      }
      shinyjs::show("delete")

      s <- do.call(my_study, list(), envir = session)
      x <- s[[section]][[idx]]

      ## custom function for each section

      updateActionButton(session, "add", i18n()$t("Update"))
    })

    ## delete ----
    observeEvent(input$delete, {
      debug_msg(id, "-delete")

      idx <- input$table_rows_selected[1]
      if (length(idx) == 0) return()

      s <- do.call(my_study, list(), envir = session)
      s[[section]][[idx]] <- NULL
      do.call(my_study, list(s), envir = session)
      shinyjs::click("clear")
    }, ignoreNULL = TRUE)

  })
}
