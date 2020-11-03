cinfo <- function(id, title = "Custom Info") {
  ns <- NS(id)

  box(id = ns("box"),
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      solidHeader = TRUE,
      title = title,
      dataTableOutput(ns("table")),
      actionButton(ns("add"), "Add", icon("plus")),
      actionButton(ns("delete"), "Delete", icon("trash"))
  )
}

cinfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    info <- reactiveVal(NULL)
    disp <- reactiveVal(NULL)

    # . . info ----
    observeEvent(info(), {
      debug_msg(id, "-info")
      # always update disp when info updates
      if (is.null(info())) {
        data.frame(name = c(), value = c()) %>% info()
      }
      info() %>% disp()
    })

    # . . add ----
    observeEvent(input$add, {
      debug_msg(id, "-add")
      new <- data.frame(name = "[name]", value = "[value]")
      disp() %>%
        dplyr::bind_rows(new) %>%
        info()
    }, ignoreNULL = TRUE)

    # . . table ----
    output$table <- renderDT({
      debug_msg(id, "-table")
      info()
    },  escape = TRUE,
        extensions = "KeyTable",
        callback = DT::JS(table_tab_js),
        editable = TRUE,
        rownames = FALSE,
        options = dt_options
    )

    # . . table_cell_edit ----
    observeEvent(input$table_cell_edit, {
      debug_msg(id, "-table_cell_edit")

      # only update the display table so table doesn't refresh
      cell <- input$table_cell_edit
      ci <- disp()
      ci[cell$row, cell$col+1] <- cell$value
      disp(ci)
    }, ignoreNULL = TRUE)

    ## . . delete ----
    observeEvent(input$delete, {
      debug_msg(id, "-delete")
      idx <- input$table_rows_selected
      if (length(idx) == 0) return()

      disp()[-idx, ] %>% info()
    }, ignoreNULL = TRUE)

    # return value
    list(
      info = info,
      disp = disp
    )
  })
}
