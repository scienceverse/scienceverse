## app.R ##
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(reg)
library(dplyr)
library(tidyr)
options("scipen"=10, "digits"=4)
source("R/utils.R")

## Interface Tab Items ----

### study_tab ----
study_tab <- tabItem(
  tabName = "study_tab",
  h3("Study Info"),
  textInput("study_name", "Study Name", "", "100%"),
  textAreaInput("study_description", "Study Description", "", "100%")
)

### hypo_tab ----
hypo_tab <- tabItem(
  tabName = "hypo_tab",
  h3("Hypotheses"),
  textInput("hypo_id", "Hypothesis ID", "Hypothesis 1", "100%"),
  textAreaInput("hypo_description", "Hypothesis Description", "", "100%"),
  selectInput("hypo_evaluation", "Evaluation", c("All criteria must be true" = "&", "At least one criterion must be true" = "|"), "&"),
  h4("Criteria"),

  fluidRow(
    column(width = 4, selectInput("hypo_analysis_id", NULL, c("Analysis 1"))),
    column(width = 3, textInput("hypo_result", NULL, "p_value", NULL, "Result")),
    column(width = 2, selectInput("hypo_operator", NULL, c("<", "=", ">", "!="), "<")),
    column(width = 3, textInput("hypo_comparator", NULL, "0.05", NULL, "Comparator"))
  ),
  actionButton("add_criterion", "Add Criterion", icon("plus")),
  actionButton("add_hypothesis", "Add Hypothesis", icon("plus"))
)

### meth_tab ----
meth_tab <- tabItem(
  tabName = "meth_tab",
  h3("Methods")
)

### data_tab ----
data_tab <- tabItem(
  tabName = "data_tab",
  h3("Data"),
  textInput("data_id", "Data ID", "Data 1", "100%"),
  fileInput("data_file", "Upload Data"),
  tableOutput("data_table")
)

### anal_tab ----
anal_tab <- tabItem(
  tabName = "anal_tab",
  h3("Analyses"),
  textInput("anal_id", "Analysis ID", "Analysis 1", "100%"),
  selectInput("anal_func", "Test Function",
              c("t.test",
                "cor.test",
                "custom")),
  hidden(textAreaInput("anal_code", "Custom Analysis Function", "", "100%")),
  h4("Parameters"),
  DTOutput("param_table"),
  actionButton("add_param", "Add Parameter", icon("plus")),
  actionButton("add_analysis", "Add Analysis", icon("plus"))
)

### json_tab ----
json_tab <- tabItem(
  tabName = "json_tab",
  h3("JSON Text"),
  verbatimTextOutput("json_text")
)


## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Reg"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Study Info", tabName = "study_tab"),
      menuItem("Hypotheses", tabName = "hypo_tab"),
      #menuItem("Methods", tabName = "meth_tab"),
      menuItem("Data", tabName = "data_tab"),
      menuItem("Analysis", tabName = "anal_tab"),
      menuItem("JSON", tabName = "json_tab")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      study_tab,
      hypo_tab,
      #meth_tab,
      data_tab,
      anal_tab,
      json_tab
    )
  )
)

## server ----
server <- function(input, output, session) {
  # myStudy ----
  myStudy <- reactive({
    study <- study(input$study_name,
                   description = input$study_description) %>%
      add_hypothesis(input$hypo_description,
                     input$hypo_evaluation,
                     input$hypo_id) %>%
      add_analysis(input$anal_func,
                   func.params[[input$anal_func]],
                   input$anal_code,
                   input$anal_id) %>%
      add_criterion(input$hypo_result,
                    input$hypo_operator,
                    input$hypo_comparator,
                    input$hypo_id,
                    input$anal_id)

    if (!is.null(input$data_file)) {
      study <- add_data(study, loadedData(), input$data_id)
    }

    study
  })

  # loadedData ----
  loadedData <- reactive({
    req(input$data_file)
    rio::import(input$data_file$datapath)
  })

  ### output$data_table ----
  output$data_table <- renderTable({
    loadedData()
  })

  ### output$json_text  ----
  output$json_text <- renderText({
    myStudy() %>% study_to_json()
  })

  ## func.params ----
  func.params <- list(
    t.test = list(
      x = ".data[1]$x",
      y = ".data[1]$y",
      alternative = "two.sided",
      mu = 0,
      paired = FALSE,
      var.equal = FALSE,
      conf.level = 0.95
    ),
    cor.test = list(
      x = ".data1[1]$x",
      y = ".data[1]$y",
      alternative = "two.sided",
      method = "pearson",
      conf.level = 0.95
    ),
    custom = list(
      data = ".data[1]"
    )
  )

  # trigger to call reactive functions programmatically
  paramTrigger <- makeReactiveTrigger()

  # param_table ----
  param_table <- reactive({
    paramTrigger$depend()

    if (input$anal_func == "custom") {
      show("anal_code")
    } else {
      hide("anal_code")
    }

    func.params[[input$anal_func]] %>%
      as.data.frame() %>% t() %>%
      tibble::as_tibble(rownames = "parameter") %>%
      rename("value" = "V1")
  })

  param_table_proxy <- dataTableProxy('param_table')

  ### output$param_table ----
  output$param_table <- renderDataTable({
    datatable(param_table(), editable = TRUE,
              rownames = T, options = list())
  })

  # param_table_cell_edit ----
  observeEvent(input$param_table_cell_edit, {
    info = input$param_table_cell_edit
    str(info)
    if (info$col == 1) {
      names(func.params[[input$anal_func]])[info$row] <<- info$value
    } else if (info$col == 2) {
      func.params[[input$anal_func]][info$row] <<- info$value
    }
    paramTrigger$trigger() # update param_table()

    replaceData(param_table_proxy, param_table(),
                resetPaging = FALSE)  # important
  })
} # end server()

shinyApp(ui, server)
