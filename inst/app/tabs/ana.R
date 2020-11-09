### ana_tab ----
ana_tab <- tabItem(
  tabName = "ana_tab",

  dataTableOutput("ana_table"),
  actionButton("ana_add", "Add Analysis", icon("plus")),
  actionButton("ana_delete", "Delete", icon("trash")),
  actionButton("ana_clear", "Clear", icon("times")),
  actionButton("study_analyse", "Run All Analyses", icon("code"),
               style = "float: right;"),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      fluidRow(
        column(width = 6,  textInput("ana_id", "Analysis ID", "", "100%")),
        column(width = 6, selectInput("ana_lang", "Code Language", c(
          "R" = "R", "Other Script" = "other",
          "Constant Values" = "constant"
        )))
      ),
      selectizeInput("ana_type", "Analysis Type", c("data cleaning/prep", "exclusions", "descriptives", "confirmatory analysis", "exploratory analysis", "other"), multiple = TRUE)
  ),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Analysis Code", id = "ana_code-box",
      p("Code should return a named list so that you can access these values by name in the hypothesis criteria. Currently, this app only supports R code, so other languages will be saved as custom values."),
      textAreaInput("ana_code", NULL, "", "100%")
  ),
  box(width = 12, collapsible = TRUE, collapsed = TRUE,
      title = "Constant Values", id = "ana_constant-box",
      p("You can add constant values here instead of code above (e.g., data from an archive paper or another software program). They will be translated to R code."),
      fluidRow(
        column(width = 4, textInput("ana_return_name", NULL, "", placeholder = "name")),
        column(width = 4, textInput("ana_return_object", NULL, "", placeholder = "object/value")),
        column(width = 4, actionButton("add_return", "Add Return Value"))
      ),
      tableOutput("ana_return_list")
  ),
  cinfo("ana_info"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Results", id = "ana_results-box",
      uiOutput("ana_results")
  )
)
