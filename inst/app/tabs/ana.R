### ana_tab ----
ana_tab <- tabItem(
  tabName = "ana_tab",
  h3("Analyses"),
  dataTableOutput("ana_table"),
  actionButton("ana_add", "Add Analysis", icon("plus")),
  actionButton("ana_delete", "Delete", icon("trash")),
  actionButton("ana_clear", "Clear", icon("times")),
  actionButton("study_analyse", "Run All Analyses",
               style = "float: right;"),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      textInput("ana_id", "Analysis ID", "", "100%"),
      textAreaInput("ana_code", "Analysis Code", "", "100%"),
  ),
  box(width = 12, collapsible = TRUE, collapsed = TRUE,
      title = "Return",
      p("If your code doesn't return a named list, add each object to be returned below. E.g., if your code creates two objects named `my_mean` and `my_sd`, you can refer to them in the criteria as `m` and `s` if you set name to `m` and `s` and object to `my_mean` and `my_sd`. You can also add a constant value (e.g., data from an archive paper or another software program)."),
      fluidRow(
        column(width = 4, textInput("ana_return_name", NULL, "", placeholder = "name")),
        column(width = 4, textInput("ana_return_object", NULL, "", placeholder = "object/value")),
        column(width = 4, actionButton("add_return", "Add Return Value"))
      ),
      tableOutput("ana_return_list")
  ),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Results",
      uiOutput("ana_results")
  )
)
