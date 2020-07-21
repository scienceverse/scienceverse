### ana_tab ----
ana_tab <- tabItem(
  tabName = "ana_tab",
  h3("Analyses"),
  box(width = 12,
      textInput("ana_id", "Analysis ID", "", "100%"),
      textAreaInput("ana_code", "Analysis Code", "", "100%"),
      h4("Return"),
      p("If your code doesn't return a named list, add each object to be returned below. E.g., if your code creates two objects named `my_mean` and `my_sd`, you can refer to them in the criteria as `m` and `s` if you set name to `m` and `s` and object to `my_mean` and `my_sd`."),
      fluidRow(
        column(width = 4, textInput("ana_return_name", NULL, "", placeholder = "name")),
        column(width = 4, textInput("ana_return_object", NULL, "", placeholder = "object")),
        column(width = 4, actionButton("add_return", "Add Return Value"))
      ),
      tableOutput("ana_return_list")
  ),
  actionButton("add_analysis", "Add Analysis", icon("plus")),
  box(width = 12,
      uiOutput("analysis_list")
  )
)