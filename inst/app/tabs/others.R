### met_tab ----
met_tab <- tabItem(
  tabName = "met_tab",
  h3("Methods")
)

### dat_tab ----
dat_tab <- tabItem(
  tabName = "dat_tab",
  h3("Data"),
  box(width = 12,
      textInput("dat_id", "Data ID", "", "100%"),
      checkboxInput("dat_data", "Include Data", value = TRUE),
      fileInput("dat_file", "Upload Data"),
      actionButton("add_data", "Add Data", icon("plus"))
  ),
  tableOutput("dat_table")
)

### json_tab ----
json_tab <- tabItem(
  tabName = "json_tab",
  h3("JSON Text"),
  verbatimTextOutput("json_text")
)

### output_tab ----
output_tab <- tabItem(
  tabName = "output_tab",
  h3("Human-Readable Summary"),
  htmlOutput("human_readable")
)
