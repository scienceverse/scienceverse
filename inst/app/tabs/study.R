### study_tab ----
study_tab <- tabItem(
  tabName = "study_tab",
  h3("Study Info"),
  p("This shiny app is under development and likely to have a lot of bugs."),
  fileInput("load_json", "Load from JSON", width = "100%"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      textInput("study_name", "Study Name", "", "100%"),
      textAreaInput("study_desc", "Study Description", "", "100%")
  ),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Custom Info",
      uiOutput("custom_info_list", class="section_list"),
      dataTableOutput("custom_info_table"),
      actionButton("custom_info_add", "Add", icon("plus")),
      actionButton("custom_info_delete", "Delete", icon("trash"))
  )
)

