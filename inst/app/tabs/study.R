### study_tab ----
study_tab <- tabItem(
  tabName = "study_tab",
  h3("Study Info"),
  p("This shiny app is under development and likely to have a lot of bugs."),
  fileInput("load_json", "Load from JSON", width = "100%"),
  h4("Required Info"),
  box(width = 12,
      textInput("study_name", "Study Name", "", "100%"),
      textAreaInput("study_description", "Study Description", "", "100%")
  ),
  h4("Custom Info"),
  uiOutput("custom_info_list", class="section_list"),
  box(width = 12,
      textInput("custom_info_name", "Custom Info Name", "", "100%"),
      textAreaInput("custom_info_value", "Custom Info Value", "", "100%"),
      actionButton("add_custom_info", "Add Custom Info")
  )
)
