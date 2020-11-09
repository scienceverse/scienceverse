### met_tab ----
met_tab <- tabItem(
  tabName = "met_tab",

  p("We are currently working on ways to input methods and this section is very experimental. For now, you can include methods as a free text description and structured JSON or YAML formats. If you use experimentum, download the project structure as a JSON file and upload it under Manual List Entry."),

  #sectionUI("met_sec", "Methods"),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      textInput("met_id", "Method ID", "", "100%"),
      textAreaInput("met_desc", "Method Description", "", "100%")
  ),

  cinfo("met_info"),

  box(width = 12, collapsible = TRUE, collapsed = TRUE,
      title = "Manual List Entry",
      actionButton("met_prettify", "Prettify"),
      actionButton("met_drop_empty", "Drop Empty Values"),
      radioButtons("met_type", NULL,
                   c("JSON" = "json",
                     "YAML" = "yaml"),
                   selected = "txt",
                   inline = TRUE),

      fileInput("met_file", "Load from File", width = "100%"),
      textAreaInput("met_text", NULL, "", "100%", "400px"),
      verbatimTextOutput("met_err"),
  )
)



