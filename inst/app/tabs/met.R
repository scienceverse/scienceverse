### met_tab ----
met_tab <- tabItem(
  tabName = "met_tab",
  h3("Methods"),
  p("We are currently working on ways to input methods. For now, you can include methods as free text, JSON or YAML formats. If you use experimentum, download the project structure as a JSON file and upload it here."),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Manual Entry",
      actionButton("met_prettify", "Prettify"),
      actionButton("met_drop_empty", "Drop Empty Values"),
      radioButtons("met_type", NULL,
                   c("text" = "txt",
                     "JSON" = "json",
                     "YAML" = "yaml"),
                   selected = "txt",
                   inline = TRUE),

      fileInput("met_file", "Load from File", width = "100%"),
      textAreaInput("met_text", NULL, "", "100%", "400px"),
      verbatimTextOutput("met_err"),
  ),
  cinfo("met_info")
)



