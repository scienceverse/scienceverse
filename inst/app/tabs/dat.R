### dat_tab ----
dat_tab <- tabItem(
  tabName = "dat_tab",
  h3("Data"),
  uiOutput("data_list", class="section_list"),
  box(width = 12,
      textInput("dat_id", "Data ID", "", "100%"),
      checkboxInput("dat_data", "Include Data", value = TRUE),
      fileInput("dat_file", "Upload Data"),
      actionButton("add_data", "Add Data", icon("plus")),
      actionButton("clear_data", "Clear", icon("times"))
  ),
  tabsetPanel(type = "tabs",
              tabPanel("Data", dataTableOutput("dat_table")),
              tabPanel("Codebook",
                       h4("This section is under construction and quite buggy"),
                       uiOutput("var_list"),
                       hidden(textInput("var_name", "Name")),
                       textInput("var_desc", "Description", width="100%"),
                       selectInput("var_type", "Data Type",
                                   c("Integer (int)" = "int",
                                     "Character (string)" = "string",
                                     "Double (float)" = "float",
                                     "Logical (bool)" = "bool")),
                       actionButton("var_update", "Update")),
              tabPanel("Codebook JSON",
                       downloadButton("download_cb", "Download"),
                       verbatimTextOutput("codebook_json"))
  )
)
