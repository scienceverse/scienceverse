### out_tab ----
out_tab <- tabItem(
  tabName = "out_tab",

  tabsetPanel(type = "tabs",
              tabPanel("Human-Readable",
                       downloadButton("download_pre", "Download Prereg Report"),
                       downloadButton("download_post", "Download Postreg Report"),
                       htmlOutput("human_readable")),
              tabPanel("Machine-Readable JSON",
                       downloadButton("download_json", "Download"),
                       checkboxInput("dat_include", "Include Data", value = TRUE),
                       verbatimTextOutput("json_text"))
  )
)
