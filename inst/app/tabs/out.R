### out_tab ----
out_tab <- tabItem(
  tabName = "out_tab",

  tabsetPanel(type = "tabs",
    tabPanel("Human-Readable",
      downloadButton("download_pre", "Download Prereg Report"),
      downloadButton("download_post", "Download Postreg Report"),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Study Info",
          htmlOutput("out_study")
      ),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Authors",
          htmlOutput("out_aut")
      ),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Hypotheses",
          htmlOutput("out_hyp")
      ),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Analyses",
          htmlOutput("out_ana")
      ),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Data",
          htmlOutput("out_dat")
      ),
      box(width = 12, collapsible = TRUE, collapsed = FALSE,
          title = "Results",
          htmlOutput("out_res")
      ),

      htmlOutput("human_readable")),
    tabPanel("Script",
      fluidRow(
        column(3, downloadButton("download_script", "Download Script")),
        column(3, checkboxInput("dat_embed", "Embed Data", value = TRUE)),
        column(3, radioButtons("script_ext", "Script Type", c("R Markdown" = ".Rmd", "R Script" = ".R"), selected = ".Rmd")),
        column(3, selectInput("script_header_lvl", "Top Header Level", 1:4, 3))
      ),
      verbatimTextOutput("script_text")
    ),
    tabPanel("Machine-Readable JSON",
      downloadButton("download_json", "Download JSON"),
      checkboxInput("dat_include", "Include Data", value = TRUE),
      verbatimTextOutput("json_text")
    )
  )
)
