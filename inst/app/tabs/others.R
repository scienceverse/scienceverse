### aut_tab ----
aut_tab <- tabItem(
  tabName = "aut_tab",
  h3("Authors"),
  dataTableOutput("aut_table"),
  actionButton("aut_add", "Add Author", icon("plus")),
  actionButton("aut_delete", "Delete Author", icon("trash")),
  actionButton("aut_clear", "Clear", icon("times")),
  actionButton("aut_reorder", "Reorder Authors", icon("sort")),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      hidden(
        numericInput("aut_n", "Author Number", value = 1, min = 1)
      ),
      fluidRow(
        column(width = 6,
              textInput("given", "Given Name(s) including initials")),
        column(width = 6,
               textInput("surname", "Last Name(s)"))
      )
  ),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Recommended Info",
      fluidRow(
        column(width = 9, textInput("orcid", "ORCiD")),
        column(width = 3, actionButton("get_orcid", "Look up ORCiD", icon("orcid")))
      ),
      checkboxGroupInput("roles", "Contributor Roles",
                         inline = TRUE,
                         choices = credit_roles("names"))
  ),
  box(width = 12, collapsible = TRUE, collapsed = TRUE,
      title = "Custom Info",
      uiOutput("aut_info_list", class="section_list"),
      fluidRow(
        column(width = 3,
               textInput("aut_info_name", NULL, "",
                         "100%", "Custom Info Name")),
        column(width = 6,
               textInput("aut_info_value", NULL, "",
                         "100%", "Custom Info Value")),
        column(width = 3,
               actionButton("aut_info_add", "Add Custom Info", icon("plus")))
      )
  ),

  # . . credit ----
  box(width = 12, collapsible = TRUE, collapsed = TRUE,
    title = "CRediT",
    tabsetPanel(type = "tabs",
      tabPanel("Contributor Roles",
               uiOutput("credit_roles")),
      tabPanel("JATS Format",
        downloadButton("download_jats", "Download"),
        HTML("CRediT in JATS 1.2 format as described at <a href='https://jats4r.org/credit-taxonomy'>https://jats4r.org/credit-taxonomy</a>"),
        verbatimTextOutput("jats_text")
      )
    )
  )
)

### met_tab ----
met_tab <- tabItem(
  tabName = "met_tab",
  h3("Methods")
)


### output_tab ----
output_tab <- tabItem(
  tabName = "output_tab",

  tabsetPanel(type = "tabs",
    tabPanel("Human-Readable",
             downloadButton("download_pre", "Download Prereg Report"),
             downloadButton("download_post", "Download Postreg Report"),
             htmlOutput("human_readable")),
    tabPanel("Machine-Readable JSON",
             downloadButton("download_json", "Download"),
             verbatimTextOutput("json_text"))
  )
)
