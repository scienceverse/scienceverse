### aut_tab ----
aut_tab <- tabItem(
  tabName = "aut_tab",
  h3("Authors"),
  uiOutput("author_list"),
  actionButton("author_reorder", "Reorder Authors", icon("sort")),

  box(width=12,
      hidden(
        numericInput("author_n", "Author Number", value = 1, min = 1)
      ),
      fluidRow(
        column(width = 6,
              textInput("given", "Given Name(s) including initials")),
        column(width = 6,
               textInput("surname", "Last Name(s)"))
      ),
      fluidRow(
        column(width = 9, textInput("orcid", "ORCiD")),
        column(width = 3, actionButton("get_orcid", "Look up ORCiD", icon("orcid")))
      ),
      checkboxGroupInput("roles", "Contributor Roles",
                         inline = TRUE,
                         choices = credit_roles("names")),
      uiOutput("author_info_list", class="section_list"),
      fluidRow(
        column(width = 3,
               textInput("author_info_name", NULL, "",
                         "100%", "Custom Info Name")),
        column(width = 6,
               textInput("author_info_value", NULL, "",
                         "100%", "Custom Info Value")),
        column(width = 3,
               actionButton("add_author_info", "Add Custom Info", icon("plus")))
      )
  ),
  actionButton("add_author", "Add Author", icon("user-plus")),

  # . . credit ----
  h4("CRediT"),
  tabsetPanel(type = "tabs",
              tabPanel("Contributor Roles", uiOutput("credit_roles")),
              tabPanel("JATS Format",
                       p(HTML("CRediT in JATS 1.2 format as described at <a href='https://jats4r.org/credit-taxonomy'>https://jats4r.org/credit-taxonomy</a>")),
                       downloadButton("download_jats", "Download"),
                       verbatimTextOutput("jats_text")))
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
