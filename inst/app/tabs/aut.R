### aut_tab ----
aut_tab <- tabItem(
  tabName = "aut_tab",
  h3("Authors"),
  dataTableOutput("aut_table"),
  actionButton("aut_add", "Add Author", icon("plus")),
  actionButton("aut_delete", "Delete Author", icon("trash")),
  actionButton("aut_clear", "Clear", icon("times")),

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
  cinfo("aut_info"),

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

