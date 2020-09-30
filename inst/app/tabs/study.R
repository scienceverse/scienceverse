### study_tab ----
study_tab <- tabItem(
  tabName = "study_tab",
  h3("Study Info"),
  box(width = 12,
      textInput("study_name", "Study Name", "", "100%"),
      textAreaInput("study_description", "Study Description", "", "100%")
  ),
  h4("Authors"),
  htmlOutput("author_list"),
  actionButton("author_reorder", "Reorder Authors"),

  h4("Add an Author"),
  box(width=12,
      hidden(
        numericInput("author_n", "Author Number", value = 1, min = 1)
      ),
      textInput("given", "Given Name(s) including initials"),
      textInput("surname", "Last Name(s)"),
      textInput("orcid", "ORCiD"),
      actionButton("get_orcid", "Look up ORCiD"),
      textInput("email", "Email"),
      checkboxGroupInput("roles", "Contributor Roles",
                         choices = credit_roles("names")),
      actionButton("add_author", "Add Author"),
  ),
  h4("Contributor Roles"),
  uiOutput("credit_roles")
)
