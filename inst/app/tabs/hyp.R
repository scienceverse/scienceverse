### hyp_tab ----
hyp_tab <- tabItem(
  tabName = "hyp_tab",
  h3("Hypotheses"),
  dataTableOutput("hyp_table"),
  actionButton("hyp_add", "Add Hypothesis", icon("plus")),
  actionButton("hyp_delete", "Delete", icon("trash")),
  actionButton("hyp_clear", "Clear", icon("times")),

  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      textInput("hyp_id", "Hypothesis ID", "", "100%"),
      textAreaInput("hyp_desc", "Hypothesis Description", "", "100%")
  ),

  ## . criteria ----
  box(width = 12, collapsible = TRUE,
      title = "Criteria",
      fluidRow(
        column(width = 3, textInput("crit_id", "id", "", placeholder = "Criterion ID")),
        column(width = 3, selectInput("crit_ana_id", "analysis_id", c())),
        column(width = 2, textInput("crit_result", "result", "", NULL, "Result")),
        column(width = 2, selectInput("crit_operator", "operator", c("<", "=", ">", "!="))),
        column(width = 2, textInput("crit_comparator", "comparator", "", NULL, "Comparator"))
      ),
      textOutput("crit_warning"),
      actionButton("crit_add", "Add Criterion", icon("plus")),
      dataTableOutput("crit_table")
  ),
  ## . evaluation ----
  box(width = 12, collapsible = TRUE,
      title = "Evaluation",
      p("What combination of criteria will corroborate or falsify your hypothesis? Use the criteria IDs above and any of the following symbols: ( ) & | !"),

      textInput("eval_cor_eval", "Corroboration Evaluation", "", "100%"),
      textInput("eval_fal_eval", "Falsification Evaluation", "", "100%")
  ),
  cinfo("hyp_info")
)
