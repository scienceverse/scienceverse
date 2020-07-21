### hyp_tab ----
hyp_tab <- tabItem(
  tabName = "hyp_tab",
  h3("Hypotheses"),

  box(width = 12,
      textInput("hyp_id", "Hypothesis ID", "", "100%"),
      textAreaInput("hyp_description", "Hypothesis Description", "", "100%")
  ),

  ## . criteria ----
  box(width = 12,
      title = "Criteria",
      fluidRow(
        column(width = 3, textInput("crit_id", "id", "", placeholder = "Criterion ID")),
        column(width = 3, selectInput("crit_ana_id", "analysis_id", c())),
        column(width = 2, textInput("crit_result", "result", "", NULL, "Result")),
        column(width = 2, selectInput("crit_operator", "operator", c("<", "=", ">", "!="))),
        column(width = 2, textInput("crit_comparator", "comparator", "", NULL, "Comparator"))
      ),
      textOutput("criteria_warning"),
      actionButton("add_criterion", "Add Criterion", icon("plus")),
      tableOutput("criteria_table")
  ),
  ## . evaluation ----
  box(width = 12, title = "Evaluation",
      p("What combination of criteria will corroborate or falsify your hypothesis? Use the criteria IDs above and any of the following symbols: ()&|!"),

      textAreaInput("eval_cor_desc", "Corroboration Description", "", "100%"),
      textInput("eval_cor_eval", "Corroboration Evaluation", "", "100%"),
      textAreaInput("eval_fal_desc", "Falsification Description", "", "100%"),
      textInput("eval_fal_eval", "Falsification Evaluation", "", "100%")
  ),

  actionButton("add_hypothesis", "Add Hypothesis", icon("plus"))
)