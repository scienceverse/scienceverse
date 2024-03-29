### hyp_tab ----
hyp_tab <- tabItem(
  tabName = "hyp_tab",

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
      p("Criteria are logical values (true or false) that test your hypothesis from the results of analyses. For example, your hypothesis might just require that a p-value from one analysis be less than .05, or it might require several criteria be true (or false)."),
      fluidRow(
        column(width = 3, textInput("crit_id", "id", "", placeholder = "Criterion ID")),
        column(width = 3, selectInput("crit_ana_id", "analysis_id", c())),
        column(width = 2, textInput("crit_result", "result", "", NULL, "Result")),
        column(width = 2, selectInput("crit_operator", "operator", c("<", "=", ">", "!="))),
        column(width = 2, textInput("crit_comparator", "comparator", "", NULL, "Comparator"))
      ),
      textOutput("crit_warning"),
      actionButton("crit_add", "Add Criterion", icon("plus")),
      actionButton("crit_delete", "Delete Criterion", icon("trash")),
      dataTableOutput("crit_table")
  ),
  ## . evaluation ----
  box(width = 12, collapsible = TRUE,
      title = "Evaluation",
      p("What combination of criteria will corroborate or falsify your hypothesis? Use the criteria IDs above and any of the following symbols: ( ) & | !"),

      fluidRow(
        column(width = 6, textInput(
          "eval_cor_eval",
          "Corroboration Evaluation", "", "100%")),
        column(width = 6, textInput(
          "eval_fal_eval",
          "Falsification Evaluation", "", "100%"))
      ),
      fluidRow(
        column(width = 6, textAreaInput(
          "eval_cor_desc",
          "Corroboration Description", "")),
        column(width = 6, textAreaInput(
          "eval_fal_desc",
          "Falsification Description", ""))
      )
  ),
  cinfo("hyp_info")
)
