translator <- Translator$new(translation_json_path = "i18n/translation.json")

trans_labels <- list(
  updateTextInput = c(
    study_name = "Study Name",
    given = "Given Name(s) including initials",
    surname = "Last Name(s)",
    orcid = "ORCiD",

    hyp_id = "Hypothesis ID",
    eval_cor_eval = "Corroboration Evaluation",
    eval_fal_eval = "Falsification Evaluation"
  ),
  updateSelectInput = c(
    lang = "Change language"
  ),
  updateNumericInput = c(
    author_n = "Author Number"
  ),
  updateTextAreaInput = c(
    study_description = "Study Description",
    eval_cor_desc = "Corroboration Description",
    eval_fal_desc = "Falsification Description"
  ),
  updateCheckboxInput = c(
    roles = "Contributor Roles"
  ),
  updateActionButton = c(
    author_reorder = "Reorder Authors",
    add_author = "Add Author",
    add_hypothesis = "Add Hypothesis",
    add_criterion = "Add Criterion"
  )
)
