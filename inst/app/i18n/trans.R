translator <- Translator$new(translation_json_path = "i18n/translation.json")

trans_text <- list()

trans_labels <- list(
  updateTextInput = c(),
  updateTextAreaInput = c(),
  updateSelectInput = c(),
  updateNumericInput = c(),
  updateCheckboxInput = c(),
  updateCheckboxGroupInput = c(),
  updateActionButton = c()
)

h2 <- function(x, ...) {
  trans_text[x] <<- x
  shiny::h2(x, en = x, ...)
}
h3 <- function(x, ...) {
  trans_text[x] <<- x
  shiny::h3(x, en = x, ...)
}
h4 <- function(x, ...) {
  trans_text[x] <<- x
  shiny::h4(x, en = x, ...)
}
p <- function(x, ...) {
  trans_text[x] <<- x
  shiny::p(x, en = x, ...)
}

textInput <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateTextInput[inputId] <<- label
  shiny::textInput(inputId, label, ...)
}

textAreaInput <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateTextAreaInput[inputId] <<- label
  shiny::textAreaInput(inputId, label, ...)
}

selectInput <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateSelectInput[inputId] <<- label
  shiny::selectInput(inputId, label, ...)
}

numericInput <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateNumericInput[inputId] <<- label
  shiny::numericInput(inputId, label, ...)
}

checkboxInput <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateCheckboxInput[inputId] <<- label
  shiny::checkboxInput(inputId, label, ...)
}

checkboxGroupInput<- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateCheckboxGroupInput[inputId] <<- label
  shiny::checkboxGroupInput(inputId, label, ...)
}

actionButton <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateActionButton[inputId] <<- label
  shiny::actionButton(inputId, label, ...)
}


save_trans <- function(trans_text, trans_labels) {
  ## save translations ----
  is_local <- Sys.getenv('SHINY_PORT') == ""
  if (is_local) {
    all_text <- c(trans_text, trans_labels) %>%
      unname() %>%
      unlist() %>%
      unique() %>%
      sort()
    j <- jsonlite::read_json("i18n/translation.json")
    cur_text <- sapply(j$translation, "[[", "en")
    new_text <- setdiff(all_text, cur_text) %>%
      lapply(function(x) { list("en" = x) })

    if (length(new_text)) {
      j$translation <- c(j$translation, new_text)

      # save to new_translation so user can choose to update
      jsonlite::toJSON(j, auto_unbox = TRUE) %>%
        jsonlite::prettify(2) %>%
        write("i18n/new_translation.json")

      debug_msg("saving ", length(new_text),
              " new translations")
    }
  }
}
