translator <- Translator$new(translation_json_path = "i18n/translation.json")

trans_text <- list()

trans_labels <- list(
  updateTextInput = c(),
  updateTextAreaInput = c(),
  updateSelectInput = c(),
  updateNumericInput = c(),
  updateCheckboxInput = c(),
  updateActionButton = c()
)

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

actionButton <- function(inputId, label, ...) {
  if (!is.null(label))
    trans_labels$updateActionButton[inputId] <<- label
  shiny::actionButton(inputId, label, ...)
}
