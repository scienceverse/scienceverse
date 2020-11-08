pqrpt_tbl <- read.csv("data/PQRPT.csv")

titles <- dplyr::filter(pqrpt_tbl, !grepl("[0-9]", Label))
entries <- dplyr::filter(pqrpt_tbl, grepl("[0-9]", Label))
entries$category <- gsub("[0-9]", "", entries$Label)

pqrptUI <- function(id, name, desc) {
  ns <- NS(id)

  if (id == "T9") {
    keywords <- c("psychology", "neuroscience",
                  "biology", "physics", "chemistry")
    inputUI <- selectizeInput(ns("selectize"), NULL,
                              keywords, multiple = TRUE)
  } else if (id == "T10" || id == "T11") {
    inputUI <- selectInput(ns("select"), NULL,
      list(
        "Not available" = c(
          "Not applicable",
          "Cannot be made available"
        ),
        "Access via download" = c(
          "All purposes",
          "Use restricted to scientific purposes",
          "Use agreed and defined on an individual case basis"),
        "Restricted access" = c(
          "Via secure data center",
          "Upon request by member of scientific community"
        ),
        "Other (please specify)" = "other"
      )
    )
  } else {
    inputUI <- textAreaInput(ns("text"), NULL)
  }

  fluidRow(
    column(width = 1, actionButton(ns("add"), id, icon("plus"))),
    column(width = 6, h4(name), p(desc)),
    column(width = 5, inputUI)
  )
}

pqrpt_box <- lapply(seq_along(titles$Label), function(i) {
  cat_entries <- dplyr::filter(entries, category == titles$Label[[i]])
  box_args <- list(width = 12,
                   title = titles$Name[[i]],
                   collapsible = TRUE, collapsed = TRUE)

  lapply(seq_along(cat_entries$Label), function(j) {
    pqrptUI(id = cat_entries$Label[[j]],
            name = cat_entries$Name[[j]],
            desc = cat_entries$Description[[j]])
  }) %>%
    c(box_args, .) %>%
    do.call("box", .)
})

### pre_tab ----
pre_tab <- tabItem(
  tabName = "pre_tab",
  HTML("This section allows you to associate parts of the scienceverse output with the <a href='https://docs.google.com/spreadsheets/d/1vlp5GN-HXrtrjCdjE28f_3tT6RiwhQO2vVeOZGOaFsQ/edit#gid=0'>Preregistration for Quantitative Research in Psychology Template</a>. It is under development and doesn't work yet."),
  selectInput("pre_sections", "Sections", c()),
  pqrpt_box
)
