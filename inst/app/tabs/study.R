### study_tab ----
study_tab <- tabItem(
  tabName = "study_tab",
  p("This shiny app is under development; all materials created should be carefully checked."),
  fileInput("load_json", "Load from JSON", width = "100%"),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Required Info",
      p("The study name will be used for naming downloaded files, so should be relatively short. You can add a longer official title below."),
      textInput("study_name", "Study Name", "", "100%")
  ),
  box(width = 12, collapsible = TRUE, collapsed = FALSE,
      title = "Recommended Info",
      textInput("study_title", "Study Title", "", "100%"),
      textAreaInput("study_desc", "Study Description", "", "100%"),
      textInput("study_keywords", "Keywords (separate with semicolons)", "", "100%", )
  ),
  cinfo("study_info")
)

