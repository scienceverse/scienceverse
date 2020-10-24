library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar() ,
    dashboardBody(
        dataTableOutput("dt")
    )
)

server <- function(input, output) {
    output$dt <- renderDataTable(
        data.frame(a = 1:3),
        editable = TRUE,
        style = 'jqueryui'
    )
}

# Run the application
shinyApp(ui = ui, server = server)
