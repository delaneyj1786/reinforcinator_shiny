
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

        # Show a plot of the generated distribution
            tabsetPanel(
                id = "selectedtab",
                tabPanel("Tab1", uiOutput("Tab1"),
                tabPanel("Tab2", uiOutput("Tab2"))
                )
            )
        )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
output$Tab1 <- renderUI({
    tabsetPanel(id = "subTabPanel1",
                tabPanel("sub11"),
                tabPanel("sub12")
    )
})
output$Tab2 <- renderUI({
    tabsetPanel(id = "subTabPanel2",
                tabPanel("sub21"),
                tabPanel("sub22"))
})
}

# Run the application
shinyApp(ui = ui, server = server)
