
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("controller", "Controller", 1:3,1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "hidden_tabs",
                type = "hidden",
                tabPanelBody("panel1", "Panel 1 Contents"),
                tabPanelBody("panel2", "Panel 2 Contents"),
                tabPanelBody("panel3", "Panel 3 Contents")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
observeEvent(input$controller,{
    updateTabsetPanel(session,"hidden_tabs",selected = paste0("panel",input$controller) )
})

}

# Run the application
shinyApp(ui = ui, server = server)
