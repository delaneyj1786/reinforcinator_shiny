

library(shiny)
library(ReenforcinateR)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reinforcinator"),

    # Sidebar with a slider input for input for functions
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("Input_File",
                                    "Elevator",
                                    "Picture",
                                    "Two_Person",
                                    "Reinforcement",
                                    "Neutral",
                                    "Punishment")),
            actionButton("button1", "Confirm Data Selection")

        ), # close sidbar panel

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(position = "above",
                        tabPanel("Data",tableOutput("contents"))
            ) # close tabset panel
        ) # close main panel

        )   # close sidebar layout
) # close UI fluid

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    ### alternative w/pre load
    dat1<-eventReactive(input$button1,{
        switch(input$dataset,
               "Input_File"= csv(),
               "Elevator" = elevator,
               "Picture" = picture_stream,
               "Two_Person" = two_person_picture,
               "Reinforcement" = reinforcement,
               "Neutral" = noeffect,
               "Punishment" = punishment)

    })

    # Display Original Data
    output$contents <- renderTable({
        dat1()
    })

}

# Run the application
shinyApp(ui = ui, server = server)
