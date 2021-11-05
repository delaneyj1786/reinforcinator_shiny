
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Manipulation"),

    # Sidebar with a slider input for number of bins
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
            actionButton("button1", "Confirm Data Selection"),
        ), # end sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(position = "above",
                       tabPanel("Data",tableOutput("contents"))
           )
        ) # end main panel

    ) #end sidebarLayout
) # end UI

# Define server logic required to draw a histogram
server <- function(input, output) {

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
} # end server

# Run the application
shinyApp(ui = ui, server = server)
