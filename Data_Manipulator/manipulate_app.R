
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
            br(),
            br(),
            selectInput(inputId = "beh_stream",
                        label = "Select Behavior Stream Column:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "beh_var",
                        label = "Select Target Behavior (DV):",
                        choices = "Nothing Selected"),

            selectInput(inputId = "reinf_var",
                        label = "Select Target Consequence (IV):",
                        choices = "Nothing Selected"),

            selectInput(inputId = "delete_var",
                        label = "Select Variable for Deletion:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "combine_var1",
                        label = "Select Variable 1 for Combination:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "combine_var2",
                        label = "Select Variable 2 for Combination:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "partner_var",
                        label = "Select Partner Variable:",
                        choices = "Nothing Selected"),

            actionButton("combinebutton", "Combine Codes"), # combine codes
            actionButton("deletebutton", "Delete Codes"), # delete codes
            actionButton("partnerbutton", "Run Partner Analysis") # delete codes
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

    ######## Sidebar interface for selecting function arguments
    # get options for behavior var
    observe({
        # requires file 1
        #    req(input$file1)
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "beh_stream",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    }) ### Close for behavior stream variable


    # update behavior options based on column levels
    # https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))
        updateSelectInput(session, "beh_var",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

} # end server

# Run the application
shinyApp(ui = ui, server = server)
