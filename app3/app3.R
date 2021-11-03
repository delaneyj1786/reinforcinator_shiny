library(shiny)
library(ReenforcinateR)
library(tidyverse)

## Needs a pipeline
# No group?: Then use recounter or recounter 2
# Group - split and apply on list


## the issue is with recounter vs. recounter 2

# figure out how to adapt group with recounter2

# also - have a feeling we want split_group inside group_recounter (so legacy is the sep. function)

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
            actionButton("button1", "Confirm Data Selection"),
            br(),
            selectInput(inputId = "beh_stream",
                        label = "Select behavior stream column:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "beh_var",
                        label = "Select target behavior:",
                        choices = "Nothing Selected"),  ## needs to be updated with the dataset behavior column


            selectInput(inputId = "reinf_var",
                        label = "Select target consequence:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "group_var",
                        label = "Select Group Variable:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "group_var2",
                        label = "Select Second Group Variable:",
                        choices = "Nothing Selected"),

            actionButton("button2", "Run Overall Analysis"), # no grouping
            actionButton("button3", "Run Group Analysis"), # grouping
            actionButton("button4", "Run 2 Group Analysis"), # grouping by two

            br(),
            br(),
            downloadLink("downloadData", "Download Recounted Data File")

        ), # close sidebar panel

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(position = "above",
                        tabPanel("Data",tableOutput("contents")),
                        tabPanel("Recounted Data",tableOutput("contents_rc")),
                        tabPanel("Recounted Group",tableOutput("contents_rcsplit_df"))

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

    # Display reinforcer data
    output$contents_rc <- renderTable({
        rc_df()
    })



    # Display recounted group split df
    output$contents_rcsplit_df <- renderTable({
        recount_split_df()
    })



    ######## Sidebar interface for selecting function arguments
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

    # Update for consequence
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))


        updateSelectInput(session, "reinf_var",
                          label = NULL,
                          # choices = unique(dat1()$beh_stream[dat1()$beh_stream==input$beh_stream]),
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

    # Group name column
    observe({
        # requires file 1
        #    req(input$file1)
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "group_var",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    }) ### Close for behavior stream variable


    # Group 2 name column
    observe({
        # requires file 1
        #    req(input$file1)
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "group_var2",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    }) ### Close for behavior stream variable


    ## Activate reinforcer ####
    ### Overall Analysis #################
    observeEvent(c(input$button2,input$beh_var,input$reinf_var,input$beh_stream),{

        # create data frame
        behaviorstream<<-eventReactive(input$button2,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create rc_df
        rc_df<<-reactive({
            Recounter2(behaviorstream(),
                       input$beh_var,
                       input$reinf_var,
                       actor = NULL,
                       missing_data = NULL)$recounted_data_frame
        })
    }) ## Close button2

    ### Group Analysis #################
    observeEvent(c(input$button3,input$beh_var,input$reinf_var,input$beh_stream, input$group_var),{

        # create data frame
        behaviorstream<<-eventReactive(input$button3,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create split_df
        split_df<<-reactive({
            group_splitter(dat1(),
                       behaviorstream(),
                       input$beh_var,
                       input$reinf_var,
                       input$group_var,
                       actor = NULL)
        })


# We just need the character input ... not the actual stream ..
# YES!!!!
        # run reinforcinator on split
        recount_split_df<<-reactive({
            group_split_recounter(
                split_df(),
                input$beh_stream,
                input$beh_var,
                input$reinf_var,
                input$group_var
            )
        })

    }) ## Close button2




}

# Run the application
shinyApp(ui = ui, server = server)
