# note : testing files located in desktop / sage analysis
# creating some shorter test files

#### TODO
# Will need to add a 'search by video feature'
#  simple filter - but needs percentage summary foo particular behaviors (i.g., ALL OP videos)


library(shiny)
library(devtools)
#install_github("delaneyj1786/REINFORCINATOR")
library(ReenforcinateR)
library(tidyverse)

## Needs a pipeline
# No group?: Then use recounter or recounter 2
# Group - split and apply on list

ui <- fluidPage(

    # Application title
    titlePanel("Reinforcinator"),

    # Sidebar with a slider input for input for functions
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),


            tags$hr(),
            checkboxInput("header", "Header", TRUE),
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
                        label = "Select Behavior Stream Column:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "beh_var",
                        label = "Select Target Behavior (DV):",
                        choices = "Nothing Selected"),


            selectInput(inputId = "reinf_var",
                        label = "Select Target Consequence (IV):",
                        choices = "Nothing Selected"),

            selectInput(inputId = "group_var",
                        label = "Select Group Variable:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "group_var2",
                        label = "Select Second Group Variable:",
                        choices = "Nothing Selected"),


## Need 1) partner rcounter 2) deleter 3) combiner


            actionButton("button2", "Run Overall Analysis"), # no grouping
br(),
            actionButton("button3", "Run Group Analysis"), # grouping
br(),
            actionButton("button4", "Run 2 Group Analysis"), # grouping by two
            br(),
actionButton("run_runplot", "Run Running Plot"),
            br(),
            downloadLink("downloadData", "Download Recounted Data File"),
br(),
            downloadLink("downloadData_1group", "Download Recounted Group Data File"),
br(),
            downloadLink("downloadData_2group", "Download Recounted 2 Group Data File")
        ), # close sidebar panel

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(position = "above",
                        tabPanel("Data",tableOutput("contents")),
                        tabPanel("Recounted Data",tableOutput("contents_rc")),
                        tabPanel("Recounted Group",tableOutput("contents_rcsplit_df")),
                        tabPanel("Recounted 2 Group",tableOutput("contents_rcsplit_df2")),
                        tabPanel("Running Plot",  plotOutput("run_plot_contents")) ## should change the name - did not test yet

            ) # close tabset panel
        ) # close main panel

        )   # close sidebar layout
) # close UI fluid

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    csv<<-reactive({
        #      if(!exists(input$file1)) return()
        read_csv(input$file1$datapath)
    })



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

    ### Display Data Frames ####
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

    # Display 2 recounted group split df
    output$contents_rcsplit_df2 <- renderTable({
        recount_split_df2()
    })

#### Display Plots ####
    ############### PLOTS #####

    output$run_plot_contents <- renderPrint({
        run_plot()  ## did not add to server yet
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


    #
    # ### Group Analysis 2 #################
    observeEvent(c(input$button4,input$beh_var,input$reinf_var,input$beh_stream, input$group_var, input$group_var2),{

        # create data frame
        behaviorstream<<-eventReactive(input$button4,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create split_df
        split_df2<<-reactive({
            group_splitter2(dat1(),
                           behaviorstream(),
                           input$beh_var,
                           input$reinf_var,
                           input$group_var,
                           input$group_var2,
                        filt = TRUE)
        })


        recount_split_df2<<-reactive({
            group_split_recounter(
                split_df2(),
                input$beh_stream,
                input$beh_var,
                input$reinf_var,
                input$group_var  ## techincally ... actor i think
            )
        })

    }) ## Close button2

##################
    # Download Recounted DF
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(rc_df(), file)
        }
    )


    # Download 1 group
    output$downloadData_1group <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(recount_split_df(), file)
        }
    )


    # Download 2 group
    output$downloadData_2group <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(recount_split_df2(), file)
        }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
