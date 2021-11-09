
library(shiny)

## Need Two summaries
# 1. For raw data
# 2. For transformed (manipulated)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Data Manipulation"),

    # Sidebar with a slider input for number of bins
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

            selectInput(inputId = "partner_type",
                        label = "Select Partner Type:",
                        choices = "Nothing Selected"),

            actionButton("combinebutton", "Combine Codes"), # combine codes
            actionButton("deletebutton", "Delete Codes"), # delete codes
            actionButton("partnerbutton", "Run Partner Analysis"), # delete codes
            br(),
            br(),
            actionButton("runsum", "Run Raw Data Summaries"), # run
            actionButton("runsum_m", "Run Manipulated Data Summaries"), # run summaries
            br(),
            br(),
            downloadLink("downloadData_combine", "Download Combined Data File"),
            downloadLink("downloadData_delete", "Download Delete Data File"),
            downloadLink("downloadData_partner", "Download Partner Data File")

        ), # end sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(position = "above",
                       tabPanel("Data",tableOutput("contents")),
                       tabPanel("Delete Data", tableOutput("delete_contents")),
                       tabPanel("Combine Data", tableOutput("combine_contents")),
                       tabPanel("Partner Recode", tableOutput("partner_contents")),
                       tabPanel("Raw Summary", verbatimTextOutput("raw_sum_contents")), # not add to server
                       tabPanel("Delete Summary", verbatimTextOutput("delete_sum_contents")), # not add to server
                       tabPanel("Combine Summary", verbatimTextOutput("combine_sum_contents")) # not add to server

           )
        ) # end main panel

    ) #end sidebarLayout
) # end UI

# Define server logic required to draw a histogram
server <- function(input, output,  session) {

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
    # Display Original Data
    output$contents <- renderTable({
        dat1()
    })

    # Display Delete Data
    output$delete_contents <- renderTable({
        delete_df()
    })


    # Display combine Data
    output$combine_contents <- renderTable({
        combine_df()
    })


    # Display partner Data
    output$partner_contents <- renderTable({
        partner_df()
    })

############# Summaries @@@@@

    # Display raw summaries
    output$raw_sum_contents <- renderPrint({
        raw_sum()
    })

#
#     # Display manipulated summaries ** not implemented
#     output$man_sum_contents <- renderPrint({
#         man_sum()
#     })


    # Display delete summaries ** not implemented
    output$delete_sum_contents <- renderPrint({
        delete_sum()
    })



    # Display combine summaries ** not implemented
    output$combine_sum_contents <- renderPrint({
        combine_sum()
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


    # Update for consequence
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))


        updateSelectInput(session, "reinf_var",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })


    # Update for Deletion Var
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))


        updateSelectInput(session, "delete_var",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

    # Update for Combine Var1
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))


        updateSelectInput(session, "combine_var1",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })



    # Update for Combine Var2
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))


        updateSelectInput(session, "combine_var2",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

    # partner var column
    observe({
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "partner_var",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    }) ### Close for behavior stream variable


    # Update for partner type
    observeEvent(input$partner_var,{
        column_levels <- as.character(sort(unique(dat1()[[input$partner_var]])))

        updateSelectInput(session, "partner_type",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })




    ####################
    # Deleter
    ## Activate Deleter  ####
    ### Overall Analysis #################
    observeEvent(c(input$deletebutton,input$beh_stream, input$delete_var),{

        # create data frame
        behaviorstream<<-eventReactive(input$deletebutton,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create rc_df
        delete_df<<-reactive({
            deleter(dat1(),
                    behaviorstream(),
                       input$delete_var)
        })
    }) ## Close button2




    # Combiner
    ## Activate Combiner  ####
    ### Overall Analysis #################
    observeEvent(c(input$combinebutton,input$beh_stream, input$combine_var1, input$combine_var2),{

        # create data frame
        behaviorstream<<-eventReactive(input$combinebutton,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create rc_df
        combine_df<<-reactive({
            combiner(dat1(),
                    behaviorstream(),
                    input$combine_var1,
                    input$combine_var2)
        })
    }) ## Close button2




    # Partner
    ## Activate Partner Analysis  ####
    ### Overall Analysis #################
    observeEvent(c(input$partnerbutton,input$beh_stream, input$partner_type, input$reinf_var, input$partner_var),{

        # create data frame
        behaviorstream<<-eventReactive(input$partnerbutton,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # create partner col
        partnerstream<<-eventReactive(input$partnerbutton,{
            (((dat1()[[input$partner_var]])))
        }) # close behavior stream

        # create rc_df
        partner_df<<-reactive({
            partner_recoder(dat1(),
                     behaviorstream(),
                     input$partner_type,
                     input$reinf_var,
                     partnerstream())
        })
    }) ## Close button2


    #### Downloading
    #download  combine df
    output$downloadData_combine <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(combine_df(), file)
        }
    )



    #download delete df
    output$downloadData_delete <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(delete_df(), file)
        }
    )



    #download Partner df
    output$downloadData_partner <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(partner_df(), file)
        }
    )


    #########################
    # Summaries #
    ## Raw Data ##

    observeEvent(c(input$runsum,input$beh_stream, input$beh_var),{

        # create data frame
        behaviorstream<<-eventReactive(input$runsum,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream


        # summarize
        # raw_sum<<-reactive({
        #     table(dat1()[,3])
        # }
        raw_sum <<- reactive({
            janitor::tabyl(behaviorstream())
        })
    }) ## Close button2





    ## Combined Data ##

    observeEvent(c(input$runsum_m,input$beh_stream, input$beh_var, input$combinebutton, input$combine_var1, input$combine_var2),{

        # create data frame
        behaviorstream_combine<<-eventReactive(input$runsum_m,{
            (((combine_df()[[input$beh_stream]])))
        }) # close behavior stream


        # summarize
        # raw_sum<<-reactive({
        #     table(dat1()[,3])
        # })


        combine_sum <<- reactive({
            janitor::tabyl(behaviorstream_combine())
        })
    }) ## Close button2



    ## Delete Data ##

    observeEvent(c(input$runsum_m,input$beh_stream, input$beh_var, input$deletebutton, input$delete_var),{

        # create data frame
        behaviorstream_delete<<-eventReactive(input$runsum_m,{
            (((delete_df()[[input$beh_stream]])))
        }) # close behavior stream


        # summarize
        # raw_sum<<-reactive({
        #     table(dat1()[,3])
        # })


        delete_sum <<- reactive({
            janitor::tabyl(behaviorstream_delete())
        })
    }) ## Close button2


    # Partner Data ##



} # end server

# Run the application
shinyApp(ui = ui, server = server)
