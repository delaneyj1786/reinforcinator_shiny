# note : testing files located in desktop / sage analysis
# creating some shorter test files

#### TODO
# Will need to add a 'search by video feature'
#  simple filter - but needs percentage summary foo particular behaviors (i.g., ALL OP videos)

#### Need to update plot data ... it is not updating the two graphs for multiple groups
## grouping has no effect on the graph output

#test <- recounter(elevator,BEH,"o","A")$recounted_data_frame


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


            actionButton("button2", "Run Overall Analysis"), br(), # no group
            actionButton("button3", "Run Group Analysis"), br(), # grouping
            actionButton("button4", "Run 2 Group Analysis"), br(), # grouping by two
actionButton("run_runplot", "Run Running Plot"), br(),
actionButton("run_meanplot", "Run Mean Plot"), br(),
actionButton("run_sequenceplot", "Run Overall Sequence Plot"), br(),

            downloadLink("downloadData", "Download Recounted Data File"), br(),
            downloadLink("downloadData_1group", "Download Recounted Group Data File"), br(),
            downloadLink("downloadData_2group", "Download Recounted 2 Group Data File")
        ), # close sidebar panel

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(position = "above",
                        tabPanel("Data",tableOutput("contents")),
                        tabPanel("Recounted Data",tableOutput("contents_rc")),
                        tabPanel("Recounted Group",tableOutput("contents_rcsplit_df")),
                        tabPanel("Recounted 2 Group",tableOutput("contents_rcsplit_df2")),
                        tabPanel("Plot Dat", tableOutput("contents_plot_dat")),
                        tabPanel("Running Plot",  plotOutput("run_plot_contents")),
                        tabPanel("Mean Change Plot", plotOutput("mean_plot_contents")),## should change the name - did not test yet
                        tabPanel("Overall Sequence Plot", plotOutput("sequence_plot_contents")), ## should change the name - did not test yet
                        tabPanel("Overall Data Descriptives",verbatimTextOutput("descriptive_contents")), # should be vanilla ...
                        tabPanel("Recounted Table",verbatimTextOutput("rc_tables_contents")), # should be vanilla only for the base recounted table
                        tabPanel("Grouped Data Descriptives ",verbatimTextOutput("descriptive_contents_group")), # should be vanilla only for the base recounted table

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

    # Display plot data
    output$contents_plot_dat <- renderTable({
        plot_dat()
    })


#### Display Plots ####
    ############### PLOTS #####

    output$run_plot_contents <- renderPlot({
        run_plot()  ## did not add to server yet
    })

    output$mean_plot_contents <- renderPlot({
        mean_plot()
    })

    output$sequence_plot_contents <- renderPlot({
        sequence_plot()
    })


#### Display Summary RC ####
    output$descriptive_contents <- renderPrint({
        descriptives()
    })

## summary descriptives for group
    output$descriptive_contents_group <- renderPrint({
        descriptives_group()
    })




#### Display Probabilities and Tables ####

### Tables

    output$rc_tables_contents <- renderPrint({
        rc_tables()
    })

### Probabilities
# output$rc_avg_prob_contents <- renderPrint({
#     rc_avg_prob()
# })


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

###### CLEAN DATA ########
    # # Filter out NA values
          # dat1 <<- eventReactive(input$button1, {
          #      tidyr::drop_na(dat1())
          #        })



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

    descriptives <<-reactive({
        Recounter2(behaviorstream(),
                   input$beh_var,
                   input$reinf_var,
                   actor = NULL,
                   missing_data = NULL)$descriptive_statistics
    })



    ## Recounted Tables ##
    #### for overall ####
rc_tables <<- reactive({
    tables_recount_table(rc_df())$output_list
})


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



## group descriptive statistics here

        descriptives_group <<- reactive({
            group_split_recounter_desc(split_df())
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


######## Plotting Functions ######
    # ### Run Plot (change name) #################
    observeEvent(c(input$run_runplot),{

         # create data for plotting
         plot_dat <<-reactive({
             plotting_restructure(rc_df())

         })


         # create actual ggplot
         run_plot <<- reactive({
             # Thus the average value changes for each sub-series (e.g., Before [red] takes over)
             ggplot(plot_dat(),aes(x = recount_stream_index, y = sub_series_run_prob,
                                 color = (recount_sequence), group = sub_series)) + geom_point() +facet_grid(~sub_series) +
                 ggtitle("Running Sequence Probabilities By Sub-Series")+
                 xlab("Observation Sequence") +
                 ylab("Running Probability")
         })

     }) ## Close button2

    # ### Mean Plot  #################
    observeEvent(c(input$run_meanplot),{

        # create data for plotting
        plot_dat <<-reactive({
            plotting_restructure(rc_df())

        })

        # create aggregate data
        plot_dat2 <<- reactive({
            plot_dat() %>% group_by(sub_series, recount_sequence) %>%
                summarize(sub_series_mean = mean(sub_series_run_prob)) %>% ungroup()
        })

        # create actual ggplot
        mean_plot <<- reactive({
            # Thus the average value changes for each sub-series (e.g., Before [red] takes over)
            ggplot(filter(plot_dat2(), recount_sequence != "R"),aes(x = sub_series, y = sub_series_mean, color = (recount_sequence))) + geom_point() + geom_line() +
                ggtitle("Average Sub-Series Probabilities By Sequence") +
                xlab("Sub-Series") +
                ylab("Average Sequence Probabilities")

        })

    }) ## Close button2

    #### Overall Sequence Plot

    observeEvent(c(input$run_sequenceplot),{

        # create data for plotting
        plot_dat <<-reactive({
            plotting_restructure(rc_df())

        })

        # create aggregate data
        plot_dat2 <<- reactive({
            plot_dat() %>% group_by(sub_series, recount_sequence) %>%
                summarize(sub_series_mean = mean(sub_series_run_prob)) %>% ungroup()
        })


        # create summary across sub-series
        overall_average <<- reactive({
            plot_dat2() %>%
                group_by(recount_sequence) %>%
                summarize(mean_sub_mean = mean(sub_series_mean)) %>% ungroup() %>% filter(recount_sequence != "R")

        })

        # merge the data frames

         average_df<<- reactive({
             left_join(plot_dat(), overall_average(), by = "recount_sequence")
             })

         # create actual ggplot
         sequence_plot <<- reactive({
        ggplot(average_df(),aes(x = recount_stream_index, y = sub_series_run_prob, color = (recount_sequence))) + geom_point() + geom_line(aes(y = mean_sub_mean)) +
            ggtitle("Overall Sequence Average") +
            xlab("Observation Sequence") +
            ylab("Running Probabilities of Target")
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
