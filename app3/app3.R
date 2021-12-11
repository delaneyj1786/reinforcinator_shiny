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
install_github("delaneyj1786/REINFORCINATOR")
library(ReenforcinateR)
library(tidyverse)

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
# actionButton("run_runplot", "Run Running Plot"), br(),
# actionButton("run_meanplot", "Run Mean Plot"), br(),
# actionButton("run_sequenceplot", "Run Overall Sequence Plot"), br(),
            actionButton("run_plots", "Run Plots"), br(),

            downloadLink("downloadData", "Download Recounted Data File"), br(),
            downloadLink("downloadData_1group", "Download Recounted Group Data File"), br(),
            downloadLink("downloadData_2group", "Download Recounted 2 Group Data File")
        ), # close sidebar panel

        # Show a plot of the generated distribution
        mainPanel(
            h1("Navigation"),
            br(),
            h2("Input Sidebar (Left hand side)"),
            "The left hand panel contains drop-down menus for the user to select the dataset and variables for their reinforcer analysis.",
            br(),
            "The user must select 'Confirm Data Selection' after choosing a dataset. The user then must click on
            (1) Run overall analsis for an overall reinforcer analysis or (2) Run Group Analysis (for a single cluster variable) or (3) Run Two-Group analysis (for 2 cluster variables)",
            br(),
            h2("Output Tabs"),
            br(),
            "The output is divided into 3 sections. ",
            br(),
            "Datasets : The purpose of this tab is to allow users to see that actual data anlyzed by the reinforcinator. The user can
            download each dataset by clicking on its corresponding link on the left hand sidebar.
            This tab contains (1) Data : a printout of the original data (see 'Data'), (2) Recounted Data : a printout of the transformed data which sorts behaviors before and after reinforcement,
            (3) Recounted Group Data: a printout of the transformed data with respect to a single clustering / grouping variable and (4) Recounted 2 Group : a printout
            of the transformed data with respect to a double clustering / grouping variable.",
            br(),
            "Plots : The plots tab contains 3 plots of the reinforcement data. The first tab (Plot Dat), shows the dataset used to create the 3 plots.
            (1) Running Plot : Shows the running probability of the dependent variable across the observation series. (2) Mean change plot : This shows the average
            value of the DV before (in red) and after (in green), each reinforcer, (3) Overall Sequence plot : This shows the
            average value before (in red) and after (in green) across all reinforcers.",
            br(),
            "2. Select Second Group Variable: Click on this tab to select the column pertaining to a second grouping variable (i.e., Target ID)",
            tabsetPanel(position = "above",
                        tabPanel("Data Sets", uiOutput("Tab1")),
                        tabPanel("Plots", uiOutput("Tab2")),
                        tabPanel("Descriptives", uiOutput("Tab3")),
                        tabPanel("Instructions", uiOutput("Tab4")) # To have a separate text/only panel just for instructions

            ) # close tabset panel
        ) # close main panel

        )   # close sidebar layout
) # close UI fluid

#### SERVER LOGIC ####
server <- function(input, output, session) {

## UI Tabs
    output$Tab1 <- renderUI({
        tabsetPanel(id = "sub_tab_one",
                    tabPanel("Data",tableOutput("contents")),
        tabPanel("Recounted Data",tableOutput("contents_rc")),
        tabPanel("Recounted Group",tableOutput("contents_rcsplit_df")),
        tabPanel("Recounted 2 Group",tableOutput("contents_rcsplit_df2")))
    })


## PLOT Tabs
    output$Tab2 <- renderUI({
    tabsetPanel(id = "sub_tab_two",
    tabPanel("Plot Dat", tableOutput("contents_plot_dat")),
    tabPanel("Running Plot",  plotOutput("run_plot_contents")),
    tabPanel("Mean Change Plot", plotOutput("mean_plot_contents")),## should change the name - did not test yet
    tabPanel("Overall Sequence Plot", plotOutput("sequence_plot_contents"))) ## should change the name - did not test yet
})


## Data Tabs
    output$Tab3 <- renderUI({
        tabsetPanel(id = "sub_tab_three",
                    tabPanel("Overall Data Descriptives",verbatimTextOutput("descriptive_contents")), # should be vanilla ...
                    tabPanel("Recounted Table",verbatimTextOutput("rc_tables_contents")), # should be vanilla only for the base recounted table
                    tabPanel("Grouped Data Descriptives ",verbatimTextOutput("descriptive_contents_group")) # should be vanilla only for the base recounted table
        )
    })

    ## Data Tabs
    output$Tab4 <- renderUI({
        tabsetPanel(id = "sub_tab_four",
                    tabPanel("Background",verbatimTextOutput("backround_text")), # should be vanilla ...
                    tabPanel("Inputting Data",verbatimTextOutput("")), # should be vanilla only for the base recounted table
                    tabPanel("Interpreting Output ",verbatimTextOutput("")) # should be vanilla only for the base recounted table
        )
    })


## Load User CSV
    csv<<-reactive({
        read_csv(input$file1$datapath)
    })

## Pre Load Data
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
##  Display Original Data
    output$contents <- renderTable({
        dat1()
    })
## Display recounted data
    output$contents_rc <- renderTable({
        rc_df()
    })
## Display recounted group split df
    output$contents_rcsplit_df <- renderTable({
        recount_split_df()
    })
## Display 2 recounted group split df
    output$contents_rcsplit_df2 <- renderTable({
        recount_split_df2()
    })
## Display plot data
    output$contents_plot_dat <- renderTable({
        plot_dat()
    })
### Display Plots ####
## Running Plot
    output$run_plot_contents <- renderPlot({
        run_plot()  ## did not add to server yet
    })
## Sub Series Plot
    output$mean_plot_contents <- renderPlot({
        mean_plot()
    })
## Overall Sequence Average plots
    output$sequence_plot_contents <- renderPlot({
        sequence_plot()
    })
### Display Descriptive Stats ####
## Overall
    output$descriptive_contents <- renderPrint({
        descriptives()
    })

## Group Based Descriptives
    output$descriptive_contents_group <- renderPrint({
        descriptives_group()
    })
### Display Recounted Probabilities  ####

### Tables
    output$rc_tables_contents <- renderPrint({
        rc_tables()
    })

### Text
    output$backround_text <-renderText({

        "Inputting Data.

        The user has two options for inputting data : inputting their own dataset (Choose CSV file) or using
        a built-in data set from the Reenforcinator R package (choose dataset). After selecting one of these options, the user
        MUST CLICK ON 'Confirm Data Selection' button for the dataset to load for analysis.

            1. Choose a CSV File : Click on the drop down menu to select a file from your hard-drive.
        The file MUST be csv format.

            2. Choose a dataset : Click on the drop-down menu to select
        one of the 7 pre-loaded datasets

            Selecting Variables (Basic Analysis)

            1. Select Behavior Stream Column : This is the column of the dataframe that contains behaviorall
        observations (i.e., observations of the Independent and Dependent variables).

            2. Select Target Behavior (DV): This is the code pertaining to the 'reinforced behavior', that is, the behavior expected to
        change as a function of reinforcement.

            3. Select Target Consequence (IV): This is the code pertaining to the 'putative reinforcer', the event that is
        expected to change the trajectory of the Dependent variable.

            Selecting Variables (Group Analysis)

            In addition to the overall analysis, the Re-enforcinator allows users to calculate change within clusters of observations.
        In a single group analysis, reinforcement is calculated within each unique value of the grouping variable. For example,
        in the Two_Person dataset, the user can look for reinforcement in each separate Video (i.e., VIDELT).

        A two-group analysis extends this idea to two clustering variables. For example, in the two-person dataset, the two-group analysis can calculate
        reinforcement separately for each VIDLET WITHIN each unique Target individual (using the 'TAR' variable).

            1. Select Group Variable: Click on this tab to select the column pertaining to a grouping variable (i.e., video segment or user ID).

            2. Select Second Group Variable: Click on this tab to select the column pertaining to a second grouping variable (i.e., Target ID)

        "


    })

#### Sidebar interface ####
## Behavior Stream
    observe({
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "beh_stream",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    })
## Behavior var (based on behavior stream levels)
# https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))
        updateSelectInput(session, "beh_var",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })
## Consequence Var
    observeEvent(input$beh_stream,{ # Probably redundant
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))
        updateSelectInput(session, "reinf_var",
                          label = NULL,
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })
## Group names (single group)
    observe({
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "group_var",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    })
## Group names (two groups)
    observe({
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "group_var2",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    })

#### Activate Reinforcinator (Overall Analysis) ####
    observeEvent(c(input$button2,input$beh_var,input$reinf_var,input$beh_stream),{
        # Create Behavior Stream
        behaviorstream<<-eventReactive(input$button2,{
            (((dat1()[[input$beh_stream]])))
        })
        # Create RC Df
        rc_df<<-reactive({
            Recounter2(behaviorstream(),
                       input$beh_var,
                       input$reinf_var,
                       actor = NULL,
                       missing_data = NULL)$recounted_data_frame
        })
    }) # End observeEvent

## Overall Descriptives
    descriptives <<-reactive({
        Recounter2(behaviorstream(),
                   input$beh_var,
                   input$reinf_var,
                   actor = NULL,
                   missing_data = NULL)$descriptive_statistics
    })
## Recounted Tables (Overall Analysis)
rc_tables <<- reactive({
    tables_recount_table(rc_df())$output_list
})

#### Activate Reinforcinator (Group 1 Analysis) ####
    observeEvent(c(input$button3,input$beh_var,
                   input$reinf_var,input$beh_stream, input$group_var),{

        # Create Behavior Stream
        behaviorstream<<-eventReactive(input$button3,{
            (((dat1()[[input$beh_stream]])))
        })
        # Create Split DF (1 group)
        split_df<<-reactive({
            group_splitter(dat1(),
                       behaviorstream(),
                       input$beh_var,
                       input$reinf_var,
                       input$group_var,
                       actor = NULL)
        })
        # Create Recount Split DF (1 group)
        recount_split_df<<-reactive({
            group_split_recounter(
                split_df(),
                input$beh_stream,
                input$beh_var,
                input$reinf_var,
                input$group_var
            )
        })

        # Create Descriptives (1 group)
        descriptives_group <<- reactive({
            group_split_recounter_desc(
                split_df(),
                input$beh_stream,
                input$beh_var,
                input$reinf_var,
                input$group_var
                )
        })


    }) ## Close ObserveEvent

#### Activate Reinforcinator (Two Group Analysis) ####
    observeEvent(c(input$button4,input$beh_var,input$reinf_var,input$beh_stream,
                   input$group_var, input$group_var2),{

        # Create Behavior Stream
        behaviorstream<<-eventReactive(input$button4,{
            (((dat1()[[input$beh_stream]])))
        }) # close behavior stream

        # Create Split DF (2 group)
        split_df2<<-reactive({
            group_splitter2(dat1(),
                           behaviorstream(),
                           input$beh_var,
                           input$reinf_var,
                           input$group_var,
                           input$group_var2,
                        filt = TRUE)
        })

        # Create Recount DF (2 group)
        recount_split_df2<<-reactive({
            group_split_recounter(
                split_df2(),
                input$beh_stream,
                input$beh_var,
                input$reinf_var,
                input$group_var  ## techincally ... actor i think
            )
        })

    }) ## ObserveEvent

#### Plotting ####
## Run Plot (change name)
    observeEvent(c(input$run_plots),{
         # Create Plot Data (RC DF)
         # Overall
         plot_dat <<-reactive({
             plotting_restructure(rc_df())

         })
         # Create Overall GGplot
         run_plot <<- reactive({
             # Thus the average value changes for each sub-series (e.g., Before [red] takes over)
             ggplot(plot_dat(),aes(x = recount_stream_index, y = sub_series_run_prob,
                                 color = (recount_sequence), group = sub_series)) + geom_point() +facet_grid(~sub_series) +
                 ggtitle("Running Sequence Probabilities By Sub-Series")+
                 xlab("Observation Sequence") +
                 ylab("Running Probability")
         })

## Mean Sub-Series Plot
        # Aggregate Sub Group Data
        # Overall
        plot_dat2 <<- reactive({
            plot_dat() %>% group_by(sub_series, recount_sequence) %>%
                summarize(sub_series_mean = mean(sub_series_run_prob)) %>% ungroup()
        })

        # Create Sub-Series Average Plot
        mean_plot <<- reactive({
            ggplot(filter(plot_dat2(), recount_sequence != "R"),aes(x = sub_series, y = sub_series_mean, color = (recount_sequence))) + geom_point() + geom_line() +
                ggtitle("Average Sub-Series Probabilities By Sequence") +
                xlab("Sub-Series") +
                ylab("Average Sequence Probabilities")
        })

## Overall Mean Plot
        # Create Plot Data
        # Aggregate Sub Group Data
        # Aggregate Across Sub-series
        # Overall
        overall_average <<- reactive({
            plot_dat2() %>%
                group_by(recount_sequence) %>%
                summarize(mean_sub_mean = mean(sub_series_mean)) %>% ungroup() %>% filter(recount_sequence != "R")

        })
        # Merge Plot Dat (original) with Overall Agregate (Collapsed Plot_dat2)
         average_df<<- reactive({
             left_join(plot_dat(), overall_average(), by = "recount_sequence")
             })

         # Create Sequence Average Plot
         sequence_plot <<- reactive({
        ggplot(average_df(),aes(x = recount_stream_index, y = sub_series_run_prob, color = (recount_sequence))) + geom_point() + geom_line(aes(y = mean_sub_mean)) +
            ggtitle("Overall Sequence Average") +
            xlab("Observation Sequence") +
            ylab("Running Probabilities of Target")
         })


    }) ## Close ObserveEvent

#### Downloading ####
    # Download Recounted DF (Overall)
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
} # Close Server

# Run the application
shinyApp(ui = ui, server = server)
