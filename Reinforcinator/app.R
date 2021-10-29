## Updating input from column options
### https://stackoverflow.com/questions/42199115/error-in-r-object-of-type-closure-is-not-subsettable
#https://stackoverflow.com/questions/37887482/filtering-from-selectinput-in-r-shiny
#
# source("init.R")
# source("funs.R")
#
# # datasets
# elevator<-read_csv("E2.csv")
# picture_stream<-read_csv("mainstream1.csv")
# two_person_picture<-read_csv("sample_stream_2.csv")
#
# # more datasets
# noeffect<-read_csv("reinforcer_sample_noeffect.csv")
# punishment<-read_csv("reinforcer_sample_PUNISHMENT.csv")
# reinforcement<-read_csv("reinforcer_sample_reinforcement.csv")

library(ReenforcinateR)
library(tidyverse)
library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reinforcinator"),
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
            # https://stackoverflow.com/questions/34596286/create-selection-list-based-on-the-column-names-of-a-csv-file-for-plotting-in-sh
            # Input: Selector for choosing dataset ----
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
            br(),
            br(),
            selectInput(inputId = "beh_stream",
                        label = "Select behavior stream column:",
                        choices = "Nothing Selected"),  ## needs to be updated with the dataset behavior column

            selectInput(inputId = "beh_var",
                        label = "Select target behavior:",
                        choices = "Nothing Selected"),  ## needs to be updated with the dataset behavior column

            selectInput(inputId = "reinf_var",
                        label = "Select target consequence:",
                        choices = "Nothing Selected"),

            selectInput(inputId = "actor_var",
                        label = "Select nesting variable for regression analysis:",
                        choices = "Nothing Selected"),

            actionButton("button2", "Run Analysis"),
            br(),
            br(),
            downloadLink("downloadData", "Download Recounted Data File")

        ), # end sidebar

        # Multiple Panel Design
        mainPanel(
            "Reinforcer Analysis Schematic",
            br(),
            img(src="reinforcer_stream.png",width = 600,
                height = 300, alt = "schematic for reinforcer analysis"),
            br(),
            h1("Background"),
            br(),
            "This applet performs a reinforcer analysis, as described in James DeLaney's Master's thesis. For a walkthrough of this analysis, click the following link",
            a("Master's thesis vignette.",target ="_blank", href= "reinforcinator_vignette_thesis_data.html"),
            "For a short vignette on a sample of one person, please",
            a("click here",target ="_blank", href= "reinforcinator_vignette_sample_one.html"),
            "For a short vignette using a sample of two people, that illustrates the use of generalized estimating equations, please",
            a("click here",target ="_blank", href= "reinforcinator_vignette_sample_two.html"),
            br(),
            br(),
            h1("Instructions"),
            br(),
            h4("Step 1 : Selecting the dataset"),
            br(),
            "The Reinforcinator allows users to load a CSV file from the side panel. This is called the 'input_file' in the 'Choose a dataset' menu.
     Alternatively, the Reinforcinator comes with three pre-loaded datasets
     for the user to try. If the user wishes to try the Reinforcinator with a pre-loaded dataset.
   Once the user selects a dataset they must click on the 'Confirm  Data Selection button' to update the rest of the menu items." ,
            h4("Step 2 : Selecting the Behavior Stream Column"),
            br(),
            "After confirming the datset, the user can select any column which contains the behavior stream. All of the behaviors in this stream will provide the basis for the reinforcement.",
            h4("Step 3 : Selecting Behaviors and Reinforcers"),
            br(),
            "Select the behavior that is a candidate for reinforcement by choosing one of the options in the 'Select target Behavior' tab. .
   Select the behavior that is a potential or putative reinforcer by choosing one of the options in the 'Select target Behavior' tab. ",
            h4("Step 4 : Selecting nesting variable for the GEE regression analysis (optional)"),
            br(),
            "Select a nesting variable for which observations are nested under (i.e., behaviors by focal student). This will be used in the regression analysis to control for repeated measures under the selected unit. Note, the Reinforcinator uses the geepack package, and the nesting variable populates the 'Waves' argument of the GEE function. ",
            h4("Step 5: Run Analysis"),
            "Select the 'Run Analysis' button in order to update the results tabs below.",
            br(),
            h1("Interpreting the Results"),
            "There are 10 tabs below which each display a result of the analysis.
    The Data tab will display the CSV file and the Recounted Data File Tab will display the recounted data file (please refer to the glossary for definitions of these terms.
    All of the contingency tables and contingency table tests are accessed in their respective tabs. For a regression analysis using generalized estimated equations for nested observations (i.e., observations in focal students),
    please select a nesting variable and click on the Regression Summary tab. ) ",
            br(),
            h4("Interpreting GEE Regressions"),
            "Based on Allison and Liker (1981), the contingency table z-tests use the following equation to compare the probability of
    target behaviors to chance (defined by base rate proportion of the target behavior):  ",
            withMathJax(
                helpText("$$z(1) = \\frac{(P(T|A)-P(T)}{\\sqrt(\\frac{P(T)*(1-P(T))*(1-P(A))}{(N-k)*P(A)}}$$"

                         #   helpText("$$\\frac{z(1)}{2}$$"
                )
            ),
            "Where P(T|A) is the probability of a target behavior given the after reinforcement sequence, P(T) is the overall probability of a target behavior,
    and P(A) is the overall probability of behaviors in the reinforcement sequence. These values are obtained by the respective cells in the contingency table and the values
    are provided in the 'liker Score' summary table",
            br(),
            h4("Interpreting the GEE output"),
            "The following conversion can be used to obtain the difference between target behavior probability before and aafter reinforcement using the following equation
   to convert the beta coefficients to probabilies: ",
            withMathJax(
                helpText("$$P(y=1|x_{i}) =  \\frac{e^{\\beta_{0}+\\beta_{1}x}}{1+e^{\\beta_{0}+\\beta_{1}x}}$$"
                )
            ),
            br(),
            h1("Description of Example Datasets"),
            h4("Elevator Data"),
            "The elevator dataset consists of 119 observations of a contrived 'button pressing' experiment, wherein a participant presses (or does not press) the call
   button for the elevator to arrive.
   Observations are spread across 4 days,
   and each day has a different number of continuous observation episodes.
   Each episode occurs in a particular context and for a particular participant.
   This dataset is intended to showcase the various abilities of the Reinforcinator to
   control for various contexts with the regression analysis, as well as provide an illustration of
   how the recounted observations work in a dataset with high base rate of potential reinforcers",
            h4("Picture Data"),
            "The Picture dataset comprises 25 observations across all students in a classroom. Target behaviors can correspond
   to hypothesis one of research question one in James' DeLaney's thesis study, namely, does On-Task behavior (green) change as
   a function of Social Approval (Red). The coloring scheme uses green for target behaviors, yellow for non-target behaviors, and
   red for potentially reinforcing behaviors. This dataset is intended to provide a low-sample look at how observations are re-coded for each
   'reinforcement phase' or sub-series of observations.Note, that this dataset will not work with the regression analysis (as of version 1), due to a single level of the nesting variable (e.g., Actor).",
            h4("Two Person Picture Data"),
            img(src="reinforcer_stream_two_person.png",width = 600,
                height = 300, alt = "schematic for reinforcer analysis"),
            br(),
            "The two_person Picture dataset comprises a total of 30 observations across two students in a classroom. The patterns are the same for both students.
 This dataset is intended to provide a low-sample look at how the regression anaylsis accounts for repeated measures within a nesting variable, in this case, 'Actor' is the nesting variable.",

            h4("Reinforcer Example"),
            "This dataset contains 10 observations of a single reinforcer where the probability is greater after reinforcement for target behaviors. ",
            h1("Limitations"),
            "The version 1 of the Reinforcinator is limitted mainly in the file upload size. Files larger than 100kb may take substantial time loading. Currently,
    the app lacks a progress indicator, but even larger files should not take longer than 3 minutes to upload. Once the file is uploaded, the user must confirm the dataset in order to populate the rest of the menu items.
    This applet also only supports one nesting variable for the GEE regression analysis.",

            # https://stackoverflow.com/questions/29936561/how-do-i-add-a-link-to-open-a-pdf-file-in-a-new-window-from-my-r-shiny-app
            tabsetPanel(position = "above",
                        tabPanel("Data",tableOutput("contents")),
                        tabPanel("Recounted Data",tableOutput("contents_rc")),
                        tabPanel("Glossary",
                                 br(),
                                 h4("Recounted Observations"),
                                 "recount_stream : This is the behavior stream from the original datafile that has been replicated (i.e., recounted)
                       NxM number of times, where N is the number of original observations and M is the number of observed (potential) reinforcers.",       # https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
                                 br(),
                                 br(),
                                 ("sub_series: This refers to each iteration of a recounted series, corresponding to each reinforcer in the original datafile.
                       For example, sub-series 2 refers to the second replication (counting) of the behavior stream.
                       There are M sub-series, corresponding to M reinforcers.") ,
                                 br(),
                                 br(),
                                 ("recount_stream_index: This is the index of the original observation stream. It resets once the number of original observations has been reached. It should be used in conjunction with sub-series to create an index for the recounted observations
                       (i.e., Sub-Series 2, observation 3 refers to the 13th observation for a series of 10 with two reinforcers."),
                                 br(),
                                 br(),
                                 ("recount_recode_stream: This is the binary (i.e., Target Behavior, Non-Target Behavior), re-classification scheme of
                       the recounted observations. It is used to build the regression and contingency table models along with recount_sequence and regression_recount_sequence to classify Reinforcers as missing values."),
                                 br(),
                                 br(),
                                 ("recount_sequence: This is the trinary (i.e., Before reinforcer, After Reinforcer, Reinforcer), re-classification scheme of
                       the recounted observations, to identify an observation relative to a reinforcer, for a given sub-series. It is used to build the regression and contingency table models along with regression_recount_sequence to classify Reinforcers as missing values."),
                                 br(),
                                 br(),
                                 ("recount_recode_numeric: This is a numeric conversion of recount_recode_stream used for binomial z-tests along with recount_sequence to identify the reinforcer position for a given sub-series. "),
                                 br(),
                                 br(),
                                 ("regression_recount_sequence: This is the recount_sequence that has removed reinforcer for a given sub-series for the regression analysis. For example, reinforcer 1 for sub-series 1 is removed (coded as NA for subseries 1),
                       but counts as non-target for all other sub-series."),
                                 br(),
                                 h4("Contingency Tables"),
                                 "Contingency tables comprise a frequency and probability table of the behavior counts according to reinforcement sequence (i.e., After and Before), and
                     status as a target behavior. Probabilities are reinforcers. The probabilities are below the frequencies and contain the .1 suffix. All probabilities are
                     obtained by dividing the counts by the row margins (i.e., Reinforcement Series.",
                                 "Recounted Table: The Recounted table is a cross-classification of Target Behavior (i.e., Not Target or Target) by Reinforcer Sequence (i.e., after or before) for the Recounted series of observations.",
                                 "Average Table: The Average table is a cross-classifcation of Target Behavior by Reinforcement Sequence for the Recounted Table divided by the total number of reinforcing events",
                                 "Recomputed Table: The Recomputed table is a cross-classification Target Behavior by Reinforcement Sequence where the cell values are obtained in the following steps. 1) Each sub-series is broken into a contingency table for the row-conditioned probabilities. 2) Each cell is multiplied by the corresponding value from the Average Contingency Table. 3. The values of each sub-series are added together for a summary table of the 'Recomputed Frequencies'.  ",
                                 br(),
                                 h4("Regression Summary"),
                                 "The Regression analysis summary reports the results of a GEE analysis using the geepack package. This specifies a general linear model for non-normal distributions and uses generalized estimating equations for repeated
                        observations.
                        All models use the nesting / clustering variable as an 'ID' and sub-series as a 'wave',
                        for which observations of the same unit are ordered for
                        repeated measures. The regression is specified for a binomially distributed outcome variable."
                        ),
                        #           tabPanel("html: Recounted Data", htmlOutput("text")),
                        tabPanel("Descriptive Statistics: DataFrame",verbatimTextOutput("contents_descriptives")),
                        tabPanel("Contingency Tables: Recounted",verbatimTextOutput("contents_rc_tab")),
                        tabPanel("Contingency Tables: Average",verbatimTextOutput("contents_avg_tab")),
                        tabPanel("Contingency Tables: Recomputed",verbatimTextOutput("contents_recomp_tab")),
                        tabPanel("Liker Z score: Recounted",verbatimTextOutput("contents_recount_tab_liker")),
                        tabPanel("Liker Z score: Average",verbatimTextOutput("contents_avg_tab_liker")),
                        tabPanel("Liker Z score: Recomputed",verbatimTextOutput("contents_recomp_tab_liker")),
                        tabPanel("Regression: DataSet",tableOutput("contents_regression")),   #
                        tabPanel("Regression: Summary",verbatimTextOutput("contents_regression_sum"))   # change to verbatimTextOutput

                        #    tabPanel("Regression Analysis",tableOutput("contents"))   output$contents_avg_tab_chisq
            ) #end tabset panel
        ) # end main panel
    ) #end sidebar layout
)

# Define server logic required to draw a histogram  output$contents_rc_tab
server <- function(input, output,session) {

    # create data frame
    #    csv<<-eventReactive(input$file1,{
    #        read_csv(input$file1$datapath)
    #    })

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

    # Display reinforcer data
    output$contents_rc <- renderTable({
        rc_df()
    })



    # Table Displays *********************
    # Dispay Descriptives
    output$contents_descriptives <- renderPrint({
        rc_df_descriptives()
    })


    # Display Recount Contingency Table
    output$contents_rc_tab <- renderPrint({
        #      (sum_recount_tab()) %>%  addmargins()
        # strange work around
        #xtabs(Freq ~ Sex+Color, data=countdf)  # http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/
        #Var1	Var2	Freq
        #     sum_recount_tab()
        recount_tab_bind()
    })

    # Display Average Contingency Table
    output$contents_avg_tab <- renderPrint({
        #   table_arrange(sum_avg_tab()) %>%  addmargins()
        #    xtabs(Freq~Var1+Var2,sum_avg_tab())
        #     sum_avg_tab()

        avg_tab_bind()
    })

    # Display Recomp Contingency Table
    output$contents_recomp_tab <- renderPrint({
        #   table_arrange(sum_recomp_tab()) %>%  addmargins()
        recomp_tab_bind()
    })
    #*************************************
    # Allison and Liker Z **************
    # display recount liker
    output$contents_recount_tab_liker <- renderPrint({
        #  allison_z_recount()
        # allison_z_recount
        recount_z_list()
    })

    # display avg liker
    output$contents_avg_tab_liker <- renderPrint({
        # chi_recomp()[[1]]
        #allison_z_average()
        avg_z_list()
    })

    # display RECOMP liker
    output$contents_recomp_tab_liker <- renderPrint({
        # chi_recomp()[[1]]
        # allison_z_recomp()
        recomp_z_list()
    })
    #*****************
    # Regression Displays
    ## data frame
    output$contents_regression <-  renderTable({                #change to renderPrint({
        # chi_recomp()[[1]]
        rc_df_regression_nomissing_actor()
    })
    ## summary
    output$contents_regression_sum<- renderPrint({rc_df_regression_nomissing_actor_summary()})


    # SIDEBAR NAV ****************
    ## update options for behavior stream based on all columns of dataframe
    #https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
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
    })

    # update behavior options based on column levels
    # https://stackoverflow.com/questions/47248534/dynamically-list-choices-for-selectinput-from-a-user-selected-column
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))

        #      if(input$beh_stream== "Nothing Selected"){
        #        return()
        #      }

        updateSelectInput(session, "beh_var",
                          label = NULL,
                          # choices = unique(dat1()$beh_stream[dat1()$beh_stream==input$beh_stream]),
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

    # update reinforcer options based on column levels
    observeEvent(input$beh_stream,{
        column_levels <- as.character(sort(unique(dat1()[[input$beh_stream]])))

        #      if(input$beh_stream== "Nothing Selected"){
        #        return()
        #      }

        updateSelectInput(session, "reinf_var",
                          label = NULL,
                          # choices = unique(dat1()$beh_stream[dat1()$beh_stream==input$beh_stream]),
                          choices =  column_levels ,
                          selected = "Nothing Selected")
    })

    ## Select Actor Var
    observe({
        # requires file 1
        #      req(input$file1)
        dsnames <- names(dat1())
        cb_options <- list()
        cb_options[dsnames] <- dsnames
        updateSelectInput(session, "actor_var",
                          label = NULL,
                          choices = cb_options,
                          selected = "")
    })




    ## Activate Reinforcinator
    observeEvent(c(input$button2,input$beh_var,input$reinf_var,input$beh_stream),{

        # create data frame
        behaviorstream<<-eventReactive(input$button2,{
            (((dat1()[[input$beh_stream]])))
        })

        ## This Runs and shows the data frame
        # hopefully this works with the tidy stuff ...
        rc_df<<-reactive({
            recounter(behaviorstream(),
                       input$beh_var,
                       input$reinf_var,
                       actor = NULL,
                       missing_data = NULL)$recounted_data_frame
        })

        ## Descriptive Stats
        rc_df_descriptives <<-reactive({
                       input$beh_var,
                       input$reinf_var,
                       actor = NULL,
                       missing_data = NULL)$descriptive_statistics
        })

        ## Add Contingency Tables

        # create sum recount f tab
        sum_recount_tab <<-reactive({
            table_arrange(tables_recount_table(rc_df())$output_list$sum_recount_table) %>% addmargins()
            #  f_tab_rc_thesis_rq1_h1 <- table_arrange(contingency_tables_thesis_rq1_h1$output_list$sum_recount_table)  %>%  addmargins()

        })
        # create Prop recount f tab
        sum_recount_tab_prop <<-reactive({
            prop.table(table_arrange(tables_recount_table(rc_df())$output_list$sum_recount_table),1) %>% addmargins()
        })

        #Concatenate for printout
        recount_tab_bind<<-reactive({data.frame(rbind(sum_recount_tab(),sum_recount_tab_prop()))})

        ## Table test for recount
        aft_tar_recount<<-reactive({sum_recount_tab()[1,1]})
        bef_tar_recount<<-reactive({sum_recount_tab()[2,1]})
        aft_tot_recount<<-reactive({sum_recount_tab()[1,3]})
        bef_tot_recount<<-reactive({sum_recount_tab()[2,3]})

        ## Probabilities for the alison and liker test
        pta_recount <<- reactive({recount_tab_bind()[4,1]})
        ptt_recount <<-  reactive({
            (recount_tab_bind()[1,1]) / (recount_tab_bind()[3,3])
        })
        pa_recount <<-reactive({
            (recount_tab_bind()[1,3]) / (recount_tab_bind()[3,3])
        })

        #ns
        nta_recount <<- reactive({recount_tab_bind()[1,1]})#

        na_recount <<- reactive({recount_tab_bind()[1,3]})#

        n_recount <<- reactive({(avg_tab_bind()[3,3])})

        n_recount <<-reactive({
            (recount_tab_bind()[3,3])
        })

        # z for recount
        allison_z_recount <<- reactive({allison_liker_z(pta_recount(),
                                                        ptt_recount(),
                                                        pa_recount(),
                                                        n_recount(),1) })

        #  reactive({names(allison_z_recount())<<- "rdsdsf"})

        # p for z recount
        pvalue_recount = reactive({
            2*pnorm(abs(as.numeric(allison_z_recount())), lower.tail = F)
        })


        ## NEED ADD IN P (BEFORE)
        recount_z_list <<- reactive({
            list(allison_liker_z_score =allison_z_recount(),
                 p_value = pvalue_recount(),
                 probability_target_after_reinforcementr=pta_recount(),
                 probability_target_overall=ptt_recount(),
                 probability_after_reinforcement_overall=pa_recount(),
                 n_after_reinforcement =na_recount(),
                 n_target_after_reinforcement = nta_recount(),
                 sample_of_events = n_recount())
        })




        # AVG *************************
        # create sum average f tab
        sum_avg_tab <<-reactive({
            table_arrange(tables_recount_table(rc_df())$output_list$average_table) %>% addmargins()
        })

        # create Prop recount f tab
        sum_avg_tab_prop <<-reactive({
            prop.table(table_arrange(tables_recount_table(rc_df())$output_list$average_table),1) %>% addmargins()
        })

        #Concatenate for printout
        avg_tab_bind<<-reactive({data.frame(rbind(sum_avg_tab(),sum_avg_tab_prop()))})

        ## Table test for AVG
        aft_tar_avg<<-reactive({sum_avg_tab()[1,1]})
        bef_tar_avg<<-reactive({sum_avg_tab()[2,1]})
        aft_tot_avg<<-reactive({sum_avg_tab()[1,3]})
        bef_tot_avg<<-reactive({sum_avg_tab()[2,3]})



        ## Probabilities for the alison and liker test
        pta_average <<- reactive({avg_tab_bind()[4,1]})

        ptt_average <<-  reactive({
            (avg_tab_bind()[1,1]) / (avg_tab_bind()[3,3])
        })
        pa_average <<-reactive({
            (avg_tab_bind()[1,3]) / (avg_tab_bind()[3,3])
        })

        # N's
        nta_average <<-  reactive({avg_tab_bind()[1,1]})# n target and after
        na_average <<-  reactive({avg_tab_bind()[1,3]})# n after all
        n_average <<-reactive({
            (avg_tab_bind()[3,3])
        })

        allison_z_average <<- reactive({allison_liker_z(pta_average(),
                                                        ptt_average(),
                                                        pa_average(),
                                                        n_average(),1)})


        #  reactive({names(allison_z_recount())<<- "rdsdsf"})

        # p for z average
        pvalue_avg = reactive({
            2*pnorm(abs(as.numeric(allison_z_recount())),
                    lower.tail = F)
        })

        # n reinforcers
        #    reinf <<- reactive({
        #     rc_df_descriptives$n_reinf})

        avg_z_list <<- reactive({
            list(allison_liker_z_score =allison_z_average(),
                 p_value = pvalue_avg(),
                 probability_target_after_reinforcementr=pta_average(),
                 probability_target_overall=ptt_average(),
                 probability_after_reinforcement_overall=pa_average(),
                 n_after_reinforcement = na_average(),
                 n_target_after_reinforcement = nta_average(),
                 sample_of_events = n_average()

            )
        })

        # RECOMP *************************
        # create sum recount f tab
        sum_recomp_tab <<-reactive({
            table_arrange(tables_recount_table(rc_df())$output_list$recompute_frequencies) %>% addmargins()
        })
        # create Prop recount f tab
        sum_recomp_tab_prop <<-reactive({
            prop.table(table_arrange(tables_recount_table(rc_df())$output_list$recompute_frequencies),1) %>% addmargins()
        })

        #Concatenate for printout
        recomp_tab_bind<<-reactive({
            data.frame(rbind(sum_recomp_tab(),sum_recomp_tab_prop()))

        })

        ## Table test for chi recomp
        aft_tar_recomp<<-reactive({sum_recomp_tab()[1,1]})
        bef_tar_recomp<<-reactive({sum_recomp_tab()[2,1]})
        aft_tot_recomp<<-reactive({sum_recomp_tab()[1,3]})
        bef_tot_recomp<<-reactive({sum_recomp_tab()[2,3]})




        ## Probabilities for the alison and liker test
        pta_recomp <<- reactive({recomp_tab_bind()[4,1]})
        ptt_recomp <<-  reactive({
            (recomp_tab_bind()[1,1]) / (recomp_tab_bind()[3,3])
        })
        pa_recomp <<-reactive({
            (recomp_tab_bind()[1,3]) / (recomp_tab_bind()[3,3])
        })




        # N's
        nta_recomp <<-  reactive({recomp_tab_bind()[1,1]})# n target and after
        na_recomp <<-  reactive({recomp_tab_bind()[1,3]})# n after all
        n_recomp <<-reactive({
            (recomp_tab_bind()[3,3])
        })



        allison_z_recomp <<- reactive({allison_liker_z(pta_recomp(),
                                                       ptt_recomp(),
                                                       pa_recomp(),
                                                       n_recomp(),1)})


    })
    # p for z recomp
    pvalue_recomp = reactive({
        2*pnorm(abs(as.numeric(allison_z_recomp())),
                lower.tail = F)
    })

    recomp_z_list <<- reactive({
        list(allison_liker_z_score =allison_z_recomp(),
             p_value = pvalue_recomp())
    })



    recomp_z_list <<- reactive({
        list(allison_liker_z_score =allison_z_recomp(),
             p_value = pvalue_recomp(),
             probability_target_after_reinforcementr=pta_recomp(),
             probability_target_overall=ptt_recomp(),
             probability_after_reinforcement_overall=pa_recomp(),
             n_after_reinforcement = na_recomp(), ###
             n_target_after_reinforcement = nta_recomp(),
             sample_of_events = n_recomp()

        )
    })

    ## Add Regressions
    # GLM Shiny
    #    https://stackoverflow.com/questions/41141157/logistic-regression-through-r-shiny

    ## need to update behaviorstream
    # create data frame
    behaviorstream_actor<<-eventReactive(input$button2,{
        (((dat1()[[input$actor_var]])))
    })

    rc_df_regression <<- eventReactive(input$actor_var,{
        Recounter2(behaviorstream(),
                   input$beh_var,
                   input$reinf_var,
                   behaviorstream_actor(),
                   missing_data = NULL)$recounted_data_frame

    })
    rc_df_regression_nomissing <<-reactive({filter(rc_df_regression(), !is.na(regression_recount_sequence ))})
    rc_df_regression_nomissing_actor <<-reactive({filter(rc_df_regression_nomissing(), !is.na(recount_actor))})

    rc_df_regression_nomissing_actor_summary_form <<- reactive({geeglm(recount_recode_numeric~regression_recount_sequence,
                                                                       id = recount_actor,
                                                                       waves = sub_series,
                                                                       data = rc_df_regression_nomissing_actor(),
                                                                       family = binomial)
    })
    rc_df_regression_nomissing_actor_summary <<- reactive({print(summary(rc_df_regression_nomissing_actor_summary_form()))})




    #download  rc_df
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(rc_df(), file)
        }
    )

}
# Run the application
shinyApp(ui = ui, server = server)
