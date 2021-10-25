

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
      br(),
      br(),
      tabsetPanel(position = "above",
                  tabPanel("Data",tableOutput("contents")),
                  tabPanel("Recounted Data",tableOutput("contents_rc")),
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
    recount_tab_bind()
  })

  # Display Average Contingency Table
  output$contents_avg_tab <- renderPrint({
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
      Recounter2(behaviorstream(),
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
