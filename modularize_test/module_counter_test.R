# https://shiny.rstudio.com/articles/modules.html
library(shiny)

counterButton_UI <- function(id, label = "Counter") { # UI function
  ns <- NS(id) # create namespace function
  tagList(
    actionButton(ns("button"), # all input ids need wrapped in ns()
                 label = label),
    verbatimTextOutput(ns("out")) # same for output ID
  )
}

counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}
