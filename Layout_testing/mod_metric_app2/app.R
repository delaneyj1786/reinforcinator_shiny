# https://github.com/emilyriederer/demo-shiny-modules/blob/master/mod-metr.R

library(shiny)
source("mod_metric.R")


metric_demo <- function() {

  df <- data.frame(day = 1:30, arr_delay = 1:30)
  ui <- fluidPage(metric_ui("x"))
  server <- function(input, output, session) {
    metric_server("x", reactive({df}), "arr_delay", 15)
  }
  shinyApp(ui, server)

}
