# https://github.com/emilyriederer/demo-shiny-modules/blob/master/mod-metr.R
# metric module ----
metric_ui <- function(id) {
  
  fluidRow(
    text_ui(NS(id, "metric")),
    plot_ui(NS(id, "metric"))
  )
  
}

metric_server <- function(id, df, vbl, threshhold) {
  
  moduleServer(id, function(input, output, session) {
    
    text_server("metric", df, vbl, threshhold)
    plot_server("metric", df, vbl, threshhold)
    
  })
  
}