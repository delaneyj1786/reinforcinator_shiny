

library(shiny)
#source("module_counter_test.R")

# counter
# ui <- fluidPage(
#     counterButton("counter1", "Counter #1")
# )


# counter
# server <- function(input, output, session) {
#     counterServer("counter1")
# }

shinyApp(ui, server)
