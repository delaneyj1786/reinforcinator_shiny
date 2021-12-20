#https://mastering-shiny.org/scaling-modules.html#module-ui
#
# ui <- fluidPage(
#   selectInput("var", "Variable", names(mtcars)),
#   numericInput("bins", "bins", 10, min = 1),
#   plotOutput("hist")
# )
# server <- function(input, output, session) {
#   data <- reactive(mtcars[[input$var]])
#   output$hist <- renderPlot({
#     hist(data(), breaks = input$bins, main = input$var)
#   }, res = 96)
# }

require(shiny)

histogramUI <- function(id){
  taglist(
    selectInput(NS(id, "var"), "Variable", choices = names(mtcars)),
    numericInput(NS(id,"bins"), "bins", value = 10, min = 1),
    plotOutput(NS(id,"hist"))
  )
}

histogramServer <- function(id){
  moduleServer(id, function(input,output,session){
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = inpu$var)
    }, res = 96)
  })
}

histogramApp <- function(){
  ui <- fluidPage(
    histogramUI("hist1")
  )

server<- function(input, output, session){
  histogramServer("hist1")
}

shinyApp(ui,server)
}
