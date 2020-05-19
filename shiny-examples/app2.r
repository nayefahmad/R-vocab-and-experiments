
library(shiny)


ui <- fluidPage(
  fluidRow(
    column(3, 
           numericInput("lambda", "Poisson rate", value = 3), 
           numericInput("n", "Sample size", value = 100), 
           actionButton("sim", "simulate!")
    ), 
    
    column(9, 
           plotOutput("p1"))
  )
)


server <- function(input, output){
  x1 <- eventReactive(input$sim, {
    rpois(input$n, input$lambda)
  })
  
  output$p1 <- renderPlot({
    hist(x1(), xlim = c(0, 50))
    
  })
}


shinyApp(ui, server)