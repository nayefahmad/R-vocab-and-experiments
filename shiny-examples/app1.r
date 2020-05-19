
library(shiny)

# reference: https://mastering-shiny.org/basic-app.html 

# UI (front-end): -----------------------
ui <- fluidPage(  
  # ^ Layout function: fluidPage() 
  
  # theme from shinythemes package:
  theme = shinythemes::shinytheme("cosmo"),
  
  titlePanel("Demo"), 
  
  # > Input controls: -----------
  # allow user to interact with app: 
  selectInput(inputId = "dataset",  # server fn accesses this with input$dataset
              label = "Dataset", 
              choices = ls("package:datasets")), 
  
  textInput(inputId = "whodis",
            label = "Whodis? "), 
  
  selectInput("seed", 
              "Random seed" ,
              choices = c(1, 2)),
  
  
  
  # > Output controls: ---------
  # tell Shiny where to put rendered output (but doesn't say
  # *how* to make the output - that's what the server fn is for)
  
  # Outputs in the UI create placeholders that are later filled by the server
  # function.
  
  # Each output fn in the UI is coupled with a `render*( )` fn in the back end 
  verbatimTextOutput(outputId = "summary"),  # server fn accesses this with output$summary 
  tableOutput("table"), 
  textOutput("whodis"),  # for regular text. Console output/code should use verbatimTextOutput() as above 
  plotOutput("normals", width = "400px"), 
  plotOutput("plot_mtcars", width = "400px")
  
  
  # Note: Layout functions, inputs, and outputs have different uses, but they
  # are fundamentally the same under the covers: they’re all just fancy ways to
  # generate HTML, and if you call any of them outside of a Shiny app, you’ll
  # see HTML printed out at the console.
  
)



# Server (back-end):-------------
server <- function(input, output, session) {
  
  # nearly evey Shiny output will use a "render*( )" function
  
  # this is the common pattern: 
  # output$ID <- renderTYPE({
  #   # Expression that generates whatever kind of output
  #   # renderTYPE expects
  # })
  
  # reactive expressions: -----------
  # These will be used in the renderOUTPUT() functions
  # They update whenever the inputs change 
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  normal_sample <- reactive({
    set.seed(input$seed)
    rnorm(100)
  })
  
  
  
  
  # render*() functions: ------
  # Hadley: do as little computation in your render functions as possible (https://mastering-shiny.org/basic-ui.html)
  output$summary <- renderPrint({
    summary(dataset())  # note to use the result of a reactive expr., you call it like a function
  })
  
  output$table <- renderTable({
    dataset()
  })
  
  output$whodis <- renderText({
    paste0("Hello ", input$whodis)
  })
  
  output$normals <- renderPlot(hist(normal_sample()))
  
  output$plot_mtcars <- renderPlot({
    plot(mtcars$mpg)
  })
  
  # Notice that I haven’t written any code that checks for changes to
  # input$dataset and explicitly updates the two outputs. That’s because outputs
  # are reactive: they automatically recalculate when their inputs change.
  # Because both of the rendering code blocks I wrote used input$dataset,
  # whenever the value of input$dataset changes (i.e. the user changes their
  # selection in the UI), both outputs will recalculate and update in the
  # browser.
}


# Launch app: -----------
shinyApp(ui, server)