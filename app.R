#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(datasets)


data("mtcars")
data("USArrests")
data("uspop")


ui <- fluidPage(
  
  
  tags$style(HTML("
    body {
      background-color: white;
      color: black;
    }
    h1 {
      font-family: 'Palatino', sans-serif;
    }
    .shiny-input-container {
      color: #000000;
    }
  ")),
  
 
  titlePanel("Data Visualization App"),
  

  sidebarLayout(
    
    
    sidebarPanel(
      
     
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("mtcars", "USArrests", "uspop")),
      
      
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   min = 1,
                   value = 10),
      
      
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        selectInput("variable", "Variable to plot against mpg:",
                    choices = c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear")),
        
        
        checkboxInput("outliers", "Show outliers", TRUE)
      )
    ),
    
    
    mainPanel(
      
      
      h3(textOutput("caption", container = span)),
      
      
      verbatimTextOutput("summary"),
      
      
      tableOutput("view"),
      
      
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        plotOutput("hpPlot")
      )
    )
  )
)


server <- function(input, output) {
  

  datasetInput <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "USArrests" = USArrests,
           "uspop" = as.data.frame(uspop))
  })
  

  output$caption <- renderText({
    input$caption
  })
  

  output$summary <- renderPrint({
    summary(datasetInput())
  })
  

  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  

  formulaText <- reactive({
    paste("hp ~", input$variable)
  })
  
  output$hpPlot <- renderPlot({
    if (input$dataset == "mtcars") {
      boxplot(as.formula(formulaText()),
              data = mtcars,
              outline = input$outliers,
              col = "#75AADB", pch = 19,
              main = paste("Plot of", input$variable, "vs hp"))
    }
  })
}

shinyApp(ui = ui, server = server)


install.packages("rsconnect")
library(rsconnect)


rsconnect::setAccountInfo(name='l8ty54-hao-mou',
                          token='D5487C6CA52A2E51ACFC8270853D12E4',
                          secret='+uLzQY0EEyw4PnHqKZr9yjS45Hz7eZdNggk/ryCT')


rsconnect::deployApp(appName = "hw6_app")