library(shiny)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("BMI Calculator"),
    sidebarPanel(
      sliderInput("ht","Enter your height in inches", min =50, max = 80 ,value = 56),
      numericInput("wt","Enter your weight in Kilograms", min = 40,max = 100, value = 50,step =1)
    ),
    mainPanel(
      h1("Results"),
      h3("Your height is"),
      verbatimTextOutput("oht"),
      h3("Your weight is"),
      verbatimTextOutput("owt"),
      h3("Your BMI is"),
      verbatimTextOutput("bmi")
    )
  )
)

BMI <- function(height,weight){
  return(weight/(0.0254*height)^2)
}

server <- function(input, output, session) {
  output$oht <- renderPrint({input$ht})
  output$owt <- renderPrint({input$wt})
  output$bmi <- renderPrint({BMI(input$ht, input$wt)})
}

shinyApp(ui, server)