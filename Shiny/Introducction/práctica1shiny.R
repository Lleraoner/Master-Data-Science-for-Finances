#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram, user interface
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      checkboxInput(inputId = 'checkbox',
                    label = 'Ver faithful',
                    value = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = 'input.checkbox',
        plotOutput("distPlot")
        ),
      conditionalPanel(
        condition = '!input.checkbox',
        plotOutput('distPlot2')
       )
     )
   )
 )

# Define server logic required to draw a histogram, servidor, todo lo que hace las formulas matematicas detrás.
server <- function(input, output) {
  
  # CheckBox
  output$value <- renderPrint({ input$checkbox })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] #data set, es un array
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
    output$distPlot2 <- renderPlot({
      ggplot(mpg, aes(x = cyl)) + geom_bar(fill = 'gray') + geom_point(aes(y = cty, color = class, size = hwy), alpha = 0.3)
    })
  }


# Run the application 
shinyApp(ui = ui, server = server)

##hay que añadir un chect box, cuyo nombre sea el que queramos, mientras funcione.
