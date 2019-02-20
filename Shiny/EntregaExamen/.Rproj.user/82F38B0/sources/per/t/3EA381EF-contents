library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Examen de Luis Llera García"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId ='color',
                  label = 'Color',
                  choices = c('grey' = 'grey', 'red' = 'red', 'blue' = 'blue'),
                  selected = 'grey'),
      sliderInput("elementos",
                  "Numero de elementos",
                  min = 1,
                  max = 50,
                  value = 20,
                  step = 1),
      actionButton('buton', 'Nuevo dataset')
    ),
    
    mainPanel(
      plotOutput("grafico_barras"),
      plotOutput("muestra_aleatoria")
    )
  )
))  
