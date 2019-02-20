library(shiny)

shinyUI(
  fluidPage(
    titlePanel(h2("Mi primer dashboard")),
    sidebarLayout(
      sidebarPanel(
        numericInput("tamanio", label = "Tamaño de la muestra", value = 500, min = 100, max = 30000, step = 100),
        numericInput('correlacion', label = 'Correlacion', value = 0, min = -1, max = 1, step = 0.1),
        actionButton("generar", label = "Generar", icon('500px')),
        
        numericInput('orden', label = 'Orden del polinomio', value = 1, min = 0),
        actionButton('modelo', label = 'Añadir modelo', icon('500px'))
      ),
      
      mainPanel(
        plotOutput('grafica')
      )
    )
  )
)