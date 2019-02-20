library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
ui <- shinyUI(
  #Se ensancha tanto como el navegador del ordenador
  fluidPage(
    useShinyjs(),
    titlePanel(h2("Mi primer dashboard")),
    sidebarLayout(
      sidebarPanel(
        selectInput('selectinputX', 
                    label = "Variable X", 
                    choices = names(mpg), 
                    multiple = FALSE),
        selectInput('selectinputY', 
                    label = "Variable Y", 
                    choices = names(mpg), 
                    multiple = FALSE),
        selectInput('selectinputfacet',
                    label = 'Facet',
                    choices = c('Año' = 'year', 'Cilindros' = 'cyl', 'Tracción' = 'drv', 'Carburante' = 'fl', 'Clase' = 'class'),
                    multiple = FALSE),
        selectInput('selectinputcolor',
                    label = 'Color',
                    choices = names(mpg),
                    multiple = FALSE),
        
        checkboxInput('cbInput', 
                      label = 'Convertir a PlotLy',
                      value = FALSE),
        checkboxGroupInput('cbGroupInput', 
                           label = NULL, 
                           c('Facet', 'Ponme color')),
        
        helpText('Se deberán seleccionar las variables para los ejes X/Y además del tipo de gráfico 
                 y la variable para usar en el facet (opcional)')
        ),
      
      mainPanel(h1("Gráfica"), ("Esta es la gráfica de MPG"),
                conditionalPanel(
                  condition = "input.cbInput",
                  plotlyOutput("graficaPlotLy")
                ),
                conditionalPanel(
                  condition = "!input.cbInput",
                  plotOutput('grafica')
                )
      )
      )
  )
  )
server <- function(input, output) {
  
  output$graficaPlotLy <- renderPlotly({
    
    facet      <- "Facet"       %in% input$cbGroupInput
    color      <- "Ponme color"     %in% input$cbGroupInput
    
    if (facet & color){
      ggplotly(ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3, aes_string(color = input$selectinputcolor)) + facet_grid(as.formula(paste(". ~", input$selectinputfacet))))
    }
    else if (facet){
      ggplotly(ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3) + facet_grid(as.formula(paste(". ~", input$selectinputfacet))))
    }
    else if (color){
      ggplotly(ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3, aes_string(color = input$selectinputcolor)))
    }
    else{
      ggplotly(ggplot(mpg, aes_string(x = input$selectinputX, y = input$selectinputY))+ geom_point(size = 3))
    }
    
  })
  
  output$grafica <- renderPlot({
    facet      <- "Facet"       %in% input$cbGroupInput
    color      <- "Ponme color"     %in% input$cbGroupInput
    
    if (facet & color){
      ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3, aes_string(color = input$selectinputcolor)) + facet_grid(as.formula(paste(". ~", input$selectinputfacet)))
    }
    else if (facet){
      ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3) + facet_grid(as.formula(paste(". ~", input$selectinputfacet)))
    }
    else if (color){
      ggplot(mpg, aes_string(x = input$selectinputX,y = input$selectinputY))+ geom_point(size = 3, aes_string(color = input$selectinputcolor))
    }
    else{
      ggplot(mpg, aes_string(x = input$selectinputX, y = input$selectinputY))+ geom_point(size = 3)
    }  })
  
  #observeEvent(input$checkboxFacet, {toggleState('selectinputfacet')})
  
}
shinyApp(ui, server)