#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)


shinyUI(
  dashboardPage(
    dashboardHeader(title = h4('Guess The Correlation')),
    dashboardSidebar(
      sidebarMenu(
        #Cuidado con los tabname
        menuItem('Introducction', tabName = 'image', icon = icon('camera-retro')),
        menuItem('Guess the correlation', tabName = 'guess', icon = icon('chart-bar')),
        menuItem('Paint the correlation', tabName = 'paint', icon = icon('chart-bar')),
        #menuItem('Change the correlation', tabName = 'change', icon = icon('chart-bar')),
        menuItem('Guess the median', tabName = 'median', icon = icon('chart-bar'))
        #menuItem('Conclusion And References', tabName = 'referencias', icon= icon('chart-bar'))
      )
    ),
    dashboardBody(tabItems(
      
      tabItem(tabName = 'image', align = 'center',
              mainPanel(style="text-align: center;",
                        h1('Shiny Final Proyect'),
                        h4('Game Guess The correlation'),
                        h2("What is this game"),
                        p("In probability theory, the birthday problem or birthday paradox concerns the probability that,"),
                        p("in a set of n randomly chosen people, some pair of them will have the same birthday.By the pigeonhole principle,"),
                        p("the probability reaches 100% when the number of people reaches 366 (since there are only 365 possible birthdays."),
                        p("However, 99.9% probability is reached with just 70 people, and 50% probability with 23 people."),
                        p("These conclusions are based on the assumption that each day of the year is equally probable for a birthday."),
                        p("The history of the problem is obscure. W. W. Rouse Ball indicated that it was first discussed by Harold Davenport.")
              )
      ),
      
      tabItem(tabName = 'guess',
              h2('Correlation Chart'),
              plotOutput('modelo'),
              uiOutput('guesscorrelation')),
      
      tabItem(tabName = 'paint',
              h2('Paint the correlation Chart'),
              plotOutput('modelopintado', click = 'paintclick'),
              uiOutput('paintmodel')),
      # el de change no se lo que hay que ahcer solo se que hay un boton que te he puesto en el server
      tabItem(tabName = 'change',
              h2('Change Chart'),
              box(plotOutput('modelocambiado')),
              box(
                h2('Game change the Correlation'),
                br(),
                h3("Selecciona la correlacion correcta")
              ),
              uiOutput('cambiomodelo')),
      
      tabItem(tabName = 'median',
              h2('Guess the median'),
              plotOutput('graficomediana'),
              uiOutput('compruebamediana'))
      
      
      
    )
    
    
    ))
)

