library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Analiza napak glede na moc mnozice S"),
  
    tabPanel("Napake aproksimacije ploščine 1. metode",
             sidebarPanel(
               
               selectInput("vrednost", label = "Izberite moč množice S:",
                           choices=(sort(unique(tabela2$Delez_izbranih_tock))))),
             
             mainPanel(plotOutput("rezultati"))),
    
    
    uiOutput("izborTabPanel")))
  
  
  
