library(shiny)
library(dplyr)
library(ggplot2)


shinyServer(function(input, output) {
  
  
  output$rezultati<- renderPlot({
    
    tabela2 <- tabela2 %>% filter(Delez_izbranih_tock == input$vrednost)
    
    print(ggplot(data=tabela2, aes(x = Stevilo_vseh_tock_v_množici_S, y = Uspesnost_izracunane_ploscine_1._metode), group=1) + geom_line(color="#CC0033") +
            ylab("Uspešnost 1. metode pri izracunu ploscine") + xlab("Stevilo vseh tock v mnozici S"))
  })
  

})
  