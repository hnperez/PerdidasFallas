library(shiny)
library(dplyr)
library(shiny)
library(fitdistrplus)
library(lubridate)
library(ggplot2)
library(lmomco)
library(extremeStat)

# distribuciones estadisticas continuas posibles para impacto

distribd<-c("nbinom", "pois", "hyper")

distribc<-c("wak", "kap", "wei", "gev", "pe3", "gno", "nor", "gpa", "glo", 
            "ray", "rice", "lap", "revgum", "gum", "gam", "exp", "ln3") 

# distribuciones estadisticas discretas posibles para frecuencia

# Define UI for application that fit an statistic distribution for a datasets
ui <- fluidPage(fileInput("upload", "busque archivo de depositos", accept = ".txt", multiple = TRUE),
                dateRangeInput("rangofecha", "Ingrese rango de fechas para ajustar distribucion", start = "2020-01-01",
                               end   = "2021-12-31"), 
                selectInput("discretas", "Seleccione distribucion para frecuencia de eventos", distribd, selected="nbinom"),
                selectInput("continuas", "Seleccione distribucion para impacto de eventos", distribc, selected="wak"),
                numericInput("confianza", "Seleccione nivel de confianza para calcular VaR", value = 99, min = 95, max = 100, step=0.5),
                numericInput("nsimul", "Seleccione numero de simulaciones", value = 1000, min = 1000, max = 20000, step=1000),
                verbatimTextOutput("cargaarchivos"),
                plotOutput("graficofreq"),
                plotOutput("graficoVaR"),
                verbatimTextOutput("perdidas"))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##leo datos de freuencia de fallas
  datafreq<- reactive({
    read.csv(input$upload[[1, 'datapath']], header = TRUE, sep="\t", dec = ",")
  })
  ##leo datos de severidad de fallas
  datasev<- reactive({
    read.csv(input$upload[[2, 'datapath']], header = TRUE, sep="\t", dec = ",")
  })
  ##filtro datos de frecuencia de fallas por rango de fecha de usuario
  xfreq <- reactive({datafreq() %>%  filter(ymd(Fecha)> as.POSIXct(input$rangofecha[1]) & 
                                              ymd(Fecha)< as.POSIXct(input$rangofecha[2]))})
  
  ##filtro datos de severidad de fallas por rango de fecha de usuario
  
  xsev <- reactive({datasev() %>%  filter(ymd(Fecha)> as.POSIXct(input$rangofecha[1]) & 
                                              ymd(Fecha)< as.POSIXct(input$rangofecha[2]))})
  
  ##Se ajustan respectivas funciones de probabilidad Poisson y Binomial negativa a las series de FRECUENCIA fraudes (columna 2) y fallas (columna 3)
  ffraudenbinom=reactive(fitdist(as.vector(xfreq()[,2]), "nbinom"))
  ffraudepois=reactive(fitdist(as.vector(xfreq()[,2]), "pois"))
  ffallasnbinom=reactive(fitdist(as.vector(xfreq()[,3]), "nbinom"))
  ffallaspois=reactive(fitdist(as.vector(xfreq()[,3]), "pois"))
  
  
  ##Se ajustan distribuciones continuas de probailidad a las series de SEVERIDAD de fraudes (columna 2) y fallas (columna 3)
  ajusteimpactfraude <- reactive(distLfit(as.vector(xsev()[,2]))) ##ajusteimpact es un vector de distribuciones ajustadas a los impactos de fraudes
  ajusteimpactfallas <- reactive(distLfit(as.vector(xsev()[,3]))) ##ajusteimpact es un vector de distribuciones ajustadas a los impactos de fallas
  
  ##Abro una ventana 2 x 2 para graficar la frecuencia y posibles distribuciones de fraudes y fallas
  
  output$graficofreq <- renderPlot({
    par(mfrow = c(2, 2))
    plot.legend <- c("Binomial Negativa", "Poisson")
    denscomp(list(ffallasnbinom(),  ffallaspois()), legendtext = plot.legend)
    qqcomp(list(ffallasnbinom(),  ffallaspois()), legendtext = plot.legend) 
    denscomp(list(ffraudenbinom(),  ffraudepois()), legendtext = plot.legend)
    qqcomp(list(ffraudenbinom(),  ffraudepois()), legendtext = plot.legend) 
    
  })
  
  ###Aqui es donde tengo el error. Necesito crear un Data frame reactivo que en base a la seleccion del usuario
  ### mediante input$discretas me genere en la primera que denominaremos "fraudes" y en la segunda columna "fallas"
  ###valores aleatorios de las respectivas distribuciones Poisson o Binomial negatva, dependiendo delo seleccinado por el usuario ###
  
    Fr <- reactive(data.frame(fraudes= (if(input$discretas =="pois") { rpois(1000,ffraudepois()[["estimate"]][["lambda"]]) }
                              else { rnbinom(1000,mu=ffraudenbinom()[["estimate"]][["mu"]],size=ffraudenbinom()[["estimate"]][["size"]]) }),
                         fallas=   ( if(input$discretas =="pois") { rpois(1000,ffallaspois()[["estimate"]][["lambda"]]) }
                                  else {rnbinom(1000,mu=ffallasnbinom()[["estimate"]][["mu"]],size=ffallasnbinom()[["estimate"]][["size"]]) } )))
  
 # P<-reactive({
  #  P<- data.frame(fraudes=0,fallas=0)
   # for (i in 1:input$nsimul) {
    #  P$fraudes[i]= sum(rlmomco(Fr$fraudes()[i],ajusteimpactfraude()[["parameter"]][[input$continuas]]))
     # P$fallas[i]= sum(rlmomco(Fr$fallas()[i],ajusteimpactfallas()[["parameter"]][[input$continuas]]))
    #}
    #return(P)
  #})
   
  #V<-reactive(perdida=rowSums(P()))
  
    
 
  
  #output$graficoVaR <- renderPlot({
   # hist(as.numeric(V()),breaks=100, col = "blue")  })
    
 
 output$perdidas <- renderTable(Fr)

}
# Run the application 
shinyApp(ui = ui, server = server)