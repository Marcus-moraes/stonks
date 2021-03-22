library(shiny)
library(rvest)
library(XML)
library(tidyverse)
library(googlesheets4)
source("yahoo_support.R")




ui <- fluidPage(
  titlePanel("FORECAST DEMO"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crp",label = "Criptomoeda desejada" ,choices = crypto_fun()),
      # selectInput("cur", label = "Moeda para compara??o (ex: REAL)", choices = currency_fun()),
      sliderInput("tempo", label = "Horizonte de pesquisa (dias)", min = 0, max = 365, value = 50),
      submitButton("submit")
    ),
    mainPanel("teste",
              plotOutput("grf")
              
    )
  )
  
  
)





server <- function(input, output) {
  
  
  
  datainput <- reactive({
    getSymbols(input$crp, src = "yahoo", auto.assign = F)
  })
  
  output$grf <- renderPlot({
    dale <- paste("last",input$tempo,"days",sep= " ")
    
    chartSeries(datainput(), subset = dale, name = paste(input$crp,"em",input$tempo,"dias",sep=" "), theme = chartTheme("white"))
  })
  
  
  
  
  
  
  
  
  
}




shinyApp(ui = ui, server = server)