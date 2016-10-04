# ui.R

library(shiny)
portarias <- readRDS("portarias_MAPA_2016-09-08.RDS")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  HTML('<div align = "center">'),
  # Application title
  titlePanel("Portarias do Ministério da Agricultura, Pecuária e Abastecimento"),
  HTML('</div>'),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "data", label = "Portarias publicadas a partir da data:",
                value = Sys.Date() - 3, format = "dd/mm/yyyy"),
      br(),
      br(),
      textInput(inputId = "termo",label = "Digite aqui os termos da busca",
                value = ""),
      br(),
      actionButton(inputId = "buscar", label = "Buscar")),
    
    # Show a plot of the generated distribution
    mainPanel(
      selectInput(inputId = "numero_portaria",
                         label = "Escolha o número da portaria que deseja ver:",
                         choices = "", selected = ""),
      br(), 
      verbatimTextOutput("resultado")
    )
  )
))
