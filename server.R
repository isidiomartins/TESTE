# server.R

portarias <- readRDS("portarias_MAPA_2016-09-08.RDS")

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  observeEvent(
    input$data,
    updateSelectInput(session = session, inputId = "numero_portaria",
                      label = "Adicione um termo de busca aqui",
                      choices = portarias$número[portarias$data > input$data])
  )
  
  observeEvent(
    input$termo,
    updateSelectInput(session = session, 
                      inputId = "numero_portaria",
                      label = "Escolha o número da portaria que deseja ver:",
                      choices = portarias$número[grep(pattern = tolower(input$termo),
                                                      x = tolower(portarias$conteúdo))])
  )
  
  output$resultado <- renderPrint({
        cat(portarias$conteúdo[portarias$número == input$numero_portaria])
        
      })
  
})
