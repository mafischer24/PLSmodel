library(shiny)
library(tidyverse)

#not sure why this inputs aren't allowed inside ui
whichdata <- c("greenland", "alaska")

# create the user interface
ui <- fluidPage(
  "This is Grace's Shiny App",
  selectInput("whichdatachoose", "Which datasets would you like to use as calibration data?",
                 whichdata, multiple = TRUE),
  fileInput("upload", "Select your FTIRS dataset", accept = ".csv"),
  tableOutput("contents")

)

# define the behavior of app
server <- function(input, output, session){
  output$contents <- renderTable({
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read_csv(file$datapath)
  })

  pls_data <- reactive({
    source('R/compiled_data_load.R')
  })
  output$summary <- renderPrint({
    summary(pls_data())
  })

}

# build shiny app
shinyApp(ui, server)
