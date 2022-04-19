library(shiny)
library(ggplot2)
library(pls)

intro_panel <- tabPanel(
  "About",
  titlePanel("Learn about our model"),
  img(src = "~/Desktop/PLSmodel/paper_files/final_paper_draft/fig1.png"),
  p("we predict BSi and TOC content from FTIRS data")
)

locations <- c("Alaska", "Greenland", "Arctic (AK+GL)")

second_panel <- tabPanel(
  "use Model",
  titlePanel("upload your data"),
  selectInput("dataset", label ="what is your location?", choices = locations ),
  fileInput("upload", "Upload a file"),
  tableOutput("files")

)

rmsep_plot <- mainPanel(
  plotOutput("plot")
)

ui <- navbarPage(
  "BSi Predictive Model",
  intro_panel,
  second_panel
)

server <- function(input, output, session) {
  output$files <- renderTable(input$upload)
}

shinyApp(ui, server)
