library(shiny)
library(ggplot2)
library(pls)
library(plsr)
# need to change this load when change name of package

## Static objects
## loading our data
greenland_df <- read_ftirs("Samples/greenland_csv",
                           "csvFiles/wet-chem-data.csv",
                           format = "wide") %>%
              select(-1883)
alaska_df <- read_ftirs("Samples/alaska_csv",
                        "csvFiles/AlaskaWetChem.csv",
                        format = "wide") %>%
  # this is missing one BSi value?
  select(-1883)

combined_artic_df <- rbind(greenland_df, alaska_df)

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

# Need to attach our df
#replace this line with the functions eventually (or the attached
# dataset!)


# create the user interface
ui <- fluidPage(
  "This is our PLS Shiny App",
  selectInput("whichdatachoose",
              "Which datasets would you like to use as calibration data?",
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

shinyApp(ui, server)
