library(shiny)
library(ggplot2)
library(pls)
library(plsr)
library(dplyr)
library(readr)
# need to change this load when change name of package

# ## Static objects
# ## loading our data
# greenland_df <- read_ftirs("Samples/greenland_csv",
#   "csvFiles/wet-chem-data.csv",
#   format = "wide"
# ) %>%
#   select(-1883)
# alaska_df <- read_ftirs("Samples/alaska_csv",
#   "csvFiles/AlaskaWetChem.csv",
#   format = "wide"
# ) %>%
#   # this is missing one BSi value?
#   select(-1883)
#
# combined_artic_df <- rbind(greenland_df, alaska_df)


## Defining our different panels
## clever and keeps ui clean, but can be potentially confusing about what
## needs to be included so we should be mindful
about_panel <-
  tabPanel(
    "About",
    titlePanel("Learn about our model"),
    img(src = "fig_shiny.png"),
    p("We predict BSi and TOC content from FTIRS data")
  )

# Do we want these locations?
locations <- c("Alaska", "Greenland", "Arctic (AK+GL)")

use_mod_panel <- tabPanel(
  "Use Model",
  titlePanel("upload your data"),
  selectInput("dataset", label = "What is your location?", choices = locations),
  ## What do we want to accept? Think we want to accept
  ## pathway to directory?
  fileInput("upload", "Upload a file", accept = ".csv"),
  tableOutput("files")
)

# this plot isn't being rendered to output atm
rmsep_plot <- mainPanel(
  plotOutput("plot")
)

# create the user interface
ui <- navbarPage(
  "BSi Predictive Modeling",
 about_panel,
  use_mod_panel
)

# define the behavior of app
server <- function(input, output, session) {
  output$files <- renderTable({
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    read_csv(file$datapath)

  })

  output$summary <- renderPrint({
    summary(pls_data())
  })


}

shinyApp(ui, server)
