library(shiny)
library(ggplot2)
library(pls)
library(dplyr)
library(readr)
library(shinyFiles)

library(plsr)
# need to change this load when change name of package

## Static objects
## loading our data

# greenland_df <- read_ftirs(here::here("Samples/greenland_csv"),
#   here::here("csvFiles/wet-chem-data.csv")
# )
# alaska_df <- read_ftirs(here::here("Samples/alaska_csv"),
#   here::here("csvFiles/AlaskaWetChem.csv")
# )  # this is missing one BSi value?
#
# combined_arctic_df_wide <- rbind(greenland, alaska) %>%
#  pivot_ftirs_wider()
#
# combined_mod <-  plsr(bsi~., ncomp = 10, data = combined_arctic_df_wide, validation = "CV", segments = 10)
# summary(combined_mod)
#
# #predicting combined with combined
# predict(combined_arctic_df_wide%>%select(-1))

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
  ##fileInput("upload", "Upload a file", accept = ".csv"),
  shinyDirButton('directory_select', 'Select a directory', title='Select a directory'),
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
  # output$files <- renderTable({
  #   file <- input$upload
  #   ext <- tools::file_ext(file$datapath)
  #   req(file)
  #   validate(need(ext == "csv", "Please upload a csv file"))
  #   read_csv(file$datapath)
  # })

  volumes <-  c(home = "~")

      #getVolumes()
  shinyDirChoose(input, 'directory_select', roots = volumes,
               filetypes = c("csv"),
               session=session)
  dirname <- reactive({parseDirPath(volumes, input$directory_select)})

#  df <- reactive({read_ftirs(dirname())})

  output$files <- renderTable({
   # as.character(dirname())

    #as.data.frame(read_csv(paste(dirname(), "/FISK-10.0.csv", sep = "")))
    x <- read_ftirs(dirname())
    head(x)


  }
  )
  # output$dirname <- renderText({
  #    parseDirPath(c(home = "~"), dirname())
  #  })
  observe({print(dirname())})
  #observe({print(df)})



}

shinyApp(ui, server)
