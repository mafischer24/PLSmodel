library(shiny)
library(tidyverse)

# create the user interface
ui <- fluidPage(
  "This is our PLS Shiny App"
)

# define the behavior of app
server <- function(input, output, session){

}

# build shiny app
shinyApp(ui, server)
