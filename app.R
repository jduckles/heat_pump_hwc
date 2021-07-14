library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("HWC Savings Caluclator"),
  tabsetPanel(
    hwcCalculatorUI("calc") 
  )
)

server <- function(input, output, session) {
  hwcCalculator("calc") 
}

shinyApp(ui, server)