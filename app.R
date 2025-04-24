library(here)
library(shiny)

ui <- source(here("src","ui.R"))$value
server = source(here("src", "server.R"))$value

shinyApp(ui = ui, server = server)