library(plotly)
library(shiny)

# setwd("/opt/shiny-server/samples/sample-apps/rivviz") 
options(shiny.sanitize.errors = T)

server2 <- shinyServer(function(input, output, session) {

source("src/server1.R", local = T)
source("src/server2.R", local = T)

} # end of shiny server function definition

) # end of shiny server function
