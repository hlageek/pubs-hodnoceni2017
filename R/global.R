# load packages
require(shiny)
require(shinythemes)
require(dplyr)
require(ggplot2)
require(tidytext)
require(stringr)
require(markdown)
require(shinycssloaders)
require(plotly)
require(shinyWidgets)

national_results <- data.table::fread("../data/processed/national_results.tsv")
journals <- data.table::fread("../data/processed/journals.tsv")
load("../data/scales2017/scales2017_processed.RData")

source("helpers.R", local = T)
source("src/org_by_disc.R", local = T)
source("src/page1.R", local = T)







