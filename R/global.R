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
require(shinyBS)

national_results <- feather::read_feather("../data/processed/national_results.feather")
journals <- feather::read_feather("../data/processed/journals.feather")
load("../data/scales2017/scales2017_processed.RData")

# source functions
source("helpers.R", local = T)
source("src/org_by_disc.R", local = T)
source("src/plot_freq_words.R", local = T)

# source UI
source("src/page1.R", local = T)
source("src/page2.R", local = T)







