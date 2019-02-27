
#### Module to select years ####
sliderRangeUI <- function (id, label, min, max, value, animate) {
  ns <- NS(id)
  tagList(
    sliderInput( # Input control - slider for years
      ns("filter_range"),
      label = label,
      min = min, 
      max = max,
      value = value,
      step = NULL,
      round = TRUE,
      sep = "",
      animate = animate
    ) ) }

sliderRange <- function(input, output, session) {
  
  return(list(range_limits = reactive({input$filter_range}), 
              range_max = reactive({input$filter_range[1]}),
              range_min = reactive({input$filter_range[2]})
  ))
}
####

#### Module to select from multiple numerical values ####
sliderIntegerUI <- function (id, label, value, min, max, step) {
  ns <- NS(id)
  tagList(
    sliderInput( # Input control - slider for years
      ns("filter_integer"),
      label = label,
      min = min,
      max = max,
      value = value,
      step = step,
      round = TRUE,
      sep = "",
      animate = FALSE
    ) ) }

sliderInteger <- function(input, output, session) {
  
  return(reactive({input$filter_integer}))
  
}
####

#### Module to select a list of textual inputs to be used for filtering ####
filterSelectUI <- function(id, label, filtervar) {
  ns <- NS(id)
  tagList(
    selectInput( # Input control - disciplines selector
      ns("filter_text"),
      label,
      unique(filtervar),
      selected = "",
      multiple = T
    )) }

filterSelect <- function(input, output, session) {
  reactive({input$filter_text})
}
####

#### Module to select single option from a list of text inputs ####
singleSelectUI <- function(id, label, choices, selected) {
  ns <- NS(id)
  tagList(
    selectInput( # Input control - disciplines selector
      ns("single_option"),
      label,
      choices,
      selected,
      multiple = F
    )) }
singleSelect <- function(input, output, session) {
  reactive({input$single_option})
}
####


# vizPickerUI <- function(id, label) {
#   ns <- NS(id)
#   tagList(
#     radioButtons( # Input control - disciplines selector
#       ns("pick_viz"),
#       label,
#       choices = c("point", "line", "bar"),
#       selected = "point"
#     )) }


#### Module to select from exclusive option by buttons ####
pickerUI <- function(id, label, choices, selected) {
  ns <- NS(id)
  tagList(
    radioButtons( # Input control - disciplines selector
      ns("picker"),
      label,
      choices,
      selected
    )) }

picker <- function(input, output, session) {
  reactive({input$picker})
}
####

# Plot fun for bibliometrics ####

bibliometric_plot <- function(data, filter2, filter2use, main, secondary, themer, ...) {
    
    filter2 <- enquo(filter2)
    main <- enquo(main)
    secondary <- enquo(secondary)
    
data %>% 
    filter(!!filter2 %in% filter2use) %>% 
    count(!!main, wt_var = !!secondary) %>% 
    ggplot(aes(x = reorder(factor(!!main), n),
               y = n,
               fill = wt_var,
               text = paste(wt_var, "\n",
                            !!main, "\n",
                            n))) +
    geom_bar(stat = "identity",
             position = position_dodge2(preserve = "single")) +
    labs(x = "", y = "" ) + #, title = title2
    themer +
    theme(legend.title = element_blank()) +
    coord_flip() #+
}
####

# help popup ####
# https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R
# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title = NULL) {
    a(href = "#",
      class = "popover-link",
      `data-toggle` = "popover",
      `data-title` = title,
      `data-content` = content,
      `data-html` = "true",
      `data-trigger` = "hover",
      icon("question-circle")
    )
}
  

