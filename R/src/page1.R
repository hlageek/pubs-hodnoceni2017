page1 <- tabPanel(
    "Evalvis 17+",
    fluidPage(
        # https://douglas-watson.github.io/post/2018-02_shiny_google_analytics/
        tags$head(includeHTML(("google-analytics.html"))),
        
        theme = shinytheme("united"),
        
        
        # TITLE AND HEADER PANEL FOR PAGE ####
        titlePanel(title = "Evalvis 17+"),
        # Main panel name
        #headerPanel(h4("Visual guide")),
        #tags$br(),
        tags$br(),
        
        p(
            
            "Charting application for the data from the ", a("bibliometric evaluation of the Czech R&D results", href = "https://hodnoceni17.rvvi.cz/www/biblio-vo"), " of research organisations in 2017."
            
        ),
        # Header title
        
        # SIDEBAR PANEL
        
        fluidRow( 
            # Start a row ####
            
            
            column( # Left column ####
                    2,
                    # start columns 1-3
                    
                    # Left sidebar panel description
                    h4(""),
                    #h4("Data controls"),
                    tags$br(),
                    ("Filter displayed data"),
                    tags$br(),
                    
                    # Changing the slider color
                    # Source https://stackoverflow.com/questions/36906265/how-to-color-sliderbar-sliderinput
                    
                    
                    # data source ####
                    
                    wellPanel(pickerUI("data_source", "Data source", c("Web of Science" = "wos", "Scopus" = "scopus"), "wos")),
                    
                    
                    
                    # Input organization  ####
                    # called by Shiny module via helpers 
                    # Last argument draws a set of possible values from the data file
                    
                    wellPanel(
                              
                   
                    
                    uiOutput("data_controls"),
                    
                    conditionalPanel(condition = "output.check1 > 0 && output.check2 > 0",
                                p("Flip axes",
                                  materialSwitch(inputId = "axes",
                                               label = "",
                                               value = FALSE,
                                               status = "primary")))
                    
                     ),

                    
                    
                    
                    wellPanel(
                    # Input AIS/SJR score ####
                    
                    uiOutput("score"),
                    
                    # Flip percentages for AIS/SJR ####
                    p("Percentages ", 
                    bsButton("q2", 
                             label = "?", 
                             style = "primary",
                             size = "extra-small")),
                    
                    bsPopover(id = "q2", 
                              title = "", 
                              content = "Percentages represent a share of results in the selected percentile range from the sum of all results in each discipline and organization.",
                              placement = "right", 
                              trigger = "hover",
                              options = list(container = "body")),

                    uiOutput("score_percentage")

                    
                    
                    
                    ),

                    
                    # Set threshold for displayed data ####
                    
                    p("Set minimum count",
                    bsButton("q3", 
                             label = "?", 
                             style = "primary",
                             size = "extra-small")),
            
            bsPopover(id = "q3", 
                      title = "", 
                      content = "Minimum number of results an organization should have in the (selected) discipline and the (selected) percentile range in order to be included in the analysis. Note that this filter takes effect prior to percentage calculations and may, therefore, exclude organizations with low number of results despite their relevance in selected disciplines.",
                      placement = "right", 
                      trigger = "hover",
                      options = list(container = "body")),
            
                    numericInput(inputId = "threshold",
                                 label = "",
                                 value = 0,
                                 min = 0,
                                 step = 1)
                    
                    
                    
            ), # end columns 1-2
            
            
            
            
            column( # Central column ####
                    8,
                    # start columns 3-10
                    
                    # Middle panel title
                    (div(h4("Chart"), align = "center")),
                    tags$br(),
   
                    
                    # Output text ####
                    #textOutput("out_org"),
                    #textOutput("leg_val"),
                    
                    
                    # Output plotly ####
                    uiOutput("plotui") %>% withSpinner()#,
                    #plotlyOutput("plot")
                    
                    
            ),
            # end columns 3-10
            
            
            column( # Right column ####
                    2,
                    
                    # Right sidedebar panel description
                    #h4("Visualization controls"),
                    h4(""),
                    tags$br(),
                    ("Adjust visualizations"),
                    tags$br(),
                    
                    wellPanel(
                        # Input plot title ####
                        textInput(inputId = "plot_title", 
                                  label = "Plot title", 
                                  placeholder = "Custom title"),
                        actionButton(inputId = "update_plot_title", 
                                     label =  "Update plot title")),
                    
                    # Show legend ####
                    wellPanel(
                    p(strong("Legend")),
                    materialSwitch(inputId = "legend", 
                                   label = "",
                                   value = FALSE,
                                   status = "primary"),
                    
                    uiOutput("adjust_leg_X"),
                    uiOutput("adjust_leg_Y")
                    ),
                    # Input ggplot2 theme  ####
                    # via Shiny module in helpers
                    # Arguments take choices and default value
                    wellPanel(singleSelectUI("theme", 
                                             label = "Plot theme", 
                                             c("Stats", "Grid", "Void", "Classic"), 
                                             "Stats")),
                    
                    
                    # Input plot size ####
                    wellPanel(singleSelectUI("plot_size", "Resize plot", c(as.character(seq(200, 1200, 100)), "auto"), "auto")),
                    
                    
                    
                    # Flip axes ####
                    
                    p("Flip  perspective"),
                    materialSwitch(inputId = "flip", 
                                   label = "",
                                   value = FALSE,
                                   status = "primary")
                    
            ) # end columns 10-12
            
        ),
        # fluid-row 1 end
        

        # FOOTER ####
        #hr(),
        #HTML('<div style="background-color:#e95420;">'),
        includeMarkdown("www/footer.Rmd")#,
        #HTML('</div>')
        #HTML('<p align = "right">Contact: hladik at flu.cas.cz</p>')#,

        # h5(textOutput("counter")) # Counter display
        
    ) # fluid-page end
    
) # tabpanel end

