page1 <- tabPanel(
    "Bibliometric data",
    fluidPage(
        theme = shinytheme("united"),
        
        
        # TITLE AND HEADER PANEL FOR PAGE ####
        titlePanel(title = "Czech R&D Evaluation 2017"),
        # Main panel name
        #headerPanel(h4("Visual guide")),
        #tags$br(),
        tags$br(),
        p(
            a("Bibliometric evaluation of Czech R&D results", href = "https://hodnoceni17.rvvi.cz/www"),
            "is a primary source of information on the results of Czech science. It is not, strictly speaking, a bibliographical database. Its basic unit of analysis is a",
            tags$b("result"),
            ",",
            "i.e. various types of outcomes of basic and applied research supported with public expenditures."
        ),
        # Header title
        
        # SIDEBAR PANEL
        
        fluidRow( 
            # Start a row ####
            
            
            column( # Left column ####
                    2,
                    # start columns 1-3
                    
                    # Left sidebar panel description
                    h4("Data controls"),
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
                    
                    wellPanel(uiOutput("data_controls")),
                    
                    
                    
                    # Input AIS/SJR score ####
                    
                    uiOutput("score"),
                    textOutput("test")
                    
            ), # end columns 1-2
            
            
            
            
            column( # Central column ####
                    8,
                    # start columns 3-10
                    
                    # Middle panel title
                    (div(h4("Visualization"), align = "center")),
                    tags$br(),
                    
                    # Output text ####
                    textOutput("out_org"),
                    textOutput("leg_val"),
                    
                    
                    # Output plotly ####
                    uiOutput("plotui")#,
                    #plotlyOutput("plot")
                    
                    
            ),
            # end columns 3-10
            
            
            column( # Right column ####
                    2,
                    
                    # Right sidedebar panel description
                    h4("Visualization controls"),
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
                    
                    materialSwitch(inputId = "flip", 
                                   label = "Flip",
                                   value = FALSE,
                                   status = "primary"),
                    
                    # Show legend ####
                    
                    materialSwitch(inputId = "legend", 
                                   label = "Legend",
                                   value = FALSE,
                                   status = "primary"),
                    
                    uiOutput("adjust_leg_X"),
                    uiOutput("adjust_leg_Y")
                    
            ) # end columns 10-12
            
        ),
        # fluid-row 1 end
        
        
        # FOOTER ####
        hr(),
        HTML('<p align = "right">Contact: hladik at flu.cas.cz</p>')#,
        # h5(textOutput("counter")) # Counter display
        
    ) # fluid-page end
    
) # tabpanel end

