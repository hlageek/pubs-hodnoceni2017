
    
    
    # data choices ####
    
    data_source <- callModule(picker, "data_source")
    
    # prepare source data to work with based on data source ####
    source_data <- reactive({national_results  %>% 
            filter(segment == data_source()) 
    })
    
    # Input discipline and organization ####
    
    output$data_controls <- renderUI({
        
        tagList(
            filterSelectUI("org", label = "Select organization", source_data()$org),
            
            filterSelectUI("discs", label = "Select discipline", source_data()$discs)
        )
    })
    
    # Input score  ####
    
    output$score <- renderUI({
        
        
        if (data_source() == "wos") {
            
            sliderTextInput("percentile", "AIS percentile", seq(0, 100, 5), selected = c(0,100), grid = TRUE, animate = TRUE)
            
        } else if (data_source() == "scopus") {
            
            sliderTextInput("percentile", "SJR percentile", seq(0, 100, 5), selected = c(0,100), grid = TRUE, animate = TRUE)
            
        }
    })
    
    
    # create outputs for UI 
    
    # grab organization input ####
    org <- callModule(filterSelect, "org")
    
    # grab discipline input ####
    discs <- callModule(filterSelect, "discs")
    
    # grab percentiles ####
    
    output$percentile_low <- renderText({input$percentile[1]})
    output$percentile_high <- renderText({input$percentile[2]})
    
    
    
    # render organization ####
    output$out_org <- renderText({org()})
    
    # grab plot size input ####
    plot_size <- callModule(singleSelect, "plot_size")
    
    # grab theme input ####
    theme <- callModule(singleSelect, "theme")
    
    
    # grab plot title input ####
    new_plot_title <- eventReactive(
        eventExpr = input$update_plot_title, 
        valueExpr = { input$plot_title },
        ignoreNULL = FALSE
    )
    

    # grab axis flipper ####
    flip_status <- reactive({input$flip})
    
    # grab legend switch ####
    legend_status <- reactive({input$legend})
    
    # adjust legend position ####
    
    
        
    output$adjust_leg_X <- renderUI({
        if (legend_status() == TRUE) {
        sliderInput("adjust_leg_val_X", "Legend X position",
                    min = -2, 
                    max = 3,
                    step = 0.1, 
                    round = TRUE,
                    ticks = FALSE,
                    value = 1)
            }
                         })
    
    output$adjust_leg_Y <- renderUI({
        if (legend_status() == TRUE) {
        sliderInput("adjust_leg_val_Y", "Legend Y position",
                    min = -2, 
                    max = 3,
                    step = 0.1, 
                    round = TRUE,
                    ticks = FALSE,
                    value = -0.1)
        }
                      })
    
    
    leg_val_X <- reactive({input$adjust_leg_val_X})
    leg_val_Y <- reactive({input$adjust_leg_val_Y})
    
    
    # Plot function ####
    
    
    output$plotui <- renderUI({
        
        if (isTruthy(org()) | isTruthy(discs())) {
            
            output$myplot <- renderPlotly({
                org_by_disc(source_data = source_data(),
                            input_org = org(),
                            input_discs =  discs(),
                            input_theme = theme(),
                            input_title = new_plot_title(),
                            input_pct_low = as.numeric(input$percentile[1]),
                            input_pct_high = as.numeric(input$percentile[2]),
                            flip_status = flip_status(),
                            legend_status = legend_status(),
                            input_leg_val_X = leg_val_X(),
                            input_leg_val_Y = leg_val_Y()
                            )
            })
            
            plotlyOutput("myplot", height = plot_size())
            
        } else {
            
            output$warn <- renderText({"Please select data to display."})
            textOutput("warn")
        }
    })
    
    
    