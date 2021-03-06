
    
    
  
    
    # Input discipline and organization ####
    
    output$data_controls2 <- renderUI({
        
        tagList(
            filterSelectUI("org2", label = "Select organization", source_data2()$org),
            
            filterSelectUI("discs2", label = "Select discipline", source_data2()$discs)
        )
    })
    
    # Input score  ####
    
    output$score2 <- renderUI({
        
        
        if (data_source2() == "wos") {
            
            sliderTextInput("percentile2", "AIS percentile", rev(seq(0, 100, 5)), selected = c(100,0), grid = TRUE)
            
        } else if (data_source2() == "scopus") {
            
            sliderTextInput("percentile2", "SJR percentile", rev(seq(0, 100, 5)), selected = c(100,0), grid = TRUE)
            
        }
    })
    
    
    # create outputs for UI 
    
    # grab organization input ####
    org2 <- callModule(filterSelect, "org2")
    
    # grab discipline input ####
    discs2 <- callModule(filterSelect, "discs2")
    
    # grab percentiles ####
    
    output$percentile_high2 <- renderText({input$percentile2[1]})
    output$percentile_low2 <- renderText({input$percentile2[2]})
    
# duplicated start ####    
    

    
    # data choices ####
    
    data_source22 <- callModule(picker, "data_source22")
    
    # prepare source data to work with based on data source ####
    # WoS or Scopus
    source_data22 <- reactive({national_results  %>% 
            select(segment, org, discs, ais, sjr, title) %>% 
            filter(segment == data_source22()) 
    })
    
    # Input discipline and organization ####
    
    output$data_controls22 <- renderUI({
        
        tagList(
            filterSelectUI("org22", label = "Select organization", source_data22()$org),
            
            filterSelectUI("discs22", label = "Select discipline", source_data22()$discs)
        )
    })
    
    # Input score  ####
    
    output$score22 <- renderUI({
        
        
        if (data_source22() == "wos") {
            
            sliderTextInput("percentile22", "AIS percentile", rev(seq(0, 100, 5)), selected = c(100,0), grid = TRUE)
            
        } else if (data_source2() == "scopus") {
            
            sliderTextInput("percentile22", "SJR percentile", rev(seq(0, 100, 5)), selected = c(100,0), grid = TRUE)
            
        }
    })
    
    # grab organization input ####
    org22 <- callModule(filterSelect, "org22")
    
    # grab discipline input ####
    discs22 <- callModule(filterSelect, "discs22")
    
    # grab percentiles ####
    
    output$percentile_high22 <- renderText({input$percentile22[1]})
    output$percentile_low22 <- renderText({input$percentile22[2]}) 
    

# duplicated end ####    
    
    
    
    # render organization ####
    output$out_org2 <- renderText({org2()})
    
    # grab plot size input ####
    plot_size2 <- callModule(singleSelect, "plot_size2")
    
    # grab theme input ####
    theme2 <- callModule(singleSelect, "theme2")
    
    
    # grab plot title input ####
    new_plot_title2 <- eventReactive(
        eventExpr = input$update_plot_title2, 
        valueExpr = { input$plot_title2 },
        ignoreNULL = FALSE
    )
    
    # data choices ####
    
    data_source2 <- callModule(picker, "data_source2")
    
    # prepare source data to work with based on data source ####
    # WoS or Scopus
    source_data2 <- reactive({national_results  %>% 
            select(segment, org, discs, ais, sjr, title) %>% 
            filter(segment == data_source2()) 
    })
    
    # Plot function ####
    
    
    output$plotui2 <- renderUI({
        
        if (isTruthy(org2()) | isTruthy(discs2())) {
            
            output$myplot2 <- renderPlotly({
                plot_freq_words(source_data = source_data2(),
                            input_org = org2(),
                            input_discs =  discs2(),
                            input_theme = theme2(),
                            input_title = new_plot_title2(),
                            as.numeric(input$percentile2[2]),
                            as.numeric(input$percentile2[1]))
            })
            
            plotlyOutput("myplot2", height = plot_size2())
            
        } else {
            
            output$warn2 <- renderText({"Please select data to display."})
            textOutput("warn2")
        }
    })
    
    
    
    