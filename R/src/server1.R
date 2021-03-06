
    
    
    # data choices ####
    
    data_source <- callModule(picker, "data_source")
    
    # prepare source data to work with based on data source ####
    source_data <- reactive({national_results  %>% 
            filter(segment == data_source()) %>% 
                       # this segment could be moved to processed data rather than on the fly
            group_by(discs, org) %>% 
            mutate(total_org_disc = n()) %>% 
            group_by(org) %>% 
            mutate(total_org = n()) %>%
            group_by(discs) %>% 
            mutate(total_disc = n()) %>% 
            ungroup()
                       })
    
    # Input discipline and organization ####
    
    output$data_controls <- renderUI({
        
        tagList(
            
           
            
        # Select organization 
            
        
        
            filterSelectUI("org", label = "Select organization", sort(unique(source_data()$org))),
           
        # Flip percentages for Orgs 
           p("Percentages ", 
              bsButton("q1", 
                       label = "?", 
                       style = "primary",
                       size = "extra-small")),
            
            bsPopover(id = "q1", 
                      title = "", 
                      content = "Percentages represent a share of results in a discipline from the sum of results in an organization.",
                      placement = "right", 
                      trigger = "hover",
                      options = list(container = "body")),

                

                     materialSwitch(inputId = "flip_pct_org",
                                    label = "",
                                    value = FALSE,
                                    status = "primary"),

       
        # Select discipline 
        
            filterSelectUI("discs", label = "Select discipline", sort(unique(source_data()$discs)))
        )
    })
    
    # Input score  ####
    
    output$score <- renderUI({
        
        
        if (data_source() == "wos") {
            
            sliderTextInput("percentile", "AIS percentile", seq(0, 100, 5), selected = c(0,100), grid = TRUE, animate = FALSE)
            
        } else if (data_source() == "scopus") {
            
            sliderTextInput("percentile", "SJR percentile", seq(0, 100, 5), selected = c(0,100), grid = TRUE, animate = FALSE)
            
        }
    })
    
    # Input percentages ####
    
    output$score_percentage <- renderUI({
        
            

                materialSwitch(inputId = "flip_pct_score", 
                               label = "",
                               value = FALSE,
                               status = "primary")

            })
    
    
    # create outputs for UI 
    
    # grab organization input ####
    org <- callModule(filterSelect, "org")

    # grab discipline input ####
    discs <- callModule(filterSelect, "discs")

    # grab percentage org switch ####
    pct_score_org <- reactive({input$flip_pct_org})
    
    # grab axes switch ####
    axes_status <- reactive({input$axes})
    
    output$check1 <- reactive({
       length(org()) 
    })
    
    outputOptions(output, 'check1', suspendWhenHidden=FALSE)  
    
    output$check2 <- reactive({
        length(discs()) 
    })
    outputOptions(output, 'check2', suspendWhenHidden=FALSE)
    
    # grab percentiles ####
    
    percentile_low <- reactive({input$percentile[1]})
    percentile_high <- reactive({input$percentile[2]})
    
    # grab percentage score switch ####
    pct_score <- reactive({input$flip_pct_score})
    
    # grab threshold value ####
    threshold_val <- reactive({input$threshold})
    
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
        sliderInput("adjust_leg_val_X", "Horizontal coordinate",
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
        sliderInput("adjust_leg_val_Y", "Vertical coordinate",
                    min = -2, 
                    max = 3,
                    step = 0.1, 
                    round = TRUE,
                    ticks = FALSE,
                    value = 1)
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
                            input_pct_low = as.numeric(percentile_low()),
                            input_pct_high = as.numeric(percentile_high()),
                            flip_status = as.logical(flip_status()),
                            legend_status = as.logical(legend_status()),
                            input_leg_val_X = leg_val_X(),
                            input_leg_val_Y = leg_val_Y(),
                            input_pct_score = as.logical(pct_score()),
                            input_threshold_val = threshold_val(),
                            input_pct_score_org = pct_score_org(),
                            input_axes = axes_status()
                            )
            })
            
           
            
            plotlyOutput("myplot", height = plot_size())
                
            
        } else {
            
            output$warn <- renderText({"Please select data to display."})
            textOutput("warn")
        }
    })
    
    
    