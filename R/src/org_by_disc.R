


org_by_disc <- function(source_data, 
                        input_org, 
                        input_discs, 
                        input_theme, 
                        input_title, 
                        input_pct_low, 
                        input_pct_high,
                        flip_status,
                        legend_status,
                        input_leg_val_X,
                        input_leg_val_Y) {
    
req(source_data)
    # Theme selector ####
    theme_select <- if (input_theme == "Void") theme_void() + theme(panel.grid.major = element_blank(), 
                                                                    panel.grid.minor = element_blank()) else 
                    if (input_theme == "Stats")  theme_classic() else 
                    if (input_theme == "Grid")  theme_bw() else
                    if (input_theme == "Classic") theme_gray()
    
    # Percentile filter ####
    
    
    plot_data <- reactive({ source_data %>%
        group_by(discs) %>% 
       { if (!is.na(.$ais[1])) { # filter AIS for WOS data
            {
                if (input_pct_high < 100 |
                    input_pct_low > 0)
                    filter(., between(
                        ais,
                        quantile(ais, input_pct_low / 100),
                        quantile(ais, input_pct_high / 100)
                    ))
                else
                    .
            } %>% 
               ungroup()
       } else if (!is.na(.$sjr[1])) { # filter SJR for SCOPUS data
           {
               if (input_pct_high < 100 |
                   input_pct_low > 0)
                   filter(., between(
                       sjr,
                       quantile(sjr, input_pct_low / 100),
                       quantile(sjr, input_pct_high / 100)
                   ))
               else
                   . }
       }    
       } %>% 
        ungroup()
    })
    # Plot for discipline only ####
    
    if (isTruthy(input_discs) & !isTruthy(input_org)) {
        
        myplot <- plot_data() %>% 
            filter(discs %in% input_discs) %>% 
            group_by(org) %>% 
            count(discs) %>% 
            ggplot(aes(x = reorder(factor(org), n), 
                       y = n, 
                       fill = discs,
                       text = paste(org, "\n",
                                    discs, "\n",
                                    n))) +
            scale_fill_brewer(type = "qual", palette = "Set1", direction = 1) +
            geom_bar(stat = "identity", 
                     position = position_dodge(preserve = "single")) +
            labs(x = "", y = "", title = input_title) +
            theme_select 
        
        # legend on/off
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # flip on/off
        if (flip_status == TRUE)  {
            myplot <- myplot +
                theme(axis.text.x = element_text(angle = 75, 
                                                 vjust = 1, 
                                                 hjust=1))
        } else {
            myplot <- myplot +
                coord_flip()
        } 
        
    } 
    
    # Plot for organization only ####  
    if (!isTruthy(input_discs) & isTruthy(input_org)) {
        
        
        myplot <- plot_data() %>% 
            filter(org %in% input_org) %>% 
            group_by(org) %>% 
            count(discs) %>% 
            ggplot(aes(x = reorder(factor(discs), n), 
                       y = n, 
                       fill = org,
                       text = paste(org, "\n",
                                    discs, "\n",
                                    n))) +
            scale_fill_brewer(type = "qual", palette = "Set1", direction = 1) +
            geom_bar(stat = "identity", 
                     position = position_dodge(preserve = "single")) +
            labs(x = "", y = "", title = input_title) +
            theme_select 
        
        # legend on/off
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # flip on/off
        if (flip_status == TRUE)  {
            myplot <- myplot +
                theme(axis.text.x = element_text(angle = 75, 
                                                 vjust = 1, 
                                                 hjust=1))
        } else {
            myplot <- myplot +
                coord_flip()
        } 
        
        
        
    }
    # Plot for both discipline and organization ####    
    
    if (isTruthy(input_discs) & isTruthy(input_org)) {
        
        
        myplot <- plot_data() %>% 
            filter(org %in% input_org) %>%
            filter(discs %in% input_discs) %>%
            group_by(org) %>% 
            count(discs) %>% 
            ggplot(aes(x = reorder(factor(discs), n), 
                       y = n, 
                       fill = org,
                       text = paste(org, "\n",
                                    discs, "\n",
                                    n))) +
            scale_fill_brewer(type = "qual", palette = "Set1", direction = 1) +
            # geom_bar(stat = "identity", 
            #          position = position_dodge(preserve = "single")) +
            geom_bar(stat = "identity",
                     position = position_stack()) +
            labs(x = "", y = "", title = input_title) +
            theme_select
        
        # legend on/off
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # flip on/off
        if (flip_status == TRUE)  {
            myplot <- myplot +
                    theme(axis.text.x = element_text(angle = 75, 
                                                     vjust = 1, 
                                                     hjust=1))
                } else {
            myplot <- myplot +
                    coord_flip()
                } 
             

        
        
    }
    
    # conversion to Plotly ####
    
    if (exists("myplot")) {
        
        
        if (legend_status == TRUE) {
        ggplotly(myplot, tooltip = "text") %>% 
            layout(legend = list(orientation = "v",
                             xanchor = "right",
                             yanchor = "top",
                             x = input_leg_val_X,
                             y = input_leg_val_Y
                             ))
                
        } else {
            
            ggplotly(myplot, tooltip = "text")
            
        }
    } # end plotly code
    
} # end of function definition