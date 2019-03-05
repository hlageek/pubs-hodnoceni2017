



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
                        input_leg_val_Y,
                        input_pct_score,
                        input_threshold_val,
                        input_pct_score_org,
                        input_axes) {

req(source_data) # check if source is selected

# Theme selector ####
    theme_select <- if (input_theme == "Void") theme_void() + theme(panel.grid.major = element_blank(), 
                                                                    panel.grid.minor = element_blank()) else 
                    if (input_theme == "Stats")  theme_classic() else 
                    if (input_theme == "Grid")  theme_bw() else
                    if (input_theme == "Classic") theme_gray()
    
# Percentile filter ####
    
    # First we obtain disciplinary quantiles for all journals
    # in wos....
    quantiles_wos <- reactive({ journals %>% 
        filter(segment == "wos") %>% 
        mutate(discs = trimws(str_replace_all(discs, "[\\d\\.\\,]", ""))) %>%
        group_by(discs) %>% 
        mutate(quantiles = 
                   list(quantile(ais, c(input_pct_low/100,
                                        input_pct_high/100)))) %>%
        mutate(minq = quantiles[[1]][[1]],
               maxq = quantiles[[1]][[2]]) %>% 
        ungroup() %>% 
        select(discs, minq, maxq) %>% 
        dplyr::distinct(discs, .keep_all = TRUE) })
    # in scopus...
    quantiles_scopus <- reactive({ journals %>% 
            filter(segment == "scopus") %>% 
            mutate(discs = trimws(str_replace_all(discs, "[\\d\\.\\,]", ""))) %>%
            group_by(discs) %>% 
            mutate(quantiles = 
                       list(quantile(sjr, c(input_pct_low/100, 
                                            input_pct_high/100)))) %>%
            mutate(minq = quantiles[[1]][[1]],
                   maxq = quantiles[[1]][[2]]) %>% 
            ungroup() %>% 
            select(discs, minq, maxq) %>% 
            dplyr::distinct(discs, .keep_all = TRUE) })
    
    plot_data <- reactive({ 
       if (!is.na(source_data$ais[1])) { # filter AIS for WOS data
            
                if (input_pct_high < 100 |
                    input_pct_low > 0) {
                    
                    source_data %>%
                    left_join(quantiles_wos(), by = "discs") %>% 
                    group_by(discs) %>% 
                    filter(ais > minq & ais <= maxq)  %>% 
                    ungroup()
      
                } else {
                    source_data  }
            } 
       else if (!is.na(source_data$sjr[1])) { # filter SJR for SCOPUS data
           
               if (input_pct_high < 100 |
                   input_pct_low > 0) {
                 
                   source_data %>%
                   left_join(quantiles_scopus(), by = "discs") %>% 
                   group_by(discs) %>% 
                   filter(sjr > minq & sjr <= maxq) %>% 
                   ungroup() 
                   
                   
               } else {
                   source_data  }
           }
    })
    
    
 # Plot for discipline only ####
    
    if (isTruthy(input_discs) & !isTruthy(input_org)) {

 
        # 1. counts ####
        if (input_pct_score == FALSE) { 
            
        myplot_data <- plot_data() %>% 
            filter(discs %in% input_discs) %>% 
            #filter(!duplicated(title)) %>% 
            group_by(org) %>% 
            count(discs) %>% 
          filter(n >= input_threshold_val)
        
        validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
        
        if (flip_status == TRUE) {
          
          x <- quote(reorder(factor(org), desc(n)))
          
        } else {
          x <- quote(reorder(factor(org), n))
        }
        
        myplot <- myplot_data %>% 
            ggplot(aes(x = eval(x), 
                       y = n, 
                       fill = discs,
                       text = paste(org, "\n",
                                    discs, "\n",
                                    n))) +
          {if (n_distinct(myplot_data$discs) < 10) {
            scale_fill_brewer(type = "qual", palette = "Set1") }} +
          geom_bar(stat = "identity", 
                     position = position_dodge(preserve = "single")) +
            labs(x = "", y = "", title = input_title) +
            theme_select 
        }
        
        # 2. pecentages score ####
        
        if (input_pct_score == TRUE) { 
            
            myplot_data <- plot_data() %>% 
                filter(discs %in% input_discs) %>% 
                group_by(org, discs) %>% 
                mutate(n = n()) %>% 
                filter(n >= input_threshold_val) %>% 
                mutate(pct = round((n()/total_org_disc), 3)) %>% 
                distinct(org, discs, .keep_all = TRUE) 
            
            # myplot_data <- plot_data() %>% 
            #     filter(discs %in% input_discs) %>% 
            #     #filter(!duplicated(title)) %>% 
            #     group_by(org) %>% 
            #     count(discs)
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(org), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(org), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = discs,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select 
        } 
        
        # 3. pecentages org ####
        
        if (input_pct_score_org == TRUE) { 
            
            myplot_data <- plot_data() %>% 
                filter(discs %in% input_discs) %>% 
                group_by(org, discs) %>% 
                mutate(n = n()) %>% 
                filter(n >= input_threshold_val) %>% 
                mutate(pct = round((n/total_org), 3)) %>% 
                distinct(org, discs, .keep_all = TRUE)  
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(org), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(org), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = discs,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select 
        } 

        
        # 4. legend on/off ####
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # 5. flip on/off ####
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
        
        
        # 1. counts ####
        if (input_pct_score == FALSE) { 
            
            myplot_data <- plot_data() %>% 
                filter(org %in% input_org) %>% 
                group_by(org) %>% 
                count(discs) %>% 
              filter(n >= input_threshold_val) 
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(discs), desc(n)))
              
            } else {
              x <- quote(reorder(factor(discs), n))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = n, 
                           fill = org,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        n))) +
              {if (n_distinct(myplot_data$org) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                labs(x = "", y = "", title = input_title) +
                theme_select 
        } 
    
        # 2. pecentages score ####
    if (input_pct_score == TRUE) { 
            
            myplot_data <- plot_data() %>% 
                filter(org %in% input_org) %>% 
                group_by(org, discs) %>% 
                mutate(n = n()) %>% 
                filter(n >= input_threshold_val) %>% 
                mutate(pct = round(n/total_org_disc, 3)) %>% 
                distinct(org, discs, .keep_all = TRUE) 
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(discs), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(discs), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = org,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$org) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select 
    } 
        
        # 3. pecentages org ####
        if (input_pct_score_org == TRUE) { 
            
            myplot_data <- plot_data() %>% 
                filter(org %in% input_org) %>% 
                group_by(org, discs) %>% 
                mutate(n = n()) %>% 
                filter(n >= input_threshold_val) %>% 
                mutate(pct = round((n/total_org), 3)) %>% 
                distinct(org, discs, .keep_all = TRUE) 
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(discs), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(discs), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = org,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$org) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select 
        } 
        
        # 4. legend on/off ####
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # 5. flip on/off ####
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
        
        # 1. counts ####
        if (input_pct_score == FALSE & input_axes == FALSE) { # Counts A ####
          
          myplot_data <- plot_data() %>% 
              filter(org %in% input_org) %>%
              filter(discs %in% input_discs) %>%
              #filter(!duplicated(title)) %>% 
              group_by(org) %>% 
              count(discs) %>% 
              filter(n >= input_threshold_val) 
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(org), desc(n)))
              
            } else {
              x <- quote(reorder(factor(org), n))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = n, 
                           fill = discs,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        n))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
                geom_bar(stat = "identity",
                         position = position_dodge(preserve = "single")) +
                # geom_bar(stat = "identity",
                #          position = position_stack()) +
                labs(x = "", y = "", title = input_title) +
                theme_select
            
            
          } 
      
      if (input_pct_score == FALSE & input_axes == TRUE) {  # Counts B ####
            
            myplot_data <- plot_data() %>% 
              filter(org %in% input_org) %>%
              filter(discs %in% input_discs) %>%
              #filter(!duplicated(title)) %>% 
              group_by(org) %>% 
              count(discs) %>% 
              filter(n >= input_threshold_val) 
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(discs), desc(n)))
              
            } else {
              x <- quote(reorder(factor(discs), n))
            }
            
            myplot <- myplot_data %>% 
              ggplot(aes(x = eval(x), 
                         y = n, 
                         fill = org,
                         text = paste(org, "\n",
                                      discs, "\n",
                                      n))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity",
                       position = position_dodge(preserve = "single")) +
              # geom_bar(stat = "identity",
              #          position = position_stack()) +
              labs(x = "", y = "", title = input_title) +
              theme_select
            
            
            }
        
        
        # 2. pecentages score ####
        if (input_pct_score == TRUE & input_axes == FALSE) { # pecentages score A ####
        
         
          
    
            
            myplot_data <- plot_data() %>% 
              filter(org %in% input_org) %>%
              filter(discs %in% input_discs) %>%
              #filter(!duplicated(title)) %>% 
              group_by(org, discs) %>%  
              mutate(n = n()) %>% 
              filter(n >= input_threshold_val) %>% 
              mutate(pct = round((n()/total_org_disc), 3)) %>% 
              distinct(org, discs, .keep_all = TRUE) #pct 
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(org), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(org), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = discs,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity",
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select
          
            } 
      
      if (input_pct_score == TRUE & input_axes == TRUE) { # pecentages score B ####
            
              myplot_data <- plot_data() %>% 
                filter(org %in% input_org) %>%
                filter(discs %in% input_discs) %>%
                #filter(!duplicated(title)) %>% 
                group_by(org, discs) %>%  
                mutate(n = n()) %>% 
                filter(n >= input_threshold_val) %>% 
                mutate(pct = round((n()/total_org_disc), 3)) %>% 
                distinct(org, discs, .keep_all = TRUE) #pct 
              
              
              validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
              
              if (flip_status == TRUE) {
                
                x <- quote(reorder(factor(discs), desc(pct)))
                
              } else {
                x <- quote(reorder(factor(discs), pct))
              }
              
              myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = org,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
                {if (n_distinct(myplot_data$discs) < 10) {
                  scale_fill_brewer(type = "qual", palette = "Set1") }} +
                geom_bar(stat = "identity",
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select
            
          }
        
        
        # 3. pecentages org ####
        if (input_pct_score_org == TRUE & input_axes == FALSE) { # pecentages org A #### 
            
         
          myplot_data <- plot_data() %>% 
            filter(org %in% input_org) %>%
              filter(discs %in% input_discs) %>%
              #filter(!duplicated(title)) %>% 
              group_by(org, discs) %>% 
              mutate(n = n()) %>% 
              filter(n >= input_threshold_val) %>% 
              mutate(pct = round((n()/total_org), 3)) %>% 
              distinct(org, discs, .keep_all = TRUE)  #pct 
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(org), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(org), pct))
            }
            
            myplot <- myplot_data %>% 
                ggplot(aes(x = eval(x), 
                           y = pct, 
                           fill = discs,
                           text = paste(org, "\n",
                                        discs, "\n",
                                        pct))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                         position = position_dodge(preserve = "single")) +
                scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
                labs(x = "", y = "", title = input_title) +
                theme_select 
            
          } 
      
      if (input_pct_score_org == TRUE & input_axes == TRUE) { # pecentages org B ####
          
            myplot_data <- plot_data() %>% 
            filter(org %in% input_org) %>%
              filter(discs %in% input_discs) %>%
              #filter(!duplicated(title)) %>% 
              group_by(org, discs) %>% 
              mutate(n = n()) %>% 
              filter(n >= input_threshold_val) %>% 
              mutate(pct = round((n()/total_org), 3)) %>% 
              distinct(org, discs, .keep_all = TRUE)  #pct 
            
            
            validate(need(nrow(myplot_data) > 0, "No data match these criteria!"))
            
            if (flip_status == TRUE) {
              
              x <- quote(reorder(factor(discs), desc(pct)))
              
            } else {
              x <- quote(reorder(factor(discs), pct))
            }
            
            myplot <- myplot_data %>% 
              ggplot(aes(x = eval(x), 
                         y = pct, 
                         fill = org,
                         text = paste(org, "\n",
                                      discs, "\n",
                                      pct))) +
              {if (n_distinct(myplot_data$discs) < 10) {
                scale_fill_brewer(type = "qual", palette = "Set1") }} +
              geom_bar(stat = "identity", 
                       position = position_dodge(preserve = "single")) +
              scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) + #pct
              labs(x = "", y = "", title = input_title) +
              theme_select 
            } 
        
      
        # 4. legend on/off ####
        if (legend_status == TRUE)  {
            myplot <- myplot +
                theme(legend.title = element_blank())
        } else {
            myplot <- myplot +
                theme(legend.position="none") 
        } 
        
        # 5. flip on/off ####
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
    
    # Conversion to Plotly ####
    if (exists("myplot")) {
        
   
        
       
        if (legend_status == TRUE) {
            

        ggplotly(myplot, tooltip = "text") %>% 
            layout(legend = list(orientation = "v",
                             #xanchor = "right",
                             #yanchor = "top",
                             x = input_leg_val_X,
                             y = input_leg_val_Y
                             ))
                
        } else {
            
            
            ggplotly(myplot, tooltip = "text")
            
        } 
    
    } # end plotly code
    

} # end of function definition