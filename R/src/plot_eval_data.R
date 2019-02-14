
plot_eval_data <- function(source_data, # capture source WoS or Scopus
                           input_org, # capture organizations
                           input_discs, # capture disciplines
                           input_theme, 
                           input_title, 
                           input_pct_low, 
                           input_pct_high
) {
    
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
        geom_bar(stat = "identity", 
                 position = position_dodge(preserve = "single")) +
        labs(x = "", y = "", title = input_title) +
        theme_select +
        theme(legend.title = element_blank()) +
        coord_flip() #+
    #scale_fill_brewer(type = "qual", palette = "Set1", direction = 1)
    
    
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
        geom_bar(stat = "identity", 
                 position = position_dodge(preserve = "single")) +
        labs(x = "", y = "", title = input_title) +
        theme_select +
        theme(legend.title = element_blank()) +
        coord_flip() #+
    #scale_fill_brewer(type = "qual", palette = "Set1", direction = 1)
    
    
}

# conversion to Plotly ####

if (exists("myplot")) {
    )
, tooltip = "text") %>% 
    layout(legend = list(orientation = "v",
                         xanchor = "right",
                         y = 0))
} # end plotly code
    
} # end of function definition