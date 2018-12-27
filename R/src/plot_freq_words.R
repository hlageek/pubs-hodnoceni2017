


plot_freq_words <- function(source_data, input_org, input_discs, input_theme, input_title, input_pct_low, input_pct_high) {
    
req(source_data)
    # Theme selector ####
    theme_select <- if (input_theme == "Void") theme_void() else 
                    if (input_theme == "Stats")  theme_classic() else 
                    if (input_theme == "Grid")  theme_bw() else
                    if (input_theme == "Classic") theme_gray()
    
    # Percentile filter ####
    
    
    source_data <- source_data %>%
       { if (!is.na(.$ais[1])) { # filter AIS for WOS data
            {
                if (input_pct_high < 100 |
                    input_pct_low > 0)
                    filter(., between(
                        ais,
                        ecdf_fun(ais, input_pct_low / 100),
                        ecdf_fun(ais, input_pct_high / 100)
                    ))
                else
                    .
            }
       } else if (!is.na(.$sjr[1])) { # filter SJR for SCOPUS data
           {
               if (input_pct_high < 100 |
                   input_pct_low > 0)
                   filter(., between(
                       sjr,
                       ecdf_fun(sjr, input_pct_low / 100),
                       ecdf_fun(sjr, input_pct_high / 100)
                   ))
               else
                   . }
       }    
       }
    
    # Plot for discipline only ####
    # 
    #     if (isTruthy(input_discs) & !isTruthy(input_org)) {
    #         
    #         myplot <- source_data %>% 
    #             filter(discs %in% input_discs) %>% 
    #             group_by(org) %>% 
    #             count(discs) %>% 
    #             ggplot(aes(x = reorder(factor(org), n), 
    #                        y = n, 
    #                        fill = discs,
    #                        text = paste(org, "\n",
    #                                     discs, "\n",
    #                                     n))) +
    #             geom_bar(stat = "identity", 
    #                      position = position_dodge(preserve = "single")) +
    #             labs(x = "", y = "", title = input_title) +
    #             theme_select +
    #             theme(legend.title = element_blank()) +
    #             coord_flip()
    #         
    #         # myplot <- bibliometric_plot(data = source_data, 
    #         #                             filter2 = discs, 
    #         #                             filter2use = input_discs, 
    #         #                             main = discs, 
    #         #                             secondary = org, 
    #         #                             themer = theme_select,
    #         #                             title2 = input_title)
    #         
    #         
    #         } 
    # 
    # # Plot for organization only ####  
    #     if (!isTruthy(input_discs) & isTruthy(input_org)) {
    #         
    #                 
    #                 myplot <- source_data %>% 
    #                     filter(org %in% input_org) %>% 
    #                     group_by(org) %>% 
    #                     count(discs) %>% 
    #                     ggplot(aes(x = reorder(factor(discs), n), 
    #                                y = n, 
    #                                fill = org,
    #                                text = paste(org, "\n",
    #                                             discs, "\n",
    #                                             n))) +
    #                     geom_bar(stat = "identity", 
    #                              position = position_dodge(preserve = "single")) +
#                     labs(x = "", y = "", title = input_title) +
#                     theme_select +
#                     theme(legend.title = element_blank()) +
#                     coord_flip() #+
#                 #scale_fill_brewer(type = "qual", palette = "Set1", direction = 1)
#                 
#             
#             }
# # Plot for both discipline and organization ####    
# 
#     if (isTruthy(input_discs) & isTruthy(input_org)) {
#         
#         
#         myplot <- source_data %>% 
#             filter(org %in% input_org) %>%
#             filter(discs %in% input_discs) %>%
#             group_by(org) %>% 
#             count(discs) %>% 
#             ggplot(aes(x = reorder(factor(discs), n), 
#                        y = n, 
#                        fill = org,
#                        text = paste(org, "\n",
#                                     discs, "\n",
#                                     n))) +
#             geom_bar(stat = "identity", 
#                      position = position_dodge(preserve = "single")) +
#             labs(x = "", y = "", title = input_title) +
#             theme_select +
#             theme(legend.title = element_blank()) +
#             coord_flip() #+
#         #scale_fill_brewer(type = "qual", palette = "Set1", direction = 1)
#         
#         
#     }
#     
    
# word plot
    
    myplot <- source_data %>% 
        filter(org %in% input_org) %>% 
        unnest_tokens(words, title) %>% 
        anti_join(stop_words, by=c("words" = "word")) %>%  count(words, org) %>% 
            arrange(desc(n)) %>% 
            top_n(25, n) %>% 
        ggplot(aes(x = reorder(factor(words), n), 
                   y = n, 
                   text = paste(words, "\n",
                                n), fill = org)) +
        geom_bar(stat = "identity", 
                 position = position_dodge(preserve = "single")) +
        labs(x = "", y = "", title = input_title) +
        theme_select +
        theme(legend.title = element_blank()) +
        coord_flip() #+
        #{if (length(unique(.$org)) > 1) scale_fill_manual(values = "#F8766D")}
    #scale_fill_brewer(type = "qual", palette = "Set1", direction = 1)
    
    
   
# conversion to Plotly ####

    if (exists("myplot")) {
    ggplotly(myplot, tooltip = "text") %>% 
        layout(legend = list(orientation = "v",
                             xanchor = "right",
                             y = 0))
    } # end plotly code
    
    
} # end of function definition