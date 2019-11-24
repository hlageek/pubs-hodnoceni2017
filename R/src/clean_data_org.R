require(tidyverse)

data2018 <- feather::read_feather("data/processed/evaluation2018.feather")
data2017 <- feather::read_feather("data/processed/evaluation2017.feather")
eval_data <- bind_rows(data2017, data2018)


# check spelling interactively
eval_data_reduced %>% pull(org) %>% unique() %>% 
    str_remove(",(\\s)?([vosazp]\\.|spol\\.|příspěvková).*") %>% grep(",", ., value = T)     


eval_data_reduced <- eval_data %>% 
    select(id, year, disc_group, discs, segment,org, quantiles) %>% 
    mutate(org = str_split(org, ";")) %>% 
    unnest(org) %>% 
    mutate(org = trimws(org)) %>% 
    mutate(org = str_remove(org, "[,\\s](\\s)?([vosazp]\\.|spol\\.|příspěvková).*")) %>% 
    mutate(org = str_replace(org, "MATERIÁLOVÝ A METALURGICKÝ VÝZKUM", "Materiálový a metalurgický výzkum")) %>% 
    # the below segment was moved here from on the fly implementation in the app
    group_by(discs, org) %>% 
    mutate(total_org_disc = n()) %>% 
    group_by(org) %>% 
    mutate(total_org = n()) %>%
    group_by(discs) %>% 
    mutate(total_disc = n()) %>% 
    ungroup()

    feather::write_feather(eval_data_reduced, 
                           "data/processed/evaluation2017-2018.feather")

