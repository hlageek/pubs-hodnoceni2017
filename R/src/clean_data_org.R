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
    mutate(org = str_replace(org, "MATERIÁLOVÝ A METALURGICKÝ VÝZKUM", "Materiálový a metalurgický výzkum"))

    feather::write_feather(eval_data_reduced, 
                           "data/processed/evaluation2017-2018.feather")

