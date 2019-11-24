require(tidyverse)

data2018 <- feather::read_feather("data/processed/evaluation2018.feather")
data2017 <- feather::read_feather("data/processed/evaluation2017.feather")
eval_data <- bind_rows(data2017, data2018)

eval_data_reduced <- eval_data %>% 
    select(id, year, big_discs, discs, segment,org, quantile) %>% 
    mutate(org = str_split(org, ";")) %>% 
    unnest(org) %>% 
    mutate(org = trimws(org))

# check spelling interactively
eval_data_reduced %>% pull(org) %>% unique() %>% 
    str_remove(",(\\s)?([vosazp]\\.|spol\\.|příspěvková).*") %>% grep(",", ., value = T)     


eval_data_reduced <- eval_data %>% 
    select(id, year, big_discs, discs, segment,org, quantile) %>% 
    mutate(org = str_split(org, ";")) %>% 
    unnest(org) %>% 
    mutate(org = trimws(org)) %>% 
    mutate(org = str_remove(org, "[,\\s](\\s)?([vosazp]\\.|spol\\.|příspěvková).*")) %>% 
    mutate(org = str_replace(org, "MATERIÁLOVÝ A METALURGICKÝ VÝZKUM", "Materiálový a metalurgický výzkum"))

if (!file.exists("data/processed/evaluation2017-2018.feather")) {
    feather::write_feather(eval_data_reduced, 
                           "data/processed/evaluation2017-2018.feather")
}
