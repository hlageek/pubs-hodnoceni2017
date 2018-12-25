library(rvest)
library(tidyverse)
library(readxl)

if (!dir.exists("data/scales2017")) dir.create("data/scales2017", recursive = T)

if (!file.exists("data/scales2017/scales2017.xlsx")) {
download.file("https://www.rvvi.cz/data/x_novinky/64/files/souhrnne_zpravy_pro_poskytovatele_segmenty.xlsx", "data/scales2017/scales2017.xlsx") } else {
    cat("File already downloaded")}

avcr <- readxl::read_excel("data/scales2017/scales2017.xlsx", "AV ČR", skip = 2) %>% 
    janitor::clean_names() %>% 
    filter(!is.na(poskytovatel)) %>% 
    na_if("N/A")

vs <- readxl::read_excel("data/scales2017/scales2017.xlsx", "VŠ", skip = 2)  %>% janitor::clean_names() %>% 
    filter(!is.na(poskytovatel)) %>% 
    na_if("N/A") 

rez <- readxl::read_excel("data/scales2017/scales2017.xlsx", "Rezorty", skip = 2)  %>% 
    janitor::clean_names() %>% 
    filter(!is.na(poskytovatel)) %>% 
    na_if("N/A")           


    
path <- "data/scales2017/scales2017.xlsx"

fun <- function(x) if ("pocet_studentu" %in% names(x)) x %>% select(-pocet_studentu, pocet_studentu) %>%  mutate(fixace_v_tis_kc = as.numeric(fixace_v_tis_kc)) else cbind(x, pocet_studentu = 0) %>% mutate(fixace_v_tis_kc = as.numeric(fixace_v_tis_kc))
totals <- path %>% 
    excel_sheets() %>% 
    .[-4] %>% 
    set_names() %>% 
    map(read_excel, path = path, skip = 2) %>% 
    map(., janitor::clean_names) %>% 
    map(function(x) { x[nrow(x),] }) %>% 
    map(., fun) 

totals <- bind_rows(totals) %>% 
    mutate(poskytovatel = c("AV ČR", "Rezorty", "VŠ"))

avcr <- avcr %>% na_if("N/A") %>% 
    mutate(segment = "AVČR")
vs <- vs %>% na_if("N/A") %>% 
    mutate(ic = as.numeric(ic)) %>% 
    mutate(segment = "VŠ") 
rez <- rez %>% na_if("N/A") %>% 
    mutate(ic = as.numeric(ic),
           fixace_v_tis_kc = as.numeric(fixace_v_tis_kc),
           segment = "Rezorty")


scaled_org <- bind_rows(avcr, vs, rez)
    

save(scaled_org, totals, file = "data/scales2017/scales2017_processed.RData")
