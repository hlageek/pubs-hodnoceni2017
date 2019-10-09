library(rvest)
library(tidyverse)

ho2018_web <- read_html("https://hodnoceni18.rvvi.cz/www/biblio-obory")

links <- ho2018_web %>% 
    html_nodes("a") %>% 
    html_attr('href') %>% 
    .[str_detect(., "zip")] %>% 
    .[!duplicated(.)] %>% 
    .[-1]

discs <- ho2018_web %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    .[str_detect(., "zip")] %>% 
    .[-1]



websource <- tibble(discs, links) %>% 
    mutate(discs = trimws(str_replace(discs, " - zip.*", ""))) %>% # remove the end of captured links texts
    mutate(links = str_replace(links, ".*/public", "http://hodnoceni18.rvvi.cz/public")) %>% 
    mutate(dirs = paste0("data/discs2018/", 
                         str_replace_all(
        tolower(
        stringi::stri_trans_general(discs, 'Latin-ASCII')
        ),
        "\\s{1,}", "_")
        )) %>%  # create dirs paths
    mutate(zipfiles = paste0(dirs, "/", 
                             str_replace_all(
        tolower(
            stringi::stri_trans_general(discs, 'Latin-ASCII')
            ),
        "\\s{1,}", "_"),
        ".zip")) # create zip files paths

mkdirs <- function(x) {if (!dir.exists(x)) dir.create(x, recursive = T)} # create fun to make dirs

walk(websource$dirs, ~ mkdirs(.x)) # apply the function

if (sum(str_detect(list.files("data/discs2018", recursive = TRUE), "zip")) < 1) {
walk2(websource$links, str_replace(websource$zipfiles, "(.*discs/).*/(.*)", "\\1\\2"), ~download.file(.x, .y)) # download files from web to target files
} else {cat("Files are already downloaded")}

unzip2 <- function(zipfile, exdir) {system(paste0("7z x -o", exdir, " -y ", shQuote(zipfile)))}


websource %>% select(-zipfiles, -links) %>% 
    mutate(files = {map(.$dirs, list.files)}) %>% 
    unnest(files) %>% 
    mutate(files = paste0(dirs, "/", files)) %>% 
    filter(str_detect(files, "zip")) %>% 
    select( zipfile = files,
            exdir = dirs) %>% # select columns to serve as arguments to unzip
    pwalk(unzip2) # unzip files

sheets <- websource %>% select(-zipfiles, -links) %>% #### bacha
    mutate(files = map(.$dirs, list.files)) %>%
    mutate(files = map(.$files, 1)) %>% 
    mutate(files = paste0(dirs, "/", files)) %>% 
    mutate(files = map(.$files, list.dirs)) %>% 
    unnest(files) %>% 
    filter(str_detect(files, "Bibliometrie SCOPUS|Bibliometrie WoS")) %>% 
    mutate(disc_id = str_extract(files, "\\d\\.\\d")) %>% 
    mutate(segment = tolower(str_extract(files, "SCOPUS|WoS"))) %>% 
    filter(!duplicated(paste(disc_id, segment))) %>% 
    mutate(excel = map(.$files, list.files)) %>% 
    unnest(excel) %>% 
    filter(str_detect(excel, "Priloha3-journals\\.xlsx|Priloha3\\.xlsx")) %>% 
    mutate(excel = paste(files, excel, sep = "/")) %>% 
    mutate(source_excel = map(.$excel, readxl::read_excel)) %>% 
    unnest(source_excel) %>% 
    janitor::clean_names() %>% 
    mutate(id = rownames(.))
        
    
cleaned_sheets <- sheets %>% 
    select(-c(dirs, files, disc_id, excel, ut_wo_s)) %>% 
     rename(disc_group = discs,
            discs = ford,
            year = rok_uplatneni,
            title = vysledek,
            org = vo,
            pub = nazev_casopisu,
            quantiles = kvartilove_pasmo_casopisu,
            author = autor,
            cz_author = cz_autor,
            international = mezinarodni_spoluprace) %>% 
     mutate(disc_group = str_replace(disc_group, "^[\\W\\d]*", "")) %>% 
     mutate(discs = str_replace(discs, "^[\\W\\d]*", "")) %>% 
     mutate(quantiles = case_when(quantiles == "Decil" ~ 1,
                                  quantiles == "Q1" ~ 2,
                                  quantiles == "Q2" ~ 3,
                                  quantiles == "Q3" ~ 4,
                                  quantiles == "Q4" ~ 5))
     
    

if (!file.exists("data/processed/evaluation2017.feather")) {
    feather::write_feather(cleaned_sheets %>% 
                               filter(year == 2016), 
                           "data/processed/evaluation2017.feather")
}

if (!file.exists("data/processed/evaluation2018.feather")) {
    feather::write_feather(cleaned_sheets %>% 
                               filter(year == 2017), 
                           "data/processed/evaluation2018.feather")
}

