library(rvest)
library(tidyverse)

ho2017_web <- read_html("https://hodnoceni17.rvvi.cz/www/biblio-vo")

reports <- ho2017_web %>% 
    html_nodes("a") %>% 
    html_attr('href') %>% 
    .[-c(1:5)]

instituce <- ho2017_web %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    .[-c(1:3)]

ziprename <- function (x) {
    
    str_replace_all(x, "[\\.\\,]", "") %>% 
        trimws(.) %>% 
    stringi::stri_trans_general(., 'Latin-ASCII') %>% 
    tolower(.) %>% 
    str_replace_all("\\s", "_") %>% 
        paste0("/", ., ".zip")
     }

websource <- data_frame(instituce, reports) %>% 
    mutate(instituce = str_replace(instituce, " - zip.*", "")) %>% 
    mutate(segment = case_when(str_detect(reports, "SEGMENT%20AV") ~ "AV",
                               str_detect(reports, "SEGMENT%20VS") ~ "VS",
                               str_detect(reports, "SEGMENT%20REZORTY") ~ "REZ")) %>% 
    mutate(reports = str_replace(reports, ".*/pub", "http://hodnoceni17.rvvi.cz/pub")) %>% 
    mutate(zip_path = case_when(segment == "AV" ~ "data/zipfiles/av",
                                segment == "VS" ~ "data/zipfiles/vs",
                                segment == "REZ" ~ "data/zipfiles/rez"
                                )) %>% 
    mutate(zip_path = paste0(zip_path, ziprename(instituce))) %>% 
    group_by(segment) %>% 
    sample_n(2) %>% 
    ungroup()

mkzipdir <- function(x) {if (!dir.exists(x)) dir.create(x, recursive = T)}

lapply(websource$zip_path, mkzipdir)

map2(websource$reports, websource$zip_path, ~download.file(.x, .y))
    
map(websource$zip_path, unzip)

websource[1,] %>% 
    mutate(dir = str_replace_all(zip_path, "\\/[^\\/]*?zip", "")) %>% 
    map2(.$zip_path, .$dir, ~paste0("7z -o", .y, " -y x ", .x))

unarchiver <- function(x, dir) {
    

    for (i in seq(x)) {
        
        file <- x[i]
        system(paste0("7z -o", dir, " -y x ", file))
    }
}


unarchiver(list.files(zip_av, full.names = T), zip_av)
unarchiver(list.files(zip_vs, full.names = T), zip_vs)
unarchiver(list.files(zip_rez, full.names = T), zip_rez)

list.dirs(zip_rez) %>% iconv(from = "LATIN2", to = "UTF-8")
