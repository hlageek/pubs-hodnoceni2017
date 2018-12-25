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

websource <- data_frame(instituce, reports) %>% 
    mutate(instituce = str_replace(instituce, " - zip.*", "")) %>% 
    mutate(segment = case_when(str_detect(reports, "SEGMENT%20AV") ~ "AV",
                               str_detect(reports, "SEGMENT%20VS") ~ "VS",
                               str_detect(reports, "SEGMENT%20REZORTY") ~ "REZ"))

zip_av <- file.path("data", "zipfiles", "av")
zip_vs <- file.path("data", "zipfiles", "vs")
zip_rez <- file.path("data", "zipfiles", "rez")

sheets_vs <- list.files(zip_vs, recursive = T, full.names = T, pattern = ".xls")

library(readxl)

test <- readxl::read_excel(sheets_vs[1], sheet = 3, skip = 1)
