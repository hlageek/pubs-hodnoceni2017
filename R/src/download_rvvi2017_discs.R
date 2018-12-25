library(rvest)
library(tidyverse)

ho2017_web <- read_html("https://hodnoceni17.rvvi.cz/www/biblio-obory")

links <- ho2017_web %>% 
    html_nodes("a") %>% 
    html_attr('href') %>% 
    .[str_detect(., "zip")] %>% 
    .[!duplicated(.)]

discs <- ho2017_web %>% 
    html_nodes("li") %>% 
    html_text() %>% 
    .[str_detect(., "zip")]


ziprename <- function (x) {
    
    str_replace_all(x, "[\\.\\,]", "") %>% 
        trimws(.) %>% 
        stringi::stri_trans_general(., 'Latin-ASCII') %>% 
        tolower(.) %>% 
        str_replace_all("\\s", "_") %>% 
        paste0("/", ., ".zip")
}

websource <- data_frame(discs, links) %>% 
    mutate(discs = str_replace(discs, " - zip.*", "")) %>% # remove the end of captured links texts
    filter(str_detect(discs, "komplet")) %>% # pick only rows with complete data
    mutate(links = str_replace(links, ".*/pub", "http://hodnoceni17.rvvi.cz/pub")) %>% # relative to absolute url
    mutate(discs = str_replace_all(discs, "[\r\n]", "")) %>% # remove linebreaks
    mutate(discs = str_replace_all(discs, "(OBORY\\s)|(\\s-\\s*komplet\\s*zip.*)", "")) %>% # clean names
    mutate(dirs = paste0("data/discs/", 
                         str_replace_all(
        tolower(
        stringi::stri_trans_general(discs, 'Latin-ASCII')
        ),
        "\\s", "_")
        )) %>%  # create dirs paths
    mutate(zipfiles = paste0(dirs, "/", 
                             str_replace_all(
        tolower(
            stringi::stri_trans_general(discs, 'Latin-ASCII')
            ),
        "\\s", "_"),
        ".zip")) # create zip files paths

mkdirs <- function(x) {if (!dir.exists(x)) dir.create(x, recursive = T)} # create fun to make dirs

walk(websource$dirs, ~ mkdirs(.x)) # apply the function

if (sum(str_detect(list.files("data/discs"), "zip")) < 1) {
walk2(websource$links, str_replace(websource$zipfiles, "(.*discs/).*/(.*)", "\\1\\2"), ~download.file(.x, .y)) # download files from web to target files
} else {cat("Files are already downloaded")}

if (sum(str_detect(list.files("data/discs/bibliometrie_clanky_wos"), "zip")) < 1) {
websource %>% 
    mutate(zipfile = str_replace(zipfiles, "(.*discs/).*/(.*)", "\\1\\2")) %>% 
    # modify zipfiles paths
    select( zipfile,
            exdir = dirs) %>% # select columns to serve as arguments to unzip
    pwalk(unzip, junkpaths = TRUE) # unzip files

    } else {
        print(
            list.files("data/discs/bibliometrie_clanky_wos")[str_detect(list.files("data/discs/bibliometrie_clanky_wos"), "zip")]
        ) }

unzip2 <- function(zipfile, exdir) {system(paste0("7z -o", exdir, " -y e ", shQuote(zipfile)))}

if (sum(str_detect(list.files("data/discs/bibliometrie_clanky_wos"), "xls")) < 1) {
websource %>% select(-zipfiles, -links) %>% 
    mutate(files = {map(.$dirs, list.files)}) %>% 
    unnest(files) %>% 
    mutate(files = paste0(dirs, "/", files)) %>% 
    filter(str_detect(files, "zip")) %>% 
    select( zipfile = files,
            exdir = dirs) %>% # select columns to serve as arguments to unzip
    pwalk(unzip2) # unzip files

    } else {
    
        print(
        list.files("data/discs/bibliometrie_clanky_scopus")[str_detect(list.files("data/discs/bibliometrie_clanky_scopus"), "xls")]
    ) }
Sys.sleep(5)

sheets <- websource %>% select(-zipfiles, -links) %>% #### bacha
    mutate(files = {map(.$dirs, list.files)}) %>% 
    unnest(files) %>% 
    mutate(files = paste0(dirs, "/", files)) %>% 
    filter(str_detect(files, "xls")) %>% 
    mutate(filenames = str_replace_all(files, "[\\.][^(\\.xls)]", "_")) %>% 
    mutate(filenames = str_replace_all(filenames, "\\s", "_")) %>% 
    mutate(segment = case_when(discs == "Bibliometrie články SCOPUS" ~ "scopus",
                               discs == "Bibliometrie články WoS" ~ "wos",
                               discs == "Bibliometrie sborníky" ~ "proceedings"))

Sys.sleep(5)

sheets %>%
{walk2(.$files, .$filenames, ~file.copy(.x, .y))}

Sys.sleep(5)

national_results <- sheets  %>% 
    select(segment, files, filenames) %>% 
    mutate(discs = str_replace(files, "^.*?(\\d{1,2}[\\.\\s].*)\\.xls[x]$", "\\1")) %>% 
    mutate(pubs = map(.$filenames, readxl::read_excel, "Národní výsledky")) %>% 
    unnest(pubs) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty(c("rows", "cols")) %>% 
    mutate(pub = pmap_chr(.[c("nazev_publikace", "nazev_casopisu", "nazev_periodika")], 
                          ~paste(na.omit(c(...)), collapse = ""))) %>%
    select(segment, 
           discs, 
           title = vysledek, 
           org = vyzkumna_organizace,
           pub, ais, sjr, issn) %>% 
    mutate(pub = tools::toTitleCase(tolower(pub)),
           title = tools::toTitleCase(tolower(title)))



journals_wos <- sheets %>% 
    select(segment, files, filenames) %>% 
    mutate(discs = str_replace(files, "^.*?(\\d{1,2}[\\.\\s].*)\\.xls[x]$", "\\1")) %>% 
    filter(segment == "wos") %>% 
    mutate(wos = map(.$filenames, readxl::read_excel, "WOS časopisy")) %>% 
    unnest(wos )%>% 
    janitor::clean_names() %>% 
    janitor::remove_empty(c("rows", "cols"))%>% 
    rename(title = "nazev_casopisu") %>% 
    mutate(title = tools::toTitleCase(tolower(title)))

journals_scopus <- sheets  %>% 
    select(segment, files, filenames) %>% 
    mutate(discs = str_replace(files, "^.*?(\\d{1,2}[\\.\\s].*)\\.xls[x]$", "\\1")) %>% 
    filter(segment == "scopus") %>% 
    mutate(scopus = map(.$filenames, readxl::read_excel, "Scopus časopisy")) %>% 
    unnest(scopus) %>% 
    janitor::clean_names() %>% 
    janitor::remove_empty(c("rows", "cols")) %>% 
    mutate(title = tools::toTitleCase(tolower(title)))

journals <- bind_rows(journals_scopus, journals_wos) %>% 
    select(segment, discs, title, issn, e_issn, ais, sjr)

if (!file.exists("data/processed/national_results.tsv")) write_tsv(national_results, "data/processed/national_results.tsv")
if (!file.exists("data/processed/journals.tsv")) write_tsv(journals, "data/processed/journals.tsv")
