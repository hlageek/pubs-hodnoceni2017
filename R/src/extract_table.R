

library(pdftools)
library(dplyr)

x <- pdf_data("/Users/radimhladik/ownCloud/erko/hodnoceni2017/data/zipfiles/rez/MD 2/Centrum dopravn°ho vÏzkumu, v.v.i/Centrum dopravn°ho vÏzkumu, v. v. i.pdf")

y <- x[[2]] %>% 
    mutate(line_no = if_else(lag(space) == F & between(y, (y-3), (y+3))  | 
                                 is.na(lag(space)) & y != lag(y) |
                                 is.na(lag(y)), 1, 0)) %>% 
    mutate(line_no = as.integer(cumsum(line_no))) %>% 
    group_by(line_no) %>% 
    mutate(line_text = paste(text, collapse = " ")) %>% 
    distinct(line_no, .keep_all = T) %>% 
    ungroup() 

y <- pdf_text("/Users/radimhladik/ownCloud/erko/hodnoceni2017/data/zipfiles/rez/MD 2/Centrum dopravn°ho vÏzkumu, v.v.i/Centrum dopravn°ho vÏzkumu, v. v. i.pdf")

obor <- y %>% filter(x == 53 & y > y[line_text == "Obor"]) %>% 
     mutate(line_text = if_else(lead(y) - y < 15 & !is.na(lead(y)),  
                                paste(line_text, lead(line_text)), 
                                line_text)) %>% 
     filter(abs(lag(y) - y) > 15 | is.na(lag(y)))
 

y <- x[[2]] %>% 
    mutate(ymax = y+10, ymin = y-10) %>% 
    mutate(line_no = if_else(lead(y) < ymax & lag(y) > ymin, 0, 1))

           