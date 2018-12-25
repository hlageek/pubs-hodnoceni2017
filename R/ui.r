


# rebase check
# Define UI
shinyUI(
  
  navbarPage(tags$a(href = 'http://www.flu.cas.cz/cz', tags$img(src = 'flu_logo.png', width = 20)), windowTitle = "Evaluation 2017",
  
             page1,

tabPanel("Ranking data",
        fluidPage(
            
            textOutput("wd")
            
        )),

tabPanel("About",
         fluidPage(
             
             includeMarkdown("www/about.Rmd")
             
         )) # tabpanel end# tabpanel end
) #end navbar page

) # shinyUI end 
# Last call
