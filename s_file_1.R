## Scraping the Federal Register of Legislation
## Patrick Leslie
## 29th October 2021

## Packages

library(rvest)
library(stringr)

## Initial URLs 


for(i in 1901:2021){
  url <- paste0("https://www.legislation.gov.au/Browse/Results/ByYearNumber/Acts/Asmade/",i,"/0/0/principal")
  year_leg  <- read_html(url)
  
  body_nodes <- year_leg %>%
    html_node("body")
  
  body_nodes
}

