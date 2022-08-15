## s_file_6.R: adding an R version of git diff of the scale of the change to acts 
## amended before the end of their first term. 
## Patrick Leslie
## May 2022

## EDIT: unfortunately it is really hard to find compilations of the acts going
## back such a long way. Abandoning this as a stub for now


# rm(list=ls())

# Load in base file

a_file_2 <- read_csv("outputs/a_file_2_acts_plus_event.csv")

# loading in packages...


## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse); 
library(RSelenium);library(curl);library(httr); 
library(doParallel); library(foreach);library(data.table)
registerDoParallel(cores=32)
httr::reset_config()



#function to try urls until successful read (adapted from https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r)


try_url <- function(url){
  out<-tryCatch(
    {
      GET(url,timeout(60))%>%read_html()
    },
    error = function(cond){
      message('Caught an error!')
      return(NA)
    },
    warning = function(cond){
      message('Caught a warning!')
      return(NA)
    },
    finally = {
      message('All done, quitting.')
    }
  )  
  return(out)
}


# vector of principal acts that were amended...

Amended_same_term<- a_file_2[a_file_2$amended_same_term==1,1]$id_principal

# listed text of principal acts as made (which were later amended same term)

acts_details<- foreach(i=1:length(Amended_same_term),.verbose=T,.errorhandling="pass",.combine=rbind) %dopar% {
  
  print(i)
  
  # as made act text url
  url <-   paste0("https://www.legislation.gov.au/Details/",Amended_same_term[i])
  
  
  act<-NA
  while (is.na(act)) {
    act<-try_url(url)
  }
  
  act_text<-NULL
  act_text <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//div[contains(@class, 'Section')]") %>%
    html_text() %>% str_replace_all("\\s+", " ")%>%
    trimws()
  
  act_text <- paste(act_text, collapse = " ")
  
  dat<-NULL
  dat<- bind_cols(id_principal= Amended_same_term[i],
                  act_text=act_text)
  return(dat)
}

# listed text of principal acts as amended after first amendment (those amended same term)



# difference between acts as made and as amended