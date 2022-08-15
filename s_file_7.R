## s_file_7.R: Retrieving the Administrative Arrangement Orders from the
## Federal Register of Legislation

## Patrick Leslie
## May 2022


# rm(list=ls())

# Load in base file

aao_url <- read.csv("inputs/administrative_arrangement_orders_raw/aao_url.txt")

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



# listed text of principal acts as made (which were later amended same term)

for (i in 1:59){
  
  print(i)
  
  # as made act text url
  url <-   aao_url$URL[i]
    
  
  
  page<-NA
  while (is.na(act)) {
    page<-try_url(url)
  }
  
  #code from stackoverflow
  doc_search <- read_html(url) 
    

  id_doc<- doc_search %>% html_nodes("body") %>%
    html_elements("a")%>%
    html_attr("id") %>% as_tibble()%>%
    filter(!is.na(value)) %>%
    filter(grepl("PrimaryDoc", value)) %>%
    as.character()
  
  id_pdf<-NULL
  id_pdf<- doc_search %>% html_nodes("body") %>%
    html_elements("a")%>%
    html_attr("id") %>% as_tibble()%>%
    filter(!is.na(value)) %>%
    filter(grepl("ctl00_ctl04_hlPDF", value)) %>%
    as.character()
  
  doc_path<- "inputs/administrative_arrangement_orders_raw/"
  
  
  if(id_pdf!="character(0)"){

    doc_url<-read_html(url) %>% html_elements(paste0("#",id_doc)) %>%
      html_attr("href")
    download.file(doc_url, paste0(doc_path, "AAO_",aao_url$START_DATE[i],".doc"))
    
  }else{
    doc_url<-read_html(url) %>% html_elements(paste0("#",id_doc)) %>%
      html_attr("href")
    download.file(doc_url, paste0(doc_path, "AAO_",aao_url$START_DATE[i],".pdf"))
  }

}


for (i in 60:nrow(aao_url)){
  
  print(i)
  
  # as made act text url
  url <-   paste0(aao_url$URL[i],"/Download")
  
  
  
  page<-NA
  while (is.na(act)) {
    page<-try_url(url)
  }
  
  #code from stackoverflow
  doc_search <- read_html(url) 
  
  
  id_doc<- doc_search %>% html_nodes("body") %>%
    html_elements("a")%>%
    html_attr("id") %>% as_tibble()%>%
    filter(!is.na(value)) %>%
    filter(grepl("PrimaryDoc", value)) %>%
    as.character()
  
  id_pdf<-NULL
  id_pdf<- doc_search %>% html_nodes("body") %>%
    html_elements("a")%>%
    html_attr("id") %>% as_tibble()%>%
    filter(!is.na(value)) %>%
    filter(grepl("ctl00_ctl04_hlPDF", value)) %>%
    as.character()
  
  doc_path<- "inputs/administrative_arrangement_orders_raw/"
  
  
  if(id_pdf!="character(0)"){
    
    doc_url<-read_html(url) %>% html_elements(paste0("#",id_doc)) %>%
      html_attr("href")
    download.file(doc_url, paste0(doc_path, "AAO_",aao_url$START_DATE[i],".doc"))
    
  }else{
    doc_url<-read_html(url) %>% html_elements(paste0("#",id_doc)) %>%
      html_attr("href")
    download.file(doc_url, paste0(doc_path, "AAO_",aao_url$START_DATE[i],".pdf"))
  }
  
}
