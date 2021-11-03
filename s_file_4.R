## Scraping the Federal Register of Legislation
## s_file_4: Details of Amending Acts 1901-2021
## Patrick Leslie
## November 2021

# rm(list=ls())

## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse); 
library(RSelenium);library(curl);library(httr); 
library(doParallel); library(foreach);library(data.table)
registerDoParallel(cores=32)

## Amending Acts 


amending_acts <- read_csv("outputs/secure outputs/s_file_2_amending_acts.csv")

amending_acts<-amending_acts%>%arrange(date_enacted)



acts_details<- foreach(i=1:100,.verbose=T,.errorhandling="stop",.combine=rbind) %dopar% {
  
  print(i)
  
  url <-   paste0("https://www.legislation.gov.au/Series/",amending_acts$id_amending[i])
  
  act  <- url%>% GET(.,timeout(30))%>%read_html()
  
  
  administered_by<-NA
  date_repealed<-NA
  repealing_act_id<-NA
  word_count<- NA
  principal_acts<-NA
  
  administered_by <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//span[contains(@id, 'AdminDepts')]") %>%
    html_text()
  administered_by<-gsub("Administered by:\\s+", "",administered_by)
  administered_by<-administered_by[1]
  
  date_repealed <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//span[contains(@id, 'lblRepealed')]") %>%
    html_text() 
  date_repealed<-date_repealed[2]
  
  
  repealing_act_id <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//span[contains(@id, 'MainContent_SeriesPane_lblRepealingReason')]") %>%
    xml2::xml_children() %>% html_attr("href") %>%
    str_remove("https://www.legislation.gov.au/Details/")
  repealing_act_id<-repealing_act_id[1]
  
  #word count
  url <-   paste0("https://www.legislation.gov.au/Details/",amending_acts$id_amending[i])
  
  #test url
  #url<-"https://www.legislation.gov.au/Details/C2004A05138"
  
  act  <- url%>% GET(.,timeout(30))%>%read_html()
  
  word_count <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//div[contains(@class, 'Section')]") %>%
    html_text() %>% str_replace_all("\\s+", " ")%>%
    str_count(" ") %>%sum()
  
  
  # Principal acts that are amended by this legislation
  url <-   paste0("https://www.legislation.gov.au/Details/",amending_acts$id_amending[i],"/Amends")
  
  #test url
  #url<- "https://www.legislation.gov.au/Details/C2007A00128/Amends"
  
  act  <- url%>% GET(.,timeout(30))%>%read_html()
  
  principal_acts <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//tr[contains(@id, 'ctl00_MainContent_SeriesAmendments_RadGrid1_ctl00')]") %>%
    xml2::xml_find_all("//td")%>%html_text()%>%
    str_extract_all("C[0-9]+A[0-9]+")%>%unlist()%>%
    unique()
  
  # binding together
  dat<-NULL
  dat<- bind_cols(id_amending= amending_acts$id_amending[i],
                  id_principal = principal_acts,
                  name = amending_acts$name[i],
                  date_enacted = amending_acts$date_enacted[i],
                  year_enacted = amending_acts$year_enacted[i],
                  administered_by=administered_by,
                  date_repealed=date_repealed,
                  repealing_act_id=repealing_act_id,
                  word_count=word_count)
  return(dat)
}

#cleaning for export 

dat1$date_enacted  <- format(dat1$date_enacted,  "%Y%m%d")
dat1$date_repealed <- as.Date(dat1$date_repealed, format= "%d %b %Y")
dat1$date_repealed <- format(dat1$date_repealed,   "%Y%m%d")

#listing acts with 0 words as missing. It's just that the acts are only available in word

dat1$word_count[dat1$word_count==0]<-NA


write_csv(dat1, "outputs/s_file_4.csv")
