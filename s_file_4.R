## Scraping the Federal Register of Legislation
## s_file_4: Details of Amending Acts 1901-2021
## Patrick Leslie
## November 2021

# rm(list=ls())

## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse) 
library(RSelenium);library(curl);library(httr); library(progress)
library(doParallel); library(foreach);library(data.table);library(doSNOW)
registerDoParallel(cores=64)

rD <- rsDriver(port=200L)
remDr <- rD[["client"]]

## Amending Acts 


amending_acts <- read_csv("outputs/secure outputs/s_file_2_amending_acts.csv")

amending_acts<-amending_acts%>%arrange(date_enacted)




#function to try urls until successful read (adapted from https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r)


try_url <- function(url){
  out<-tryCatch(
    {
      GET(url,timeout(60))%>%read_html()
    },
    error = function(cond){
      # message('Caught an error! See below:')
      #message(cond)
      return(NA)
    },
    warning = function(cond){
      message('Caught an warning!')
      return(NA)
    },
    finally = {
      message('All done, quitting.')
      Sys.sleep(0.5)
      httr::reset_config()
    }
  )  
  return(out)
}

iterations <- nrow(amending_acts)    

acts_details<- foreach(i=1:iterations,.verbose=T,.errorhandling="pass") %dopar% {
  
  #for(i in 1:nrow(amending_acts)){
  print(i)
  
  url <-   paste0("https://www.legislation.gov.au/Series/",amending_acts$id_amending[i])
  
  
  act<-NA
  n<-1
  while (is.na(act)&n<100) {
    act<-try_url(url)
    n<-n+1
  }
  
  
  
  
  administered_by  <- NA
  date_repealed    <- NA
  repealing_act_id <- NA
  word_count       <- NA
  principal_acts   <- NA
  
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
  
  act<-NA
  n<-1
  while (is.na(act)&n<100) {
    act<-try_url(url)
    n<-n+1
  }
  
  word_count <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//div[contains(@class, 'Section')]") %>%
    html_text() %>% str_replace_all("\\s+", " ")%>%
    str_count(" ") %>%sum()
  
  
  # Principal acts that are amended by this legislation
  url <-   paste0("https://www.legislation.gov.au/Details/",amending_acts$id_amending[i],"/Amends")
  
  #test url
  #url<- "https://www.legislation.gov.au/Details/C2007A00128/Amends"
  
  act<-NA
  n<-1
  while (is.na(act)&n<100) {
    act<-try_url(url)
    n<-n+1
  }
  
  principal_acts <- act %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//tr[contains(@id, 'ctl00_MainContent_SeriesAmendments_RadGrid1_ctl00')]") %>%
    xml2::xml_find_all("//td")%>%html_text()%>%
    str_extract_all("C[0-9]+A[0-9]+")%>%unlist()%>%
    unique()
  
  principal_acts[is.null(principal_acts)]<-NA
  
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
  #acts_details[[i]]<-dat
  return(dat)
}

# retries<-list()
# j<-1
# for (i in 1:iterations){
#   if(grepl("Error",acts_details[[i]])==T){
#     
#     retries[[j]]<-i
#     j<-j+1
#   }
# }
# 
# retries<-unlist(retries)
# 
# amending_retry<-amending_acts[retries,]

#cleaning for export 
dat1<- bind_rows(acts_details)
dat1$date_enacted  <- format(dat1$date_enacted,  "%Y%m%d")
dat1$date_repealed <- as.Date(dat1$date_repealed, format= "%d %b %Y")
dat1$date_repealed <- format(dat1$date_repealed,   "%Y%m%d")

#listing acts with 0 words as missing. It's just that the acts are only available in word

dat1$word_count[dat1$word_count==0]<-NA

dat1<-unique(dat1)

check_for_manual_update<-dat1%>%group_by(id_amending)%>%summarise(n=n())%>%arrange(-n)

check<-check_for_manual_update[check_for_manual_update$n>=10,]


check<-check%>% left_join(dat1)%>%select(-id_principal, -n)%>%distinct()

write_csv(check, "outputs/s_file_4_manual_complete.csv")


write_csv(dat1, "outputs/s_file_4.csv")



