## Scraping the Federal Register of Legislation
## s_file_3: Details of Principal Acts 1901-2021
## Patrick Leslie
## November 2021

# rm(list=ls())

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
      message('Caught an warning!')
      return(NA)
    },
    finally = {
      message('All done, quitting.')
    }
  )  
  return(out)
}

## Principal Acts 


principal_acts <- read_csv("outputs/secure outputs/s_file_1_principal_acts.csv")

principal_acts<-principal_acts%>%arrange(date_enacted)


 

acts_details<- foreach(i=1:nrow(principal_acts),.verbose=T,.errorhandling="pass",.combine=rbind) %dopar% {
  
    print(i)
    
    url <-   paste0("https://www.legislation.gov.au/Series/",principal_acts$id_principal[i])
    
    
    act<-NA
    while (is.na(act)) {
      act<-try_url(url)
    }
    
    
    administered_by<-NA
    date_repealed<-NA
    repealing_act_id<-NA
    word_count<- NA
    
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
    url <-   paste0("https://www.legislation.gov.au/Details/",principal_acts$id_principal[i])
    
    #test url
    #url<-"https://www.legislation.gov.au/Details/C2004A05138"
    
    
    act<-NA
    while (is.na(act)) {
      act<-try_url(url)
    }
    
    word_count <- act %>%
      html_nodes("body")%>%
      xml2::xml_find_all("//div[contains(@class, 'Section')]") %>%
      html_text() %>% str_replace_all("\\s+", " ")%>%
      str_count(" ") %>%sum()
    

    # binding together
    dat<-NULL
    dat<- bind_cols(id_principal= principal_acts$id_principal[i],
                    name = principal_acts$name[i],
                    date_enacted = principal_acts$date_enacted[i],
                    year_enacted = principal_acts$year_enacted[i],
                    administered_by=administered_by,
                    date_repealed=date_repealed,
                    repealing_act_id=repealing_act_id,
                    word_count=word_count)
    return(dat)
}

#cleaning for export 

dat1 <- bind_rows(acts_details,acts_details_2279_to_4754,acts_details_to_2278)
dat1<-dat1%>%arrange(date_enacted)

dat1$date_enacted  <- format(dat1$date_enacted,  "%Y%m%d")
dat1$date_repealed <- as.Date(dat1$date_repealed, format= "%d %b %Y")
dat1$date_repealed <- format(dat1$date_repealed,   "%Y%m%d")

#listing acts with 0 words as missing. It's just that the acts are only available in word

dat1$word_count[dat1$word_count==0]<-NA


write_csv(dat1, "outputs/s_file_3.csv")
