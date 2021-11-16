## Scraping the Federal Register of Legislation
## s_file_5: Details of Amending Acts 1901-2021 -- completion using RSelenium
## Patrick Leslie
## November 2021

# rm(list=ls())

## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse) 
library(RSelenium);library(curl);library(httr); library(progress)
library(doParallel); library(foreach);library(data.table);library(doSNOW)


# load in initial data...

d_data <- read_csv("outputs/s_file_4_manual_complete.csv")


# functions for the loop...

try_click_combo <- function(){
  out<-tryCatch(
    {
      remote_driver$findElement(using="name", value="ctl00$MainContent$SeriesAmendments$RadGrid1$ctl00$ctl02$ctl00$PageSizeComboBox")$clickElement()
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


try_click_50 <- function(){
  out<-tryCatch(
    {
      remote_driver$findElement(using = "xpath", "/html/body/form/div[1]/div/div/ul/li[3]")$clickElement()
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


# attempting to set up RSelenium


rD <- rsDriver(browser="firefox", port=400L, verbose=F)
remote_driver <- rD[["client"]]

out_list<-list()

for(i in 1:nrow(d_data)){
   
  url <-   paste0("https://www.legislation.gov.au/Details/",d_data$id_amending[i],"/Amends")
  
  remote_driver$navigate(url)
  
  try_click_combo()

  try_click_50()
  
  Sys.sleep(0.3) # give the page time to fully load
  act <- remote_driver$getPageSource()[[1]]
  
  principal_acts <- read_html(act) %>%
    html_nodes("body")%>%
    xml2::xml_find_all("//tr[contains(@id, 'ctl00_MainContent_SeriesAmendments_RadGrid1_ctl00')]") %>%
    xml2::xml_find_all("//td")%>%html_text()%>%
    str_extract_all("C[0-9]+A[0-9]+")%>%unlist()%>%
    unique()
  
  principal_acts[is.null(principal_acts)]<-NA
  
  # binding together
  dat<-list()
  dat[[1]]<- bind_cols(id_amending= d_data$id_amending[i],
                       id_principal = principal_acts,
                       name = d_data$name[i],
                       date_enacted = d_data$date_enacted[i],
                       year_enacted = d_data$year_enacted[i],
                       administered_by=d_data$administered_by[i],
                       date_repealed=d_data$date_repealed[i],
                       repealing_act_id=d_data$repealing_act_id[i],
                       word_count=d_data$word_count[i])
  
  #Now for next pages...
  
  new_pages<-read_html(act) %>%
    html_nodes("body")%>%
    xml_find_all("/html/body/form/div[3]/div/div[3]/section/section/div/div[2]/div/div/div/div/div/div[2]/div/table/tbody/tr[2]/td/div/table/thead/tr[1]/td/table/tbody/tr/td/div[2]")%>%
    html_children()%>%
    length()
  
  if(new_pages>1){
    for(j in 2:new_pages){
      
      remote_driver$findElement(using = "xpath", "/html/body/form/div[3]/div/div[3]/section/section/div/div[2]/div/div/div/div/div/div[2]/div/table/tbody/tr[2]/td/div/table/thead/tr[1]/td/table/tbody/tr/td/div[3]/input[1]")$clickElement()
      Sys.sleep(0.3) # give the page time to fully load
      act <- remote_driver$getPageSource()[[1]]
      
      principal_acts <- read_html(act) %>%
        html_nodes("body")%>%
        xml2::xml_find_all("//tr[contains(@id, 'ctl00_MainContent_SeriesAmendments_RadGrid1_ctl00')]") %>%
        xml2::xml_find_all("//td")%>%html_text()%>%
        str_extract_all("C[0-9]+A[0-9]+")%>%unlist()%>%
        unique()
      
      principal_acts[is.null(principal_acts)]<-NA
      dat[[j]]<-bind_cols(id_amending= d_data$id_amending[i],
                          id_principal = principal_acts,
                          name = d_data$name[i],
                          date_enacted = d_data$date_enacted[i],
                          year_enacted = d_data$year_enacted[i],
                          administered_by=d_data$administered_by[i],
                          date_repealed=d_data$date_repealed[i],
                          repealing_act_id=d_data$repealing_act_id[i],
                          word_count=d_data$word_count[i])
      
    }
  }
  
  
  dat<-bind_rows(dat)
  
  out_list[[i]]<-dat
}


rm(rD)

data<- bind_rows(out_list)

s_file_4_amending_acts_details <- read_csv("outputs/secure outputs/s_file_4_amending_acts_details.csv")


full_data<- data%>%bind_rows(s_file_4_amending_acts_details)%>%distinct()

write_csv(full_data, "outputs/s_file_5.csv")


