## s_file_8.R: Assigning principal acts to department of administration.

## Patrick Leslie
## May 2022

# rm(list=ls())

# loading in packages...

library(readr); library(readxl);library(stringr); library(tidyverse); library(readtext)

# Load in base files

acts <- read_csv("outputs/secure outputs/s_file_3_principal_acts_details.csv")


depts <- read_excel("inputs/administrative_arrangement_orders_raw/aao_departments.xlsx")

get_aao <- function(dept, x){
  df<- dept
  vec <- df[,x] 
  vec <-  vec %>% filter(!is.na(vec[,1]))
  return(vec) 
}

#aao<-get_aao(dept=depts, x=2)

get_acts <- function(time,acts, date_enacted){
  time <- gsub("AAO_","",names(aao)[1]) %>% as.Date()
  dates<- as.Date(as.character(date_enacted),format="%Y%m%d")
  acts <- acts[dates<=time,] %>% filter(!is.na(name))
  return(acts)
}

#acts<-get_acts(time=aao,acts=acts, date_enacted = acts$date_enacted)

get_aao_text <- function(aao){
  file<-paste0("inputs/admin_orders_analysis/",names(aao)[1],".txt")
  text<-readtext::readtext(file, encoding = "utf-8")$text
  text<-gsub("\\s+", " ", str_trim(text))
  
  for (i in 1:nrow(aao)){
    text<- gsub(as.character(aao[i,]),"%<%<%",text)
  }
  text_split<-unlist(str_split(text, "%<%<%"))[-1]
  return(text_split)
}

#text<- get_aao_text(aao)

assign_acts <-function(aao,acts,text){
  for(i in 1:nrow(acts)){
    act_name <- gsub('[0-9]+', '', acts$name[i]) %>% trimws()
    if(any(grepl(act_name, text))){
      departments_admin<- aao[grepl(act_name, text),]%>% pull()
      acts$administered_by[i]<- paste0(departments_admin, collapse = "; ")
    }
    if(grepl("supply", act_name, ignore.case = T)){
      acts$administered_by[i]<- "THE DEPARTMENT OF THE TREASURY"
    }
    
    if(grepl("appropriation", act_name, ignore.case = T)){
      acts$administered_by[i]<- "THE DEPARTMENT OF THE TREASURY"
    }
  }
  return(acts)
}
 
#assigned <- assign_acts(aao,acts,text)


# loop
act_loop<-acts

for(i in 1:18){
  aao<-get_aao(dept=depts, x=i)
  print(names(aao)[1])
  text<- get_aao_text(aao)
  act_loop<- assign_acts(aao=aao,acts=act_loop,text=text)
}

table(!is.na(act_loop$administered_by))


# simplifying department names...

dept_names<- data.frame(administered_by=unique(act_loop$administered_by))
dept_names$departments<- tolower(dept_names$administered_by)

dept_names$departments<- gsub("the department of the ", "", dept_names$departments)
dept_names$departments<- gsub("the department of ", "", dept_names$departments)
dept_names$departments<- gsub("the attorney general's department", "attorney general", dept_names$departments)
dept_names$departments<- gsub("the attorney-general's department", "attorney general", dept_names$departments)
dept_names$departments<- gsub("d of ", "", dept_names$departments)
dept_names$departments<- gsub("d. of ", "", dept_names$departments)
dept_names$departments<- gsub("attorney-general's", "attorney general", dept_names$departments)
dept_names$departments<- gsub("department of", "", dept_names$departments)
dept_names$departments<- gsub("pm&c", "prime minister and cabinet", dept_names$departments)
dept_names$departments<- gsub("the prime minister’s department", "prime minister and cabinet", dept_names$departments)
dept_names$departments<- gsub("the prime minister’s department", "prime minister and cabinet", dept_names$departments)
dept_names$departments<- gsub("minister of state for ", "", dept_names$departments)
dept_names$departments<- gsub("the postmaster-general’s department", "postmaster general", dept_names$departments)
dept_names$departments<- gsub(" department", "", dept_names$departments)
dept_names$departments<- gsub("the attorney­ general's", "attorney general", dept_names$departments)
dept_names$departments<- gsub("the treasury", "treasury", dept_names$departments)
dept_names$departments<- gsub("the attorney-general’s", "attorney general", dept_names$departments)
dept_names$departments<- gsub(".", "", dept_names$departments, fixed = T)
dept_names$departments<- gsub("veterans'", "veteran", dept_names$departments)
dept_names$departments<- gsub("facsia", "families, housing, community services and indigenous affairs", dept_names$departments)
dept_names$departments<- gsub("families, community services and indigenous affairs", 
                         "families, housing, community services and indigenous affairs", dept_names$departments)
dept_names$departments<- gsub("the repatriation", "repatriation", dept_names$departments, fixed = T)
dept_names$departments<- gsub(" (part of the jobs and innovation portfolio)", "", dept_names$departments, fixed = T)
dept_names$departments<- gsub("dcita", "communications, information technology and the arts", dept_names$departments, fixed = T)
dept_names$departments<- gsub("dest", "education, science and training", dept_names$departments, fixed = T)
dept_names$departments<- gsub("ditr", "industry, tourism and resources", dept_names$departments, fixed = T)

dept_names$departments<- trimws(dept_names$departments)
unique(dept_names$departments)


name_split_list<-list()
for(i in 1:nrow(dept_names)){
  vec<- str_split(dept_names$departments[i],";\\s+")
  df<- t(data.frame(vec))
  row.names(df) <- '1'
  df<-data.frame(df)
  names(df) <- paste0('dept_', seq(1:ncol(df)))
  name_split_list[[i]]<-df
}

unique(unlist(name_split_list))%>%sort()

dept_names <-bind_cols(dept_names,bind_rows(name_split_list))



act_loop<- act_loop %>% left_join(dept_names) %>% select(-administered_by)

write_csv(act_loop, "outputs/s_file_8_principal_acts_departments.csv")
