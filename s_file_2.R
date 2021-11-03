## Scraping the Federal Register of Legislation
## s_file_2: Amending Acts 1901-2021
## Patrick Leslie
## November 2021

# rm(list=ls())

## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse); 
library(RSelenium);library(curl);library(httr)

## Amending Acts 

title<-c("Ab","Ac","Ad","Af","Ag","Ai","Al","Am","An","Ap","Ar","As","At","Au","Av",
"Ba","Be","Bi","Bo","Br","Bu",
"Ca","Ce","Ch","Ci","Cl","Co","Cr","Cu","Cy",
"Da","De","Di","Do","Dr",
"Ec","Ed","Eg","El","Em","En","Eq","Es","Eu","Ev","Ex",
"Fa","Fe","Fi","Fl","Fo","Fr","Fu",
"Ge","Gi","Gl","Go","Gr",
"Ha","He","Hi","Ho","Hu",
"Ic","Im","In","Ir",
"Ju",
"Ka",
"La","Le","Li","Lo",
"Ma","Me","Mi","Mo","Mu","My",
"Na","Ne","Ni","No","Nr","Nu",
"Oc","Of","Oh","Oi","Ol","Om","Or","Ot","Ov","Oz",
"Pa","Pe","Ph","Pi","Pl","Po","Pr","Pu","Py",
"Qa","Qu",
"Ra","Re","Ri","Ro","Ru",
"Sa","Sc","Se","Sh","Sk","Sn","So","Sp","St","Su","Sy",
"Ta","Te","Th","Ti","To","Tr","Tu",
"Un","Ur","Us",
"Ve","Vi","Vo",
"Wa","We","Wh","Wi","Wo",
"Yo")


by_title_year<-list()





for(i in 1901:2021){
  yearly<- list()
  for(j in 1:length(title)){
    
    print(paste0(i," ",title[j], " (", j, ")"))
    
    url <-   paste0("https://www.legislation.gov.au/Browse/Results/ByTitle/Acts/Asmade/", title[j], "/0/",i,"/Amending")
    
    year_leg  <- url%>% GET(.,timeout(30))%>%read_html()
    
    id_principal <- year_leg %>%
      html_nodes("body")%>%
      xml2::xml_find_all("//span[contains(@id, 'lblComlawId')]") %>%
      html_text()
    
    name <- year_leg %>%
      html_nodes("body")%>%
      xml2::xml_find_all("//a[contains(@id, 'hl')]") %>%
      html_text()
    
    date_enacted <- year_leg %>%
      html_nodes("body")%>%
      xml2::xml_find_all("//span[contains(@id, 'Label1')]") %>%
      html_text()
    
    
    # binding together
    dat<- bind_cols(id_principal=id_principal,
                    name=name,
                    date_enacted=date_enacted
    )
    
    yearly[[j]]<- dat
  }
  by_title_year[[i-1900]]<-bind_rows(yearly[sapply(yearly, nrow)>0])
  
}



dat1 <- bind_rows(by_title_year)



dat1$year_enacted<- as.numeric(stringr::str_sub(gsub("• Assented To: ", "", dat1$date_enacted),7,10))

dat1$date_enacted<- as.Date(gsub("• Assented To: ", "", dat1$date_enacted), format = "%d/%m/%Y")


plot<-dat1%>%group_by(year_enacted)%>%
  summarise(n_leg=n())

ggplot(plot, aes(year_enacted, n_leg))+geom_line()+ 
  ggtitle("Amending Acts of the Australian Parliament per year, 1901-2021")+
  scale_y_continuous(breaks = seq(0,170,10))+
  theme_bw()+ylab("")+xlab("")+
  scale_x_continuous(breaks = seq(1900,2020,10))

names(dat1)[1]<-"id_amending"

ggsave("outputs/s_file_2_graph.png", dpi=500)

write_csv(dat1, "outputs/s_file_2.csv")

