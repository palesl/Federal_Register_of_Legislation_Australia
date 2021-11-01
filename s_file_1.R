## Scraping the Federal Register of Legislation
## Patrick Leslie
## 29th October 2021

# rm(list=ls())

## Packages

library(rvest); library(xml2); library(stringr); library(tidyverse); 
library(RSelenium);library(curl);library(httr)

## Principal Acts 





portfolio<- gsub(" ", "%20", portfolio)


title<-c("Ab", "Ac", "Ad", "Ae", "Af", "Ag", "Ai", "Al", "Am", "An", "Ap", "Ar", "As", "At", "Au", "Av",
         "Ba", "Be", "Bi", "Bl", "Bo", "Br", "Bu",
         "Ca", "Ce", "Cf", "Ch", "Ci", "Cl", "Co", "Cr", "Cs", "Cu",
         "Da", "De", "Di", "Do", "Dr",
         "Ea", "Ec", "Ed", "Eg", "El", "Em", "En", "Ep", "Eq", "Es", "Eu", "Ev", "Ex",
         "Fa", "Fe", "Fi", "Fl", "Fo", "Fr", "Fu",
         "G2", "Ga", "Ge", "Gi", "Gl", "Go", "Gr", "Gu",
         "Ha", "He", "Hi", "Ho", "Hu",
         "Il", "Im", "In", "Ir",
         "Ja", "Je", "Ju", 
         "Ka", "Ke", "Ki", "Ko",
         "La", "Le", "Li", "Lo",
         "Ma", "Me", "Mi", "Mo", "Mu",
         "Na", "Ne", "Ni", "No", "Nr", "Nu",
         "Oc", "Of", "Oi", "Ol", "Om", "On", "Oo", "Or", "Ov", "Oz",
         "Pa", "Pe", "Ph", "Pi", "Pl", "Po", "Pr", "Ps", "Pu", "Py",
         "Qa", "Qu",
         "Ra", "Re", "Ri", "Ro", "Ru",
         "Sa", "Sc", "Se", "Sh", "Si", "Sk", "Sm", "Sn", "So", "Sp", "St", "Su", "Sw", "Sy",
         "Ta", "Te", "Th", "To", "Tr", "Tu", "Ty",
         "Un", "Ur",
         "Ve", "Vi", "Vo",
         "Wa", "We", "Wh", "Wi", "Wo",
         "Ye",
         "Zo")

by_title_year<-list()





for(i in 2008:2021){
  yearly<- list()
  for(j in 1:length(title)){
    
    print(paste0(i," ",title[j], " (", j, ")"))
    
    url <-   paste0("https://www.legislation.gov.au/Browse/Results/ByTitle/Acts/Asmade/", title[j], "/0/",i,"/principal")
    
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
  ggtitle("Principal Acts of the Australian Parliament per year, 1901-2021")+
  scale_y_continuous(breaks = seq(0,120,10))+
  theme_minimal()+ylab("")+xlab("")+
  scale_x_continuous(breaks = seq(1900,2020,10))

ggsave("outputs/s_file_1_graph.png", dpi=500)

write_csv(dat1, "outputs/s_file_1.csv")



# portfolio<- c("Agriculture","Agriculture%20and%20Water%20Resources",
#               "Agriculture,%20Fisheries%20and%20Forestry","Agriculture,%20Water%20and%20the%20Environment",
#               "Attorney-General's","Broadband,%20Communications%20and%20the%20Digital%20Economy",
#               "Climate%20Change%20and%20Energy%20Efficiency","Communications",
#               "Communications%20and%20the%20Arts","DCITA",
#               "DOEH","Defence",
#               "DOTARS","DEST",
#               "DEWR","DITR",
#               "Education","Education%20and%20Training",
#               "Education,%20Employment%20and%20Workplace%20Relations","Education,%20Skills%20and%20Employment",
#               "Employment","Environment",
#               "Environment%20and%20Energy","Environment,%20Water,%20Heritage%20and%20the%20Arts",
#               "FaCSIA","Finance",
#               "Foreign%20Affairs%20and%20Trade","Families,%20Housing,%20Community%20Services%20and%20Indigenous%20Affairs",
#               "Health","Home%20Affairs",
#               "Immigration and Border Protection","Industry",
#               "Immigration and Citizenship","Industry and Science",
#               "Industry,%20Innovation%20and%20Science","Industry, Innovation, Science, Research and Tertiary Education",
#               "Industry, Innovation and Science (part of the Jobs and Innovation portfolio)","Industry, Science, Energy and Resources",
#               "Innovation, Industry, Science and Research","Infrastructure and Regional Development",
#               "Infrastructure, Transport, Regional Development and Communications","Infrastructure%20and%20Transport",
#               "Infrastructure, Transport, Cities and Regional Development","Prime Minister and Cabinet",
#               "Regional Australia, Local Government, Arts and Sport","Resources, Energy and Tourism",
#               "Regional Australia, Regional Development and Local Government","Sustainability, Environment, Water, Population and Communities",
#               "Social Services","Treasury", "Veterans' Affairs")