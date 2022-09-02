## Amending acts of the Australian Parliament: Analysis  
## a_file_3: graphing time to first amendment over time.
## Patrick Leslie
### similar to a_file_2.R now with administering departments

#libraries
library(tidyverse)


#time to repeal
principal <- read_csv("outputs/secure outputs/s_file_8_principal_acts_departments.csv")

principal<-principal[!grepl("Amendment Act",principal$name),]

repeals<-principal%>%select(id_principal,date_enacted,date_repealed, repealing_act_id, dept_1)%>%
  filter(!is.na(date_repealed)) 

names(repeals)[5]<-"administered_by"

names(repeals)[3]<-"date_event"
names(repeals)[4]<-"event_act_id"

repeals$date_enacted<-as.Date(as.character(repeals$date_enacted), format= "%Y%m%d")
repeals$date_event<-as.Date(as.character(repeals$date_event), format= "%Y%m%d")



repeals$years_to_event<-(as.numeric(repeals$date_event)-
                          as.numeric(repeals$date_enacted))/365.25

repeals$event_type<- "repeal"



#time to first amendment...
principal <- read_csv("outputs/secure outputs/s_file_8_principal_acts_departments.csv")
principal<-principal[!grepl("Amendment Act",principal$name),]



amending <- read_csv("outputs/secure outputs/s_file_5_amendments_full.csv")



amendments<-principal%>%left_join(amending,by="id_principal")%>%filter(!is.na(id_amending))%>%
  group_by(id_principal)%>%
  summarise(date_enacted = min(date_enacted.x),
            first_amendment_date = min(date_enacted.y), 
            event_act_id = id_amending,
            amend_date = date_enacted.y,
            administered_by =dept_1[1])%>%
  filter(first_amendment_date==amend_date)%>%
  select(-amend_date)

amendments$date_enacted<-as.Date(as.character(amendments$date_enacted), format= "%Y%m%d")
amendments$first_amendment_date<-as.Date(as.character(amendments$first_amendment_date), format= "%Y%m%d")


amendments$years_to_event<-(as.numeric(amendments$first_amendment_date)-
                                    as.numeric(amendments$date_enacted))/365.25
                                  
names(amendments)[3]<-"date_event"

amendments$event_type <- "first amendment"                            
                                  
                                  
#graphing

joined<-bind_rows(repeals,amendments)

View(joined[joined$years_to_event<=0,])


ggplot(joined%>%filter(event_type=='first amendment'), 
       aes(date_event,years_to_event,colour=administered_by ))+
    geom_smooth(method = "lm")


## coming up with a concordance for policy area...

departments<-as.data.frame(principal$dept_1%>%unique())
names(departments)<-"department"
departments$CAP_policy_area<-NA
write_excel_csv(departments,"outputs/a_file_3_departments_to_CAP.csv")

#graphing amendments before end of parliamentary term

amendments<-principal%>%left_join(amending,by="id_principal")%>%filter(!is.na(id_amending))%>%
  group_by(id_principal)%>%
  summarise(date_enacted = min(date_enacted.x),
            first_amendment_date = min(date_enacted.y), 
            event_act_id = id_amending,
            amend_date = date_enacted.y)%>%
  filter(first_amendment_date==amend_date)%>%
  select(-amend_date)

non_amended<-principal%>%left_join(amending,by="id_principal")%>%filter(is.na(id_amending))

non_amended<-non_amended[,c(1,3)]
names(non_amended)[2]<-'date_enacted'

principal_acts_plus_event<-bind_rows(non_amended, amendments)

principal_acts_plus_event<-principal_acts_plus_event%>%left_join(principal%>%
                                                                   select("id_principal", "date_repealed"))%>%
  distinct()%>%filter(!is.na(date_enacted))



parl_dates <- read_csv("inputs/parl_dates.csv")[,1:5]

parl_dates$dissolution_date[46]<- 20220101### this will change!!!!



parl_dates$founding_date<- as.Date(as.character(parl_dates$founding_date), format= "%Y%m%d")
parl_dates$dissolution_date<- as.Date(as.character(parl_dates$dissolution_date), format= "%Y%m%d")
principal_acts_plus_event$date_enacted<- as.Date(as.character(principal_acts_plus_event$date_enacted), format= "%Y%m%d")
principal_acts_plus_event$first_amendment_date<- as.Date(as.character(principal_acts_plus_event$first_amendment_date), format= "%Y%m%d")
principal_acts_plus_event$date_repealed<- as.Date(as.character(principal_acts_plus_event$date_repealed), format= "%Y%m%d")

for(i in 1:nrow(principal_acts_plus_event)){
  print(principal_acts_plus_event$date_enacted[i])
  for(j in 1:nrow(parl_dates)){
    if(parl_dates$founding_date[j]<=principal_acts_plus_event$date_enacted[i]&
       parl_dates$dissolution_date[j]>=principal_acts_plus_event$date_enacted[i]){
      principal_acts_plus_event$parl_enacted[i]<- parl_dates$name[j]
      
    }
    
   
  }
}

principal_acts_plus_event$parl_repealed <-NA

for(i in 1:nrow(principal_acts_plus_event)){
  print(principal_acts_plus_event$date_enacted[i])
  for(j in 1:nrow(parl_dates)){
    

    if(!is.na(principal_acts_plus_event$date_repealed[i])){
      if(parl_dates$founding_date[j]<=principal_acts_plus_event$date_repealed[i]&
         parl_dates$dissolution_date[j]>=principal_acts_plus_event$date_repealed[i]){
      principal_acts_plus_event$parl_repealed[i]<- parl_dates$name[j]
      
    }}
  }
}

principal_acts_plus_event$parl_amended <-NA

for(i in 1:nrow(principal_acts_plus_event)){
  print(principal_acts_plus_event$date_enacted[i])
  for(j in 1:nrow(parl_dates)){
    
    
    if(!is.na(principal_acts_plus_event$first_amendment_date[i])){
      if(parl_dates$founding_date[j]<=principal_acts_plus_event$first_amendment_date[i]&
         parl_dates$dissolution_date[j]>=principal_acts_plus_event$first_amendment_date[i]){
      principal_acts_plus_event$parl_amended[i]<- parl_dates$name[j]
      
    }}
    
  }
}

principal_acts_plus_event$amended_same_term<-0
principal_acts_plus_event$amended_same_term[principal_acts_plus_event$parl_enacted==
                                              principal_acts_plus_event$parl_amended] <-1

principal_acts_plus_event$repealed_same_term<-0
principal_acts_plus_event$repealed_same_term[principal_acts_plus_event$parl_enacted==
                                              principal_acts_plus_event$parl_repealed] <-1


principal_acts_plus_event$altered_same_term<-0
principal_acts_plus_event$altered_same_term[principal_acts_plus_event$parl_enacted==
                                               principal_acts_plus_event$parl_repealed] <-1
principal_acts_plus_event$altered_same_term[principal_acts_plus_event$parl_enacted==
                                              principal_acts_plus_event$parl_amended] <-1

View(principal_acts_plus_event)

write.csv(principal_acts_plus_event, "outputs/a_file_2_acts_plus_event.csv", row.names = F)

parliament_sum<-principal_acts_plus_event%>%group_by(parl_enacted)%>%
  summarise(amended_same_term=mean(amended_same_term),
            repealed_same_term=mean(repealed_same_term),
            altered_same_term=mean(altered_same_term))%>%
  left_join(parl_dates%>%select(election_date,name), by=c('parl_enacted'='name'))
parliament_sum$election_date<-as.Date(parliament_sum$election_date, format = "%d.%m.%Y")

parliament_sum<-parliament_sum%>%
  arrange(election_date)

parliament_sum<-parliament_sum[,c(1,5,2:4)]

parliament_sum[,3:5]<-parliament_sum[,3:5]*100

ggplot(parliament_sum, aes(election_date, amended_same_term))+
  geom_step() + ylab("Amended same term as enacted (%)") + xlab('')+
  scale_y_continuous(limits = c(0,50)) + theme_minimal()+ 
  scale_x_date(date_breaks="5 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(vjust = 0.5))

ggsave("outputs/a_file_2_graph_1_amended_same_term.png", width=6, height=4, dpi=500)

ggplot(parliament_sum, aes(election_date, repealed_same_term))+
  geom_step() + ylab("Repealed same term as enacted (%)") + xlab('')+
  scale_y_continuous(limits = c(0,50)) + theme_minimal()+ 
  scale_x_date(date_breaks="5 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(vjust = 0.5))

ggsave("outputs/a_file_2_graph_2_repealed_same_term.png" , width=6, height=4, dpi=500 )


ggplot(parliament_sum, aes(election_date, altered_same_term))+
  geom_step() + ylab("Amended or Repealed \nsame term as enacted (%)") + xlab('')+
  scale_y_continuous(limits = c(0,50)) + theme_minimal() + 
  scale_x_date(date_breaks="5 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(vjust = 0.5))

ggsave("outputs/a_file_2_graph_3_altered_same_term.png" , width=6, height=4, dpi=500)



# accounting for the scope of amendments by weighting the amending act. 


amend_weights<- amending %>% group_by(id_amending)%>%
  summarise(amend_weight = 1/n())

amend_weights <- amend_weights %>% left_join(amending_acts %>% select(id_amending,name))

hist(amend_weights$amend_weight)

write.csv(amend_weights, "outputs/amendind_acts_weights.csv", row.names = F)

names(amend_weights)[1]<-"event_act_id"


principal_acts_plus_event<-principal_acts_plus_event%>%left_join( amend_weights )
principal_acts_plus_event$amend_weight[is.na(principal_acts_plus_event$amend_weight)]<- 1

parliament_sum_weighted<-principal_acts_plus_event%>%group_by(parl_enacted)%>%
  summarise(wt_amended_same_term=mean(amended_same_term*amend_weight),
            wt_one_amended_same_term = mean(amended_same_term==amend_weight),
            n=n())%>%
  left_join(parl_dates%>%select(election_date,name), by=c('parl_enacted'='name'))

parliament_sum_weighted$election_date<-as.Date(parliament_sum_weighted$election_date, format = "%d.%m.%Y")

parliament_sum_weighted<-parliament_sum_weighted%>%
  arrange(election_date)


parliament_sum_weighted$wt_amended_same_term <-parliament_sum_weighted$wt_amended_same_term*100
parliament_sum_weighted$wt_one_amended_same_term <- parliament_sum_weighted$wt_one_amended_same_term*100

ggplot(parliament_sum_weighted, aes(election_date, wt_amended_same_term))+
  geom_smooth(se=F, col='grey')+
  geom_step() + ylab("Pct amended same term as enacted (weighted)") + xlab('')+
  scale_y_continuous(limits = c(0,35)) + theme_minimal()+ 
  scale_x_date(date_breaks="5 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(vjust = 0.5))



ggplot(parliament_sum_weighted, aes(election_date, wt_one_amended_same_term))+
  geom_smooth(se=F, col='grey')+
  geom_step() + ylab("Pct amended same term as enacted (weighted)") + xlab('')+
  scale_y_continuous(limits = c(0,35)) + theme_minimal()+ 
  scale_x_date(date_breaks="5 year", date_labels = "%Y")+
  theme(axis.text.x = element_text(angle = 90))+
  theme(axis.text.x = element_text(vjust = 0.5))


