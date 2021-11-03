## Acts of the Australian Parliament: Analysis  
## a_file_1: graphing principal and amending legislation overlaid. 
## Patrick Leslie
## November 2021

# rm(list=ls())

library(tidyverse)


principal <- read_csv("outputs/secure outputs/s_file_1_principal_acts.csv")%>%
  group_by(year_enacted)%>%
  summarise(n_leg=n())

principal$type<-"principal"


amending <- read_csv("outputs/secure outputs/s_file_2_amending_acts.csv")%>%
  group_by(year_enacted)%>%
  summarise(n_leg=n())

amending$type<-"amending"


plot<-bind_rows(principal,amending)

plot$type<-factor(plot$type, levels = c('principal', 'amending'))

ggplot(plot, aes(year_enacted, n_leg, group=type,col=type))+geom_line()+ 
  ggtitle("Acts of the Australian Parliament per year, 1901-2021")+
  scale_y_continuous(breaks = seq(0,170,10))+
  theme_bw()+ylab("")+xlab("")+
  scale_x_continuous(breaks = seq(1900,2020,10))+
  scale_color_grey(start=.1,end=.7)

ggsave("outputs/a_file_1_graph_1.png", dpi=500)

plot2<-plot%>%group_by(year_enacted)%>%
  summarise(total_acts=sum(n_leg, na.rm=F))



ggplot(plot2, aes(year_enacted, total_acts))+geom_line()+ 
  ggtitle("Acts of the Australian Parliament per year, 1901-2021")+
  scale_y_continuous(breaks = seq(0,270,10))+
  theme_bw()+ylab("")+xlab("")+
  scale_x_continuous(breaks = seq(1900,2020,10))

ggsave("outputs/a_file_1_graph_2.png", dpi=500)


plot3<-plot2%>%mutate(cumulative=cumsum(total_acts))



ggplot(plot3, aes(year_enacted, cumulative))+geom_line()+ 
  ggtitle("Acts of the Australian Parliament (cumulative), 1901-2021")+
  scale_y_continuous(breaks = seq(0,14000,1000))+
  theme_bw()+ylab("")+xlab("")+
  scale_x_continuous(breaks = seq(1900,2020,10))

ggsave("outputs/a_file_1_graph_3.png", dpi=500)
