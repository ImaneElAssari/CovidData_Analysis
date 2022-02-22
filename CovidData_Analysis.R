library(tidyverse)
library(lubridate)

data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

data_long<-data%>%
  pivot_longer(-c(1:4),names_to ="date")%>%
  mutate(date = mdy(date))

data_long%>%
  filter(`Country/Region` == "France")%>%
  group_by(`Country/Region`,date)%>%
  summarise(value=sum(value))%>%
  #Calcul de nombre de cas par jour 
  mutate(daily_death = value - lag(value))%>%
  ggplot()+
  geom_point(aes(x = date , y = daily_death))

#Calcul de nombre de cas par jour pour chaque pays 
data_long %>%
  filter(`Country/Region` %in% c("France","US","Italy","Sweden","United Kingdom"))%>%
  group_by(`Country/Region`,date)%>%
  summarise(value=sum(value))%>%
  mutate(daily_death = value - lag(value))%>%
  ggplot()+
  geom_point(aes(x = date, y = value, col = `Country/Region`))+
  theme_bw()+
  ggtitle(" Morts Cumulés Covid ")+
  ylab("Morts")
#Calcul du taux de croissance 
data_long %>%
  filter(`Country/Region` %in% c("France","US","Italy","Sweden","United Kingdom"))%>%
  group_by(`Country/Region`,date)%>%
  summarise(value=sum(value))%>%
  mutate(daily_death = value - lag(value),
         grate = 100*(value-lag(value)) /value)  %>%
  ggplot()+
  geom_point(aes(x = date, y = grate, col = `Country/Region`))+
  theme_bw()+
  ggtitle(" Taux de Croissance par pays ")+
  ylab("Taux de croissance")+
#Separation des graphes(On veut avoir 1 graphe pour chaque pays)
  facet_wrap(~`Country/Region`)
