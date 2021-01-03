#Copyright 2020 Vishnu Kalyan Mylavarapu
  
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
  
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

covid<- read.csv("owid-covid-data.csv")
str(covid)
is.na(covid$new_tests)
covid$new_tests<-NULL
summary(covid)
library(visdat)

install.packages('devtools')
library(devtools)
install_github('tierneyn/visdat')

covid[,c(30:40)]
vis_miss(covid[,c(30:40)],warn_large_data = FALSE)

library(dplyr)

#the following function will be used repeatedly to remove missing data
delete.na <- function(DF, n=2) {
  DF[rowSums(is.na(DF)) <= n,]
}

#choosing only africa data

covid_Africa<-covid %>% filter(continent=="Africa")
vis_miss(covid_Africa,warn_large_data = FALSE)

#cleaning out the columns that have more than 50% missing values

covid_Africa$total_tests<-NULL
covid_Africa$total_tests_per_thousand<-NULL
covid_Africa$new_tests_per_thousand<-NULL
covid_Africa$new_tests_smoothed<-NULL
covid_Africa$new_tests_smoothed_per_thousand<-NULL
covid_Africa$tests_per_case<-NULL
covid_Africa$positive_rate<-NULL

head(covid_Africa)
vis_miss(covid_Africa)


typeof(covid_Africa$female_smokers)

mean(covid_Africa$female_smokers,na.rm = TRUE)
  
covid_Africa %>% summarise(count=sum(is.na(female_smokers)))
vis_miss(covid_Africa,warn_large_data = FALSE)  

covid_Africa$female_smokers<- ifelse(is.na(covid_Africa$female_smokers),2.5,covid_Africa$female_smokers)

vis_miss(covid_Africa,warn_large_data = FALSE)
mean(covid_Africa$male_smokers,na.rm = TRUE)
covid_Africa$male_smokers<- ifelse(is.na(covid_Africa$male_smokers),27.42,covid_Africa$male_smokers)
vis_miss(covid_Africa,warn_large_data = FALSE)

mean(covid_Africa$stringency_index,na.rm = TRUE)
covid_Africa$stringency_index<- ifelse(is.na(covid_Africa$stringency_index),61.04,covid_Africa$stringency_index)
vis_miss(covid_Africa,warn_large_data = FALSE)
mean(covid_Africa$extreme_poverty,na.rm = TRUE)

covid_Africa$extreme_poverty<- ifelse(is.na(covid_Africa$extreme_poverty),32.997,covid_Africa$extreme_poverty)
vis_miss(covid_Africa,warn_large_data = FALSE)
mean(covid_Africa$handwashing_facilities,na.rm = TRUE)
covid_Africa$handwashing_facilities<- ifelse(is.na(covid_Africa$handwashing_facilities),26.56,covid_Africa$handwashing_facilities)
vis_miss(covid_Africa,warn_large_data = FALSE)
mean(covid_Africa$hospital_beds_per_thousand,na.rm = TRUE)
covid_Africa$hospital_beds_per_thousand<- ifelse(is.na(covid_Africa$hospital_beds_per_thousand),1.511,covid_Africa$hospital_beds_per_thousand)
vis_miss(covid_Africa,warn_large_data = FALSE)
mean(covid_Africa$human_development_index,na.rm = TRUE)
covid_Africa$human_development_index<- ifelse(is.na(covid_Africa$human_development_index),0.54,covid_Africa$human_development_index)



#column type reorganization


library(lubridate)
library(ggplot2)
covid_Africa$date<- mdy(covid_Africa$date)
max(covid_Africa$date)
min(covid_Africa$date)


covid_Africa %>%ggplot(aes(x=date,y=new_cases_per_million,color=iso_code))+geom_line()+theme_bw()


education<-covid_Africa %>% group_by(location,hdi=human_development_index) %>% summarise(death_rate=mean(new_deaths))

education %>% ggplot(aes(x=hdi,y=death_rate,color=location))+geom_point()+ylim(0,30)


poorvsrich<- covid_Africa %>% group_by(location,poverty=extreme_poverty,gdp=gdp_per_capita)%>% summarise(death_rate=mean(new_deaths))

poorvsrich %>% ggplot(aes(x=gdp,y=death_rate,color=location))+geom_point()+ylim(0,30)

poorvsrich %>% ggplot(aes(x=poverty,y=death_rate,color=location))+geom_point()+ylim(0,10)



#choosing only Europe data

covid_Europe<-covid %>% filter(continent=="Europe")
vis_miss(covid_Europe,warn_large_data = FALSE)

covid_Europe$total_tests<-NULL
covid_Europe$total_tests_per_thousand<-NULL
covid_Europe$new_tests_per_thousand<-NULL
covid_Europe$new_tests_smoothed<-NULL
covid_Europe$new_tests_smoothed_per_thousand<-NULL
covid_Europe$tests_per_case<-NULL
covid_Europe$positive_rate<-NULL

head(covid_Europe)
vis_miss(covid_Europe)

typeof(covid_Europe$female_smokers)
mean(covid_Europe$female_smokers,na.rm = TRUE)
covid_Europe %>% summarise(count=sum(is.na(female_smokers)))
vis_miss(covid_Europe,warn_large_data = FALSE)  

covid_Europe$female_smokers<- ifelse(is.na(covid_Europe$female_smokers),2.5,covid_Europe$female_smokers)

vis_miss(covid_Europe,warn_large_data = FALSE)
mean(covid_Europe$male_smokers,na.rm = TRUE)
covid_Europe$male_smokers<- ifelse(is.na(covid_Europe$male_smokers),27.42,covid_Europe$male_smokers)
vis_miss(covid_Europe,warn_large_data = FALSE)

mean(covid_Europe$stringency_index,na.rm = TRUE)
covid_Europe$stringency_index<- ifelse(is.na(covid_Europe$stringency_index),61.04,covid_Europe$stringency_index)
vis_miss(covid_Europe,warn_large_data = FALSE)
mean(covid_Europe$extreme_poverty,na.rm = TRUE)


covid_Europe$extreme_poverty<- ifelse(is.na(covid_Europe$extreme_poverty),32.997,covid_Europe$extreme_poverty)
vis_miss(covid_Europe,warn_large_data = FALSE)
mean(covid_Europe$handwashing_facilities,na.rm = TRUE)
covid_Europe$handwashing_facilities<- ifelse(is.na(covid_Europe$handwashing_facilities),26.56,covid_Europe$handwashing_facilities)
vis_miss(covid_Europe,warn_large_data = FALSE)
mean(covid_Europe$hospital_beds_per_thousand,na.rm = TRUE)
covid_Europe$hospital_beds_per_thousand<- ifelse(is.na(covid_Europe$hospital_beds_per_thousand),1.511,covid_Europe$hospital_beds_per_thousand)
vis_miss(covid_Europe,warn_large_data = FALSE)
mean(covid_Europe$human_development_index,na.rm = TRUE)
covid_Europe$human_development_index<- ifelse(is.na(covid_Europe$human_development_index),0.54,covid_Europe$human_development_index)


#column type reorganization


library(lubridate)

covid_Europe$date<- mdy(covid_Europe$date)
max(covid_Europe$date)
min(covid_Europe$date)


covid_Europe %>%ggplot(aes(x=date,y=new_cases_per_million,color=iso_code))+geom_line()+theme_bw()


education<-covid_Europe %>% group_by(location,hdi=human_development_index) %>% summarise(death_rate=mean(new_deaths))

education %>% ggplot(aes(x=hdi,y=death_rate,color=location))+geom_point()+ylim(0,30)


poorvsrich<- covid_Europe %>% group_by(location,poverty=extreme_poverty,gdp=gdp_per_capita)%>% summarise(death_rate=mean(new_deaths))

poorvsrich %>% ggplot(aes(x=gdp,y=death_rate,color=location,size=))+geom_point()+ylim(0,30)

poorvsrich %>% ggplot(aes(x=poverty,y=death_rate,color=location))+geom_point()+ylim(0,10)
library(ggplot2)


###Asia data####
covid_Asia<-covid %>% filter(continent=="Asia")
vis_miss(covid_Asia,warn_large_data = FALSE)




covid_Asia$total_tests<-NULL
covid_Asia$total_tests_per_thousand<-NULL
covid_Asia$new_tests_per_thousand<-NULL
covid_Asia$new_tests_smoothed<-NULL
covid_Asia$new_tests_smoothed_per_thousand<-NULL
covid_Asia$tests_per_case<-NULL
covid_Asia$positive_rate<-NULL



vis_miss(covid_Asia,warn_large_data = FALSE)


covid_Asia$stringency_index<- ifelse(is.na(covid_Asia$stringency_index),mean(covid_Asia$stringency_index,na.rm = TRUE),covid_Asia$stringency_index)
covid_Asia$extreme_poverty<- ifelse(is.na(covid_Asia$extreme_poverty),mean(covid_Asia$extreme_poverty,na.rm = TRUE),covid_Asia$extreme_poverty)
covid_Asia$handwashing_facilities<- ifelse(is.na(covid_Asia$handwashing_facilities),mean(covid_Asia$handwashing_facilities,na.rm = TRUE),covid_Asia$handwashing_facilities)
covid_Asia$total_deaths<- ifelse(is.na(covid_Asia$total_deaths),mean(covid_Asia$total_deaths,na.rm = TRUE),covid_Asia$total_deaths)
covid_Asia$total_cases_per_million<- ifelse(is.na(covid_Asia$total_cases_per_million),mean(covid_Asia$total_cases_per_million,na.rm = TRUE),covid_Asia$total_cases_per_million)
covid_Asia$total_deaths_per_million<- ifelse(is.na(covid_Asia$total_deaths_per_million),mean(covid_Asia$total_deaths_per_million,na.rm = TRUE),covid_Asia$total_deaths_per_million)
covid_Asia$female_smokers<- ifelse(is.na(covid_Asia$female_smokers),mean(covid_Asia$female_smokers,na.rm = TRUE),covid_Asia$female_smokers)
covid_Asia$male_smokers<- ifelse(is.na(covid_Asia$male_smokers),mean(covid_Asia$male_smokers,na.rm = TRUE),covid_Asia$male_smokers)
covid_Asia$gdp_per_capita<- ifelse(is.na(covid_Asia$gdp_per_capita),mean(covid_Asia$gdp_per_capita,na.rm = TRUE),covid_Asia$gdp_per_capita)
covid_Asia$population_density<- ifelse(is.na(covid_Asia$population_density),mean(covid_Asia$population_density,na.rm = TRUE),covid_Asia$population_density)
covid_Asia$hospital_beds_per_thousand<- ifelse(is.na(covid_Asia$hospital_beds_per_thousand),mean(covid_Asia$hospital_beds_per_thousand,na.rm = TRUE),covid_Asia$hospital_beds_per_thousand)
covid_Asia$aged_65_older<- ifelse(is.na(covid_Asia$aged_65_older),mean(covid_Asia$aged_65_older,na.rm = TRUE),covid_Asia$aged_65_older)
covid_Asia$diabetes_prevalence<- ifelse(is.na(covid_Asia$diabetes_prevalence),mean(covid_Asia$diabetes_prevalence,na.rm = TRUE),covid_Asia$diabetes_prevalence)
covid_Asia$human_development_index<- ifelse(is.na(covid_Asia$human_development_index),mean(covid_Asia$human_development_index,na.rm = TRUE),covid_Asia$human_development_index)



covid_Asia<- delete.na(covid_Asia,n=0)

education2<-covid_Asia %>% group_by(location,hdi=human_development_index) %>% summarise(death_rate=mean(new_deaths))

education2%>% ggplot(aes(x=hdi,y=death_rate,color=location))+geom_point()+ylim(0,30)


poorvsrich2<- covid_Asia %>% group_by(location,poverty=extreme_poverty,gdp=gdp_per_capita)%>% summarise(death_rate=mean(new_deaths))

poorvsrich2 %>% ggplot(aes(x=gdp,y=death_rate,color=location))+geom_point()+ylim(0,30)

poorvsrich2 %>% ggplot(aes(x=poverty,y=death_rate,color=location))+geom_point()+ylim(0,10)



#######################

#choosing only North America data


covid_North_America<-covid %>% filter(continent=="North America")
vis_miss(covid_North_America,warn_large_data = FALSE)


#cleaning out the columns that have more than 50% missing values


covid_North_America$total_tests<-NULL
covid_North_America$total_tests_per_thousand<-NULL
covid_North_America$new_tests_per_thousand<-NULL
covid_North_America$new_tests_smoothed<-NULL
covid_North_America$new_tests_smoothed_per_thousand<-NULL
covid_North_America$tests_per_case<-NULL
covid_North_America$positive_rate<-NULL

head(covid_North_America)
vis_miss(covid_North_America)

typeof(covid_North_America$female_smokers)

mean(covid_North_America$female_smokers,na.rm = TRUE)

covid_North_America %>% summarise(count=sum(is.na(female_smokers)))
vis_miss(covid_North_America,warn_large_data = FALSE)

covid_North_America$female_smokers<- ifelse(is.na(covid_North_America$female_smokers),7.39,covid_North_America$female_smokers)

vis_miss(covid_North_America,warn_large_data = FALSE)
mean(covid_North_America$male_smokers,na.rm = TRUE)
covid_North_America$male_smokers<- ifelse(is.na(covid_North_America$male_smokers),22.09,covid_North_America$male_smokers)
vis_miss(covid_North_America,warn_large_data = FALSE)

mean(covid_North_America$stringency_index,na.rm = TRUE)
covid_North_America$stringency_index<- ifelse(is.na(covid_North_America$stringency_index),61.04,covid_North_America$stringency_index)
vis_miss(covid_North_America,warn_large_data = FALSE)

mean(covid_North_America$extreme_poverty,na.rm = TRUE)
covid_North_America$extreme_poverty<- ifelse(is.na(covid_North_America$extreme_poverty),5.2,covid_North_America$extreme_poverty)
vis_miss(covid_North_America,warn_large_data = FALSE)

mean(covid_North_America$handwashing_facilities,na.rm = TRUE)
covid_North_America$handwashing_facilities<- ifelse(is.na(covid_North_America$handwashing_facilities),77.27,covid_North_America$handwashing_facilities)
vis_miss(covid_North_America,warn_large_data = FALSE)

mean(covid_North_America$hospital_beds_per_thousand,na.rm = TRUE)
covid_North_America$hospital_beds_per_thousand<- ifelse(is.na(covid_North_America$hospital_beds_per_thousand),2.29,covid_North_America$hospital_beds_per_thousand)
vis_miss(covid_North_America,warn_large_data = FALSE)

mean(covid_North_America$human_development_index,na.rm = TRUE)
covid_North_America$human_development_index<- ifelse(is.na(covid_North_America$human_development_index),0.75,covid_North_America$human_development_index)

###column type reorganization

library(lubridate)

covid_North_America$date<- mdy(covid_North_America$date)
max(covid_North_America$date)
min(covid_North_America$date)

covid_North_America %>%ggplot(aes(x=date,y=new_cases_per_million,color=iso_code))+geom_line()+theme_bw()
education<-covid_North_America %>% group_by(location,hdi=human_development_index) %>% summarise(death_rate=mean(new_deaths))
education %>% ggplot(aes(x=hdi,y=death_rate,color=location))+geom_point()+ylim(0,30)
poorvsrich<- covid_North_America %>% group_by(location,poverty=extreme_poverty,gdp=gdp_per_capita)%>% summarise(death_rate=mean(new_deaths))
poorvsrich %>% ggplot(aes(x=gdp,y=death_rate,color=location))+geom_point()+ylim(0,30)
poorvsrich %>% ggplot(aes(x=poverty,y=death_rate,color=location))+geom_point()+ylim(0,10)


###############Covid data plotting continent wise###################


covid_asia_filter <- covid_Asia %>% group_by(location) %>% summarise(count=sum(new_cases,na.rm = TRUE))
covid_asia_filter<- head(covid_asia_filter[order(-covid_asia_filter$count),],9)
ggplot(covid_asia_filter,aes(x=location,y=count,color=location,fill=location))+geom_col()+ scale_y_continuous(name='Count',labels= scales::comma)+theme_bw()

covid_Africa_filter<-covid_Africa %>% group_by(location) %>% summarise(count=sum(new_cases,na.rm = TRUE))
covid_Africa_filter<- head(covid_Africa_filter[order(-covid_Africa_filter$count),],9)
ggplot(covid_Africa_filter,aes(x=location,y=count,color=location,fill=location))+geom_col()+scale_y_continuous(name='Count',labels= scales::comma)+theme_bw()

covid_Europe_filter<-covid_Europe %>% group_by(location) %>% summarise(count=sum(new_cases,na.rm = TRUE))
covid_Europe_filter<- head(covid_Europe_filter[order(-covid_Europe_filter$count),],9)
ggplot(covid_Europe_filter,aes(x=location,y=count,color=location,fill=location))+geom_col()+scale_y_continuous(name='Count',labels= scales::comma)+theme_bw()


covid_North_America_filter<-covid_North_America %>% group_by(location) %>% summarise(count=sum(new_cases,na.rm = TRUE))
covid_North_America_filter<- head(covid_North_America_filter[order(-covid_North_America_filter$count),],9)
ggplot(covid_North_America_filter,aes(x=location,y=count,color=location,fill=location))+geom_col()+scale_y_continuous(name='Count',labels= scales::comma)+theme_bw()

library(ggplot2)



###########  Changing date to date type for further analysis#################
library(dplyr)
typeof(covid$date)
library(lubridate)
covid$date<-mdy(covid$date)
typeof(covid$date)



#################

#Q which country is the worst affected?

covid$date
data4<-covid %>% filter(date=='2020-10-16') %>% summarise(location,total_cases,population) 
data4$total_casesper10k <- (data4$total_cases/data4$population)*100 

data4<-data4[order(-data4$total_casesper10k),]

data5<-head(data4)


ggplot(data5,aes(location,total_casesper10k,color=location,size=population))+geom_point()+theme_bw()

####worst in ratio########
compare2<-covid%>%filter(date=='2020-10-13')
compare2$comparable <- (compare2$total_cases/(compare2$population*compare2$total_tests)) 
compare2<-compare2[order(-compare2$comparable),]
compare3<-head(compare2,10)
ggplot(compare3,aes(x=location,y=comparable,color=location,size=population))+geom_point()



#################Malaria###############

# Data of malaria in 2017
malaria_2017 <- read.csv("malaria_2017.csv")
malaria_2017 <- malaria_2017 %>% filter(Year == 2017)

levels(factor(malaria_2017$WHO.Region))
malaria_africa <- malaria_2017 %>% filter(WHO.Region == "Africa")
malaria_americas <- malaria_2017 %>% filter(WHO.Region == "Americas")
malaria_mediter <- malaria_2017 %>% filter(WHO.Region == "Eastern Mediterranean")
malaria_eu <- malaria_2017 %>% filter(WHO.Region == "Europe")
malaria_SEasia <- malaria_2017 %>% filter(WHO.Region == "South-East Asia")
malaria_wpacific <- malaria_2017 %>% filter(WHO.Region == "Western Pacific")


malaria_2017 %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip() + facet_wrap(~WHO.Region)
malaria_africa %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()+theme_bw()
malaria_americas %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()
malaria_mediter %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()
malaria_eu %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()
malaria_SEasia %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()+theme_bw()
malaria_wpacific %>% ggplot(aes(x = Country, y = No..of.cases)) + geom_point() + coord_flip()

map <- map_data("world")
levels(factor(map$region))


# Adjusting levels name
install.packages("plyr")
library(plyr)
levels(factor(malaria_2017$Country))
malaria_2017$Country <-revalue(malaria_2017$Country, c("Côte d'Ivoire" = "Ivory Coast"))

malaria_map <- merge(map, malaria_2017, by.x = "region", by.y = "Country")
malaria_map <- malaria_map %>% arrange(order)

malaria_map %>% ggplot(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = No..of.cases), colour = "black") + 
  scale_fill_gradient(low = "white", high = "red") +
  ggthemes::theme_map()

####### Worst and best countries in terms of malaria##################


malaria<-covid %>% filter( location=='Uganda'| location=='Mozambique' ) %>% group_by(location)%>%summarise(date,total_deaths,population,location) 
malaria<-delete.na(malaria,0)

vis_miss(malaria)
non_malaria <-covid %>% filter(location=='United States' |location=='Italy'| location=='Canada') %>% group_by(location) %>% summarise(location,date,total_deaths,population)
ggplot(malaria,aes(x=date,y=total_deaths,color=location))+
  geom_line()+geom_line(non_malaria,mapping=(aes(x=date,y=total_deaths,color=location)))+theme_bw()




#since the cases are too few to analyze we will check india 




india<-read.csv('covid-india.csv')
india<-delete.na(india,0)
india$Date<-dmy(india$Date)

india_compare<-india %>% filter(Region=='Andhra Pradesh'|Region=='Jharkhand'|Region=='Tamil Nadu'|Region=='Odisha')
india_compare %>% ggplot(aes(x=Date,y=Confirmed.Cases,color=Region))+geom_line()+theme_bw()
india_compare %>% ggplot(aes(x=Date,y=Death,color=Region))+geom_line()




######stringency and its impact############


strigency<-covid %>% filter(location=='India'|location=='United States'|location=='Russia'|location=='Italy')   
strigency %>% ggplot(aes(x=date,y=new_deaths_per_million,shape=location,color=stringency_index))+
  geom_point()+theme_bw()


################ Impact of population density on covid spread ##################
dens<-covid[order(-covid$population_density),]
dens<-covid[order(covid$population_density),]

dens %>% filter(location == "Monaco"|location=="Singapore"|location=="Hong Kong"|location=="Gibraltar"|location=="Bahrain"|location == "Greenland"|location=="Mongolia"|location=="Namibia"|location=="Australia"|location=="Iceland") %>%  ggplot(aes(x = date, y = new_cases,color=location)) + geom_line()+theme_bw()

##################Impact of smoking##################

smoking<-covid[order(covid$male_smokers,covid$female_smokers),]

smoking<-covid[order(-covid$male_smokers,-covid$female_smokers),]

smoking %>% filter(location == "Indonesia"|location=="Tunisia"|location=="Timor"|location=="Russia"|location == "Ghana"|location=="Ethopia"|location=="Panama"|location=="Georgia"|location=="Nigeria"|location=="Eritrea") %>%  ggplot(aes(x = date, y = new_deaths,color=location)) + geom_line()+theme_bw()


################Impact of diabetes###################
diabetic<-covid[order(-covid$diabetes_prevalence),]
diabetic<-covid[order(covid$diabetes_prevalence),]

diabetic %>% filter(location == "Benin"|location=="Zimbabwe"|location=="Gambia"|location=="Greenland"|location == "Burkina Faso"|location=="Mauritius"|location=="Saudi Arabia"|location=="Egypt"|location=="United Arab Emirates"|location=="Belize") %>%  ggplot(aes(x = date, y = new_deaths,color=location)) + geom_line()+theme_bw()

######################### Handwashing   #############
handwash<-covid[order(-covid$handwashing_facilities),]
handwash<-covid[order(covid$handwashing_facilities),]

handwash%>% filter(location == "Liberia"|location=="Lesotho"|location=="Cameroon"|location=="Democratic Republic of Congo"|location == "Rwanda"|location=="Serbia"|location=="Oman"|location=="Maldives"|location=="Iraq"|location=="Armenia") %>%  ggplot(aes(x = date, y = new_deaths,color=location)) + geom_line()+theme_bw()
###No definitive evidence was found######



###############Age relation to cases######
library(dplyr)
library(tidyr)
age <- covid %>%  group_by(location) %>%
  summarise(mean_cases=mean(total_cases_per_million, na.rm = TRUE),
            aged_65=mean(aged_65_older),
            aged_70=mean(aged_70_older))

age <- age %>% gather("aged_65":"aged_70", key = "old_age", value = "elders.per.million")

age %>% 
  ggplot(aes(x = elders.per.million, y = mean_cases, colour = old_age)) + 
  geom_point() + 
  geom_smooth(method = "lm")+theme_bw()




###### State chart######

install.packages("mapdata")

library(ggmap)
library(ggplot2)
library(mapdata)
us_map= map_data("state")
covid_data<-read.csv("united.csv")
covid_data$State.Territory=tolower(covid_data$State.Territory)
mergedata<- merge(us_map,covid_data,by.x = "region",by.y = "State.Territory")

mergedata<-arrange(mergedata,order)

mergedata %>%  ggplot(aes(x=long,y=lat, group=group))+geom_polygon(aes(fill=Cases.in.Last.7.Days))+theme_bw()



##########  Vaccine Administration #################

data_states<-readxl::read_xlsx("Cali_texas_illinois.xlsx")
data_states$Age<-factor(data_states$Age,levels = c("Under 5 years","5 to 9","10 to 14","15 to 19","20 to 24","25 to 34","35 to 44","45 to 54","55 to 59","60 to 64","65 to 74","75 to 84","85+"))
ggplot(data_states,aes(x=Age,y=Poupulation,color=State))+geom_point()+theme_bw()



