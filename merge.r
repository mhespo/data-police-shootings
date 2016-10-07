library(dplyr)
library(ggplot2)
library(date)
library(lubridate)

setwd("~/data-police-shootings/")
wapo<-read.csv("fatal-police-shootings-data.csv")
c1<-read.csv("the-counted-2015.csv")
c2<-read.csv("the-counted-2016.csv")
dat<-rbind(c1, c2)

geo<-read.csv("city-county-state.csv")%>%select(City, State, County)%>%filter(State!="PR")
z<-which(duplicated(cbind(geo$City, geo$State)))
geo<-geo[-z,]
geo$city<-tolower(geo$City)
geo$state<-geo$State

dat<-dat%>%select(name, age, gender, raceethnicity,day, month, year,
                  city, state)
wapo<-wapo%>%select(name, date, age, race, gender, city, state)


dat$month<-ifelse(dat$month=="January", 1, 
                  ifelse(dat$month=="February", 2,
                   ifelse(dat$month=="March", 3,
                          ifelse(dat$month=="April", 4,
                                 ifelse(dat$month=="May", 5, 
                                        ifelse(dat$month=="June",6,
                                                ifelse(dat$month=="July", 7,
                                                       ifelse(dat$month=="August", 8, 
                                                              ifelse(dat$month=="September", 9,
                                                                     ifelse(dat$month=="October", 10,
                                                                            ifelse(dat$month=="November", 11,
                                                                                   ifelse(dat$month=="December", 12,NA)))
                                                                     )))))))))

wapo$date<-as.Date(wapo$date, "%Y-%m-%d")
wapo$month<-month(wapo$date)
wapo$year<-year(wapo$date)

wapo<-wapo%>%select(-date)
dat<-dat%>%select(-day)
names(dat)[which(names(dat)=="raceethnicity")]<-"race"

alldat<-rbind(wapo, dat)

z<-which(duplicated(cbind(alldat$name, alldat$state, alldat$city)))
dat.unique<-alldat[-z,]

dat.unique$city<-tolower(dat.unique$city)
dat.unique$city<-gsub("st. ", "saint ", dat.unique$city)


###FIGURE OUT BAD MATCHES

d<-left_join(dat.unique, geo, by=c("city", "state"))


miss<-which(is.na(d$County))

