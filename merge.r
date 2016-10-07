rm(list=ls())
library(dplyr)
library(ggplot2)
library(date)
library(lubridate)

setwd("~/sync/data-police-shootings/")
wapo<-read.csv("fatal-police-shootings-data.csv")
c1<-read.csv("the-counted-2015.csv")
c2<-read.csv("the-counted-2016.csv")
dat<-rbind(c1, c2)

geo<-read.csv("city-county-state.csv")%>%select(City, State, County)%>%filter(State!="PR")
z<-which(duplicated(cbind(geo$City, geo$State)))
geo<-geo[-z,]
geo$city<-tolower(geo$City)
geo$state<-geo$State
geo$county<-tolower(geo$County)

pop<-read.csv("AHRF-county-pop.csv")
pop$county<-tolower(pop$County.Name)
pop$county<-gsub("dist. ", "district ", pop$county)
pop$county<-gsub("the district", "district of columbia", pop$county)
pop$county<-gsub("st. ", "saint ", pop$county)
pop$county<-gsub(" \\(b\\)", "", pop$county)
pop$county<-gsub(" \\(ca\\)", "", pop$county)
pop$county<-gsub("\\(ca\\)", "", pop$county)
pop$county<-gsub("prince george's", "prince georges", pop$county)
pop$county<-gsub("matanuska-susitna", "matanuska susitna", pop$county)
pop$county<-gsub("de witt", "dewitt", pop$county)



names(pop)[which(names(pop)=="State.Abbrev")]<-"state"

cdat<-pop%>%filter(FIPS.County.Code!=0)
sdat<-pop%>%filter(FIPS.County.Code==0)


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

alldat$city<-tolower(alldat$city)
alldat$city<-gsub("st. ", "saint ", alldat$city)

###FIGURE OUT BAD MATCHES

d<-left_join(alldat, geo, by=c("city", "state"))
z<-which(duplicated(cbind(d$name, d$state)))
d<-d[-z,]

dups<-which(duplicated(d$name))
test<-which(d$name%in%d[dups, "name"])

check<-d[test,]
check<-check[order(check$name),]

####MERGE POP DAT
d1<-left_join(d, cdat, by=c("county", "state"))

z<-which(is.na(d1$County.Name))
d1[z,1:15]

d1<-d1%>%select(-City, -State, -County, -County.Name)

names(sdat)<paste("ST", names(sdat), sep="")
d1<-left_join(d1, sdat, by="state")
  
write.csv(d1, "merge-dat.csv")