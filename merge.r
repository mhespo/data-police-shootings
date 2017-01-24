rm(list=ls())
library(dplyr)
library(ggplot2)
library(date)
library(lubridate)

setwd("~/sync/data-police-shootings/")


fe<-read.csv("fatalencounters-12-13-16.csv")

names(fe)[2:5]<-c("name", "age", "gender", "race")
names(fe)[9:12]<-c("city", "state", "zip", "county")

fe$county<-tolower(fe$county)


pop<-read.csv("AHRF-county-pop.csv")
pop$county<-tolower(pop$County.Name)
pop$county<-gsub("dist. ", "district ", pop$county)
pop$county<-gsub("the district", "district of columbia", pop$county)
pop$county<-gsub("saint", "st.", pop$county)
pop$county<-gsub(" \\(b\\)", "", pop$county)
pop$county<-gsub(" \\(ca\\)", "", pop$county)
pop$county<-gsub("\\(ca\\)", "", pop$county)
#pop$county<-gsub("prince george's", "prince georges", pop$county)
pop$county<-gsub("matanuska-susitna", "matanuska susitna", pop$county)
pop$county<-gsub("de witt", "dewitt", pop$county)
pop$state<-pop$State.Abbrev

fe$county<-gsub("city of st. louis", "st. louis city", fe$county)
fe$county<-gsub("saint louis city", "st. louis city", fe$county)
fe$county<-gsub("st .louis", "st. louis city", fe$county)
fe$county<-gsub("lasalle", "la salle", fe$county)
fe$county<-gsub("prince georges", "prince george's", fe$county)
fe$county<-gsub("mcclennan", "mclennan", fe$county)
fe$county<-gsub("doña ana", "dona ana", fe$county)
fe$county<-gsub("yukon koyukuk", "yukon-koyukuk", fe$county)
fe$county<-gsub("city of baltimore", "baltimore", fe$county)
fe$county<-gsub("charelston", "charleston", fe$county)
fe$county<-gsub("athens-clarke", "clarke", fe$county)
fe$county<-gsub("prince of wales hyder", "prince of wales-hyder", fe$county)
fe$county<-gsub("laporte", "la porte", fe$county)
fe$county<-gsub("saint joseph", "st. joseph", fe$county)
fe$county<-gsub("brooklyn", "kings", fe$county)
fe$county<-gsub("clallam bay", "clallam", fe$county)
fe$county<-gsub("saint mary's", "st. mary's", fe$county)

fe$county<-gsub("city of newport news", "newport news city", fe$county)

fe$county<-gsub("kuai", "kauai", fe$county)
fe$county<-gsub("shively", "jefferson", fe$county)
fe$county<-gsub("saint clair", "st. clair", fe$county)
fe$county<-gsub("clarl", "clark", fe$county)
fe$county<-gsub("park hill", "denver", fe$county)
fe$county<-gsub("braziria", "brazoria", fe$county)
fe$county<-gsub("andalusia", "covington", fe$county)
fe$county<-gsub("oskaloosa", "okaloosa", fe$county)
fe$county<-gsub("du page", "dupage", fe$county)
fe$county<-gsub("comanche `", "comanche", fe$county)
fe$county<-gsub("philadephia", "philadelphia", fe$county)
fe$county<-gsub("oglala lakota", "shannon", fe$county)
fe$county<-gsub("montrose county", "montrose", fe$county)


cdat<-pop%>%filter(FIPS.County.Code!=0)
sdat<-pop%>%filter(FIPS.County.Code==0)

names(pop)[which(names(pop)=="State.Abbrev")]<-"state"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


fe$year<-as.numeric(substrRight(as.character(fe$Date.of.injury.resulting.in.death..month.day.year.), 4))
fe[which(fe$year==2100), "year"]<-2001

fe<-fe%>%select(name, age, gender, race, city, state, county, year)

write.csv(fe, file="fatalencount-clean.csv", row.names=FALSE)
write.csv(cdat, file="county-pop.csv", row.names=FALSE)
write.csv(sdat, file="state-pop.csv", row.names=FALSE)
 
# z<-which(!(fe$county%in%pop$county))
# cbind(fe[z, "county"], as.character(fe[z, "state"]), as.character(fe[z, "city"]))
# 
# census<-read.csv("nhgis0019_ds172_2010_county.csv")
# 

# dat13<-dat%>%filter(dat$year>=2013)
# 
# 
# ### CORRECT MISMATCHES (ESP ST.d)
# z<-which(is.na(dat13$Percent.Total.Population..One.Race..2010.))
# dat13[z, 9:12]
# 
# 
# ###FIGURE OUT BAD MATCHES
# 
# d<-left_join(alldat, geo, by=c("city", "state"))
# z<-which(duplicated(cbind(d$name, d$state)))
# d<-d[-z,]
# 
# dups<-which(duplicated(d$name))
# test<-which(d$name%in%d[dups, "name"])
# 
# check<-d[test,]
# check<-check[order(check$name),]
# 
# ####MERGE POP DAT
# d1<-left_join(d, cdat, by=c("county", "state"))
# 
# z<-which(is.na(d1$County.Name))
# d1[z,1:15]
# 
# d1<-d1%>%select(-City, -State, -County, -County.Name)
# 
# names(sdat)[7:ncol(sdat)]<-paste("ST", names(sdat), sep="")
# d1<-left_join(d1, sdat, by="state")
# 
# write.csv(d1, "merge-dat.csv")

# 
# dat<-dat%>%select(name, age, gender, raceethnicity,day, month, year,
#                   city, state)
# wapo<-wapo%>%select(name, date, age, race, gender, city, state)
# 
# 
# dat$month<-ifelse(dat$month=="January", 1, 
#                   ifelse(dat$month=="February", 2,
#                    ifelse(dat$month=="March", 3,
#                           ifelse(dat$month=="April", 4,
#                                  ifelse(dat$month=="May", 5, 
#                                         ifelse(dat$month=="June",6,
#                                                 ifelse(dat$month=="July", 7,
#                                                        ifelse(dat$month=="August", 8, 
#                                                               ifelse(dat$month=="September", 9,
#                                                                      ifelse(dat$month=="October", 10,
#                                                                             ifelse(dat$month=="November", 11,
#                                                                                    ifelse(dat$month=="December", 12,NA)))
#                                                                      )))))))))
# 
# wapo$date<-as.Date(wapo$date, "%Y-%m-%d")
# wapo$month<-month(wapo$date)
# wapo$year<-year(wapo$date)
# 
# wapo<-wapo%>%select(-date)
# dat<-dat%>%select(-day)
# names(dat)[which(names(dat)=="raceethnicity")]<-"race"
