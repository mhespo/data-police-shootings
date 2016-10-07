library(dplyr)
library(ggplot2)
setwd("~/data-police-shootings/")
wapo<-read.csv("fatal-police-shootings-data.csv")
c1<-read.csv("the-counted-2015.csv")
c2<-read.csv("the-counted-2016.csv")
guard<-rbind(c1, c2)

m1<-glmer(armed!="unarmed"~(race=="B")+(1|state), data=dat, family="binomial")
