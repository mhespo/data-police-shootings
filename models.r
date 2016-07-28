dat<-read.csv("~/data-police-shootings/fatal-police-shootings-data.csv")
m1<-glmer(armed!="unarmed"~(race=="B")+(1|state), data=dat, family="binomial")
