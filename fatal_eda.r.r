#####################################
### Analysis, Fatal Encounters Data
### R, EDA, County Level 
#####################################

### Set-up
## load packages; attached stuff etc. 
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringi)
setwd("C:/Users/Mike/Dropbox/Random Analysis/wapo project")

## Attach & merge datasets 
# fatal encounters data, clean version
# pre-2013 data isn't very good; exlcude for now 
fdat = read.csv('fdat.csv') %>%
	   filter(year >= 2013) %>%
	   select(-city) %>%
	   mutate(county.state = paste0(county, state))

# NOTE:
# Neeed to decide what to do with strange age values

# county data
cdat = read.csv('cdat.csv') %>%
	   mutate(county.state = paste0(county, state))

# joiuned data; 
# first looking at disparities in counties where shootings do occur
tmp = inner_join(fdat, cdat, 'county.state') %>%
	  mutate(age = as.numeric(as.character(age)))

#####################################################################
# NOTE:  
#Some of these don't match; e.g.,: 
table(fdat$county.state == 'oglala lakotaSD')
table(cdat$county.state == 'oglala lakotaSD')

#e.g.,: 
table(fdat$county.state == 'saint louis cityMO')
table(cdat$county.state == 'saint louis cityMO')


# Lose these 41 individuals in joining 
fdat$county.state[which(!(fdat$county.state %in% cdat$county.state))]

# 0 count counties (+ probably some errors)
cdat$county.state[which(!(cdat$county.state %in% fdat$county.state))]

#######################################################################

### Analysis
## First, some EDA of counties

# 1: count per county 
ct.tmp = tmp %>%
		 group_by(county.y, state.y) %>% 
		 tally() 

# Hmm... this is... difficult 
ct.tmp$n %>% table() %>% plot()
ct.tmp$n %>% log %>% hist
ct.tmp$n %>% quantile(seq(0, 1, .1))

# 2: Let's take a look at some rates
ct.tmp2 = tmp %>%
		  group_by(county.y, state.y, Population.Estimate..2014.) %>%
		  tally() %>%
		  mutate(per100k = (n/Population.Estimate..2014.)*100000) %>%
		  na.omit()

ct.tmp2$per100k %>% quantile(seq(0, 1, .1))

ggplot(ct.tmp2, aes(x = county.y, y = sort(per100k, decreasing = TRUE))) + 
	geom_point() + 
	theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())  +
	# 90% quantile mark 
	geom_hline(yintercept = 8.45, lty = 'dashed') 

# 3: OK, let's take a look at the top 10%
hold = ct.tmp2[which(ct.tmp2$per100k > 8.44),]

# Population size and death counts 
hold$Population.Estimate..2014. %>% summary()
hold$n 							%>% summary()

ggplot(hold, aes(x = county.y, y = sort(per100k, decreasing = TRUE), 
				 size = n, color = Population.Estimate..2014.)) + 
	geom_point() + 
	theme_bw() +
	theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())  
	
# 4: Let's look at abitary cutoff (n.deaths = 5; slightly above 80% threshold)
ct.tmp3 = tmp %>%
		  group_by(county.y, state.y, Population.Estimate..2014.) %>%
		  tally() %>%
		  filter(n >= 5) %>%
		  mutate(per100k = (n/Population.Estimate..2014.)*100000) %>%
		  na.omit()

nrow(ct.tmp3)
ct.tmp3$per100k %>% quantile(seq(0, 1, .1))

ggplot(ct.tmp3, aes(x = county.y, y = sort(per100k, decreasing = TRUE), 
				 size = n, color = Population.Estimate..2014.)) + 
	geom_point() + 
	theme_bw() +
	theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank()) 

# Second, let's take a look at Black/White disparites
# 1: count per county 
bw.ct.tmp = tmp %>%
		    filter(race %in% c('African-American/Black', 
		   					   'European-American/White')) %>% 
		    group_by(county.y, state.y, race) %>% 
		    tally() 

# Some mostly usless plots 
ggplot(bw.ct.tmp, aes(x = county.y, y = n, group = race, colour = race)) +
	geom_bar(stat = 'identity') +
	facet_grid(.~race)

bw.ct.tmp %>% 
filter(race == 'African-American/Black') %>% 
ungroup() %>% 
select(n) %>% 
table() %>% 
plot()

bw.ct.tmp %>% 
filter(race == 'European-American/White') %>% 
ungroup() %>% 
select(n) %>% 
table() %>% 
plot()

hold1 = bw.ct.tmp %>% filter(race == 'African-American/Black')
quantile(hold1$n, seq(0, 1, .1))

hold2 = bw.ct.tmp %>% filter(race == 'European-American/White')
quantile(hold2$n, seq(0, 1, .1))

# 2: Let's take a look at some rates
bw.ct.tmp2 = tmp %>%
		     group_by(county.y, state.y, race, 
		    		  Population.Black.Afr.Am.Non.Hisp.Lat..2013.,
		    		  Population.White.Non.Hisp.Latino..2013.) %>%
		     filter(race %in% c('African-American/Black', 'European-American/White')) %>%
		     tally() %>%
		     mutate(per100k = ifelse(race == 'African-American/Black',	
				  				 			 (n/Population.Black.Afr.Am.Non.Hisp.Lat..2013.)*100000,
				  			  ifelse(race == 'European-American/White', 
				  			 				 (n/Population.White.Non.Hisp.Latino..2013.)    *100000, -100))
				  	) %>%
		    na.omit()

# Some visuals
hold1 = bw.ct.tmp2 %>% filter(race == 'African-American/Black')
quantile(hold1$per100k, seq(0, 1, .1))

hold2 = bw.ct.tmp2 %>% filter(race == 'European-American/White')
quantile(hold2$per100k, seq(0, 1, .1))

hist(log(hold1$per100k))
hist(log(hold2$per100k))

#Uh... let's look at the huge rates for Blacks 
# Very low black population places + low n deaths
hold1 %>% filter(per100k >= 300)

# and high death rates in generaly are ...
# omg google 'jim hogg county'
bw.ct.tmp2 %>% filter(per100k >= 300)

#Filtering these high but low/low places for clarity
bw.ct.tmp22 = bw.ct.tmp2 %>% 
			  filter(per100k <= 300)

ggplot(bw.ct.tmp22, aes(x = county.y, y = sort(per100k, decreasing = TRUE))) +
	geom_point() + 
	theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank())  +
	facet_grid(.~race) 


# K, lets look at disparites
bw.ct.tmp3 = bw.ct.tmp2 %>%
			 ungroup() %>%
			 group_by(county.y, state.y) %>%
			 mutate(perdiff = lag(per100k) - per100k)

# And describe
summary(bw.ct.tmp3$perdiff)

quantile(bw.ct.tmp3$perdiff, seq(0, 1, .1), na.rm = TRUE)

# Largest disparities (90%)
hold3 = bw.ct.tmp3 %>% 
		filter(perdiff >= 19) %>%
		select(county.y, state.y)

# Yeah... sort of a spread problem
# e.g.,: 
bw.ct.tmp2 %>% filter(county.y == 'atascosa' & state.y  == 'TX')
bw.ct.tmp2 %>% filter(county.y == 'clackamas' & state.y  == 'OR')
bw.ct.tmp2 %>% filter(county.y == 'stanislaus' & state.y  == 'CA')

# So, let's see how things shake out when we use an arbitary population cutoff 
quantile(tmp$Population.Black.Afr.Am.Non.Hisp.Lat..2013, seq(0, 1, .1))

# Let's try about 50th and up
bw.ct.tmp4 = bw.ct.tmp2 %>%
			 ungroup() %>%
			 filter(Population.Black.Afr.Am.Non.Hisp.Lat..2013. >= 45000) %>%
			 group_by(county.y, state.y) %>%
			 mutate(perdiff = lag(per100k) - per100k)

# And describe
# much more subtle
summary(bw.ct.tmp4$perdiff)

quantile(bw.ct.tmp4$perdiff, seq(0, 1, .05), na.rm = TRUE)

# Largest disparities (90%) (n = 12)
hold4 = bw.ct.tmp4 %>% 
		filter(perdiff >= 7) %>%
		select(county.y, state.y)

bw.ct.tmp2 %>% filter(county.y == 'alameda' & state.y  == 'CA')
bw.ct.tmp2 %>% filter(county.y == 'baltimore' & state.y  == 'MD')
bw.ct.tmp2 %>% filter(county.y == 'brevard' & state.y  == 'FL')
bw.ct.tmp2 %>% filter(county.y == 'orange' & state.y  == 'CA')
bw.ct.tmp2 %>% filter(county.y == 'harrison' & state.y  == 'MS')
bw.ct.tmp2 %>% filter(county.y == 'san francisco' & state.y  == 'CA')
bw.ct.tmp2 %>% filter(county.y == 'ramsey' & state.y  == 'MN')

# & lets check medium disparity places (~60 - 85) (n = 9)
hold5 = bw.ct.tmp4 %>% 
		filter(perdiff >= 4 & perdiff < 5) %>%
		select(county.y, state.y)

# Some interesting ones for sure; rate changes fairly quickly for blacks
bw.ct.tmp2 %>% filter(county.y == 'allegheny' & state.y  == 'PA')
bw.ct.tmp2 %>% filter(county.y == 'east baton rouge' & state.y  == 'LA')
bw.ct.tmp2 %>% filter(county.y == 'fairfield' & state.y  == 'CT')
bw.ct.tmp2 %>% filter(county.y == 'spartanburg' & state.y  == 'SC')







## Second, Let's take a look at Black/White disparities
## Blacks 
## 1:  count per county 
#b.ct.tmp = tmp %>%
#		   filter(race %in% c('African-American/Black')) %>% 
#		   group_by(county.y, state.y) %>% 
#		   tally() #

## Hmm... this is... difficult 
#b.ct.tmp$n %>% table() %>% plot()
#b.ct.tmp$n %>% log %>% hist
#b.ct.tmp$n %>% quantile(seq(0, 1, .1))#

## 2: Let's take a look at some rates
#b.ct.tmp2 = tmp %>%
#		    group_by(county.y, state.y, Population.Black.Afr.Am.Non.Hisp.Lat..2013.) %>%
#		    filter(race %in% c('African-American/Black')) %>% 
#		    tally() %>%
#		    mutate(per100k = (n/Population.Black.Afr.Am.Non.Hisp.Lat..2013.)*100000) %>%
#		    na.omit()#

#b.ct.tmp2$per100k %>% quantile(seq(0, 1, .1))#

#ggplot(b.ct.tmp2, aes(x = county.y, y = sort(per100k, decreasing = TRUE))) + 
#	geom_point() + 
#	theme(axis.text.x  = element_blank(),
#          axis.ticks.x = element_blank())  +
#	# 90% quantile mark 
#	geom_hline(yintercept = 8.45, lty = 'dashed') #

## Uh... 
#b.ct.tmp2 %>% filter(per100k > 10000)#

## Let's try that again 
#b.ct.tmp2 = b.ct.tmp2 %>% filter(per100k < 300)
#b.ct.tmp2$per100k %>% quantile(seq(0, 1, .1))#

#ggplot(b.ct.tmp2, aes(x = county.y, y = sort(per100k, decreasing = TRUE))) + 
#	geom_point() + 
#	theme(axis.text.x  = element_blank(),
#          axis.ticks.x = element_blank())  #
#

## 3: lets look at the these super high counties
#b.hold = b.ct.tmp2[which(b.ct.tmp2$per100k > 35.38),]#

## Population size and death counts 
#b.hold$Population.Black.Afr.Am.Non.Hisp.Lat..2013. %>% summary()
#b.hold$n 										   %>% summary()#

#ggplot(b.hold, aes(x = county.y, y = sort(per100k, decreasing = TRUE), 
#				 size = n, color = Population.Black.Afr.Am.Non.Hisp.Lat..2013.)) + 
#	geom_point() + 
#	theme_bw() +
#	theme(axis.text.x  = element_blank(),
#          axis.ticks.x = element_blank())  #

## 4: Let's look at abitary cutoff (n.deaths = 5; slightly above 80% threshold)
#b.ct.tmp3 = tmp %>%
#		    group_by(county.y, state.y, Population.Black.Afr.Am.Non.Hisp.Lat..2013.) %>%
#		    filter(race %in% c('African-American/Black')) %>% 
#		    tally() %>%
#		    filter(n >= 5) %>%
#		    mutate(per100k = (n/Population.Black.Afr.Am.Non.Hisp.Lat..2013.)*100000) %>%
#		    na.omit()#

#nrow(b.ct.tmp3)
#b.ct.tmp3$per100k %>% quantile(seq(0, 1, .1))#

#ggplot(b.ct.tmp3, aes(x = county.y, y = sort(per100k, decreasing = TRUE), 
#				 size = n, color = Population.Black.Afr.Am.Non.Hisp.Lat..2013.)) + 
#	geom_point() + 
#	theme_bw() +
#	theme(axis.text.x  = element_blank(),
#          axis.ticks.x = element_blank()) #
#
#
#
#
#
#
#
#
#

## 1a: shooting count/state pop. for each state
##state_vars = c('state', "STPopulation.Estimate..2014.", names(dat)[157:252])
##dots = lapply(state_vars, as.symbol)##

##tmp.state = dat %>%
##	  	    group_by_(.dots = dots) %>% 
##	        tally() %>%
##	        mutate(per100k = (n/STPopulation.Estimate..2014.)* 100000) #

#tmp.state2 = dat %>%
#	  	  	 group_by(state, STPopulation.Estimate..2014.) %>% 
#	       	 tally() %>%
#	       	 mutate(per100k = (n/STPopulation.Estimate..2014.)* 100000) ##

#ggplot(tmp.state2, aes(x = state, y = per100k)) + 
#	geom_point() +
#	theme_bw() +
#	coord_flip()#

## 1b: Race specfic rates per 100k 
## Blacks 
#tmp.state.B = dat %>%
#	  	      group_by_(.dots = dots) %>% 
#	          filter(race %in% c('B', 'Black')) %>%
#	          tally() %>%
#	          mutate(B.per100k = (n/STPopulation.Black.Afr.Am..2013.)* 100000) %>%
#	          ungroup() %>%
#	          select(state, n, STPopulation.Black.Afr.Am..2013., B.per100k)#

## Whites 
#tmp.state.W = dat %>%
#	  	      group_by_(.dots = dots) %>% 
#	          filter(race %in% c('W', 'White')) %>%
#	          tally() %>%
#	          mutate(W.per100k = (n/STPopulation.White..2013.)* 100000) %>%
#	          ungroup() %>% 
#	          select(state, n, STPopulation.White..2013., W.per100k)#

## Note: is the pop gor Whites right???
#dat$STPopulation.White..2013. 		 %>% summary()
#dat$STPopulation.Black.Afr.Am..2013. %>% summary()#

#dat$STPopulation.White.Non.Hisp.Latino..2013 %>% summary()#

#tmp.state = tmp.state.W %>%
#			inner_join(tmp.state.B, 'state') %>%
#			gather()#

#ggplot(tmp.state, aes(x = state, y = B.per100k)) + 
#	geom_point() +
#	theme_bw() +
#	coord_flip()#


