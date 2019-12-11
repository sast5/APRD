###################################
## Major League Baseball Project ##
###################################

## Group 19: Whitney Carter, Blair Galbreath, Fairy Nguyenloc, Samuel Statton, Wenbin Yang ##

## Graphs will be slightly different than the draft document because we used Tableau for those visuals ##

## Loading Packages & Files ##
setwd('~/MSBC_5030')
#install.packages("ggplot")
library(ggplot2)

teamdata <- read.csv('MLB_Team_Data.csv', stringsAsFactors = FALSE)
playerdata <- read.csv('MLB_Player_Data.csv', stringsAsFactors = FALSE)
hrdist <- read.csv('MLB_HRDist.csv', stringsAsFactors = FALSE)

head(teamdata)
head(playerdata)
head(hrdist)

## Basic Visualization of Home Runs from 1965 to 2019 ##
boxplot(teamdata$hr ~ teamdata$year) # Total Home Runs
boxplot(teamdata$hr/teamdata$g ~ teamdata$year) # Home Runs per Game

## Subsetting by Different Eras ##
pre1986 <- subset(teamdata, teamdata$year >= 1965 & teamdata$year <= 1985)
steroidyears <- subset(teamdata, teamdata$year >= 1986 & teamdata$year <= 2003)
post2003 <- subset(teamdata, teamdata$year >= 2004 & teamdata$year <= 2019)

boxplot(pre1986$hr/pre1986$g ~ pre1986$year)
boxplot(steroidyears$hr/steroidyears$g ~ steroidyears$year) 
## Dip from 1988 to 1992 is unexplained as no rules were changed and 162 games were played like normal ## 
boxplot(post2003$hr/post2003$g ~ post2003$year)
## Low point in 2014 and record total in 2019 ##


## All Teams 2014-2019 ##
teamagg <- subset(teamdata, teamdata$year>= 2014)
ggplot(data=teamagg, aes(x=year, y=hr, color=team)) +
  geom_line(aes(size=2))+
  geom_point(aes(size=3), show.legend=FALSE)+
  ggtitle('Home Runs by Team')+
  labs(y='Homeruns', x='Year')+
  theme_grey()+
  guides(size=FALSE)


## Compared to Height of Steroid Era ## 
teamagg_2 <- subset(teamdata, teamdata$year>= 1997 & teamdata$year <= 2003)
ggplot(data=teamagg_2, aes(x=year, y=hr, color=team)) +
  geom_line(aes(size=2))+
  geom_point(aes(size=3), show.legend=FALSE)+
  ggtitle('Home Runs by Team')+
  labs(y='Homeruns', x='Year')+
  theme_grey()+
  guides(size=FALSE)


## Individual Player Analysis ##
## Top 5 Home Run Hitters ##
playeragg <- subset(playerdata, playerdata$playerid == "cruzne02" | playerdata$playerid == 'arenano01' | 
                      playerdata$playerid == 'troutmi01' | playerdata$playerid == 'encared01'  |playerdata$playerid == 'martijd02')
playeragg <- subset(playeragg, playeragg$year >= 2014 & playeragg$team != 'TOT') ## 
playerhr <- aggregate(hr ~ name + year, FUN=sum, data=playeragg)

ggplot(data=playerhr, aes(x=year, y=hr, color=name)) +
  geom_line(aes(size=2))+
  geom_point(aes(size=3), show.legend=FALSE)+
  ggtitle('Top 5 Homerun Hitters')+
  labs(y='Homeruns', x='Year')+
  theme_grey()+
  guides(size=FALSE)

## League Average ##
playeragg_2 <- subset(playerdata, playerdata$year >= 2014 & playerdata$team!= 'TOT' & playerdata$g >= 100)
playerhr2 <- aggregate(hr~year, FUN=mean, data=playeragg_2)

ggplot(data=playerhr2, aes(x=year, y=hr)) +
  geom_line(aes(size=2))+
  geom_point(aes(size=3), show.legend=FALSE)+
  ggtitle('Top 5 Homerun Hitters')+
  labs(y='Homeruns', x='Year')+
  theme_grey()+
  guides(size=FALSE)
# Not surprising since the team average mirrors this graph

## Top 5 OPS ##
playerops <- aggregate(ops ~ name + year, FUN=mean, data=playeragg)

ggplot(data=playerops, aes(x=year, y=ops, color=name)) +
  geom_line(aes(size=2))+
  geom_point(aes(size=3), show.legend=FALSE)+
  ggtitle('OPS')+
  labs(y='OPS', x='Year')+
  theme_grey()+
  guides(size=FALSE)



#############
## T-Tests ##
#############

## 2019 vs. 2015-2018 ##
not2019 <- subset(teamdata, teamdata$year >= 2015 & teamdata$year <= 2018)
data2019 <- subset(teamdata, teamdata$year == 2019)
t.test(data2019$hr, not2019$hr)


## 2019 vs. 2009-2018 ##
lastdec <- subset(teamdata, teamdata$year >= 2009 & teamdata$year <= 2018)
t.test(data2019$hr, lastdec$hr)


#################
## Ball Travel ##
#################

boxplot(hrdist$avg_hr_distance ~ hrdist$year) ## Doesn't tell us much

hrdist2019 <- na.omit(subset(hrdist, hrdist$year==2019 & hrdist$attempts > 30))
hrdistrest <- na.omit(subset(hrdist, hrdist$year != 2019 & hrdist$attempts > 30))

## Max Distance ##
par(mfrow=c(1,2))
hist(hrdist2019$max_distance)
hist(hrdistrest$max_distance)

mean(hrdist2019$max_distance)
mean(hrdistrest$max_distance)
## Different

## Average Distances ##
par(mfrow=c(1,2))
hist(hrdist2019$avg_distance)
hist(hrdistrest$avg_distance)

mean(hrdist2019$avg_distance)
mean(hrdistrest$avg_distance)
## Slight Difference

## Average Home Run Distance ##
par(mfrow=c(1,2))
hist(hrdist2019$avg_hr_distance)
hist(hrdistrest$avg_hr_distance)

mean(hrdist2019$avg_hr_distance)
mean(hrdistrest$avg_hr_distance)
## Look Similar

#############
## T-Tests ##
#############

## Average Distance ##
t.test(hrdist2019$avg_distance, hrdistrest$avg_distance)

## Max Distance ##
t.test(hrdist2019$max_distance, hrdistrest$max_distance)

## Average Home Run Distance ##
t.test(hrdist2019$avg_hr_distance, hrdistrest$avg_hr_distance)





