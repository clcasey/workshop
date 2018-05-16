#Chrissy Casey
#clcasey@uga.edu
#05/14/18
#my first script
library("ggplot2", lib.loc="~/R/win-library/3.4")
library(lubridate)
library(ggplot2)
# set working directory
setwd("C:/Users/workshop/Documents/Casey/mers")
# read data file in
mers<-read.csv("file:///C:/Users/workshop/Downloads/cases.csv/cases.csv")
# use head function to inspect data
head(mers)

summary(mers)

#use class function to state class of mers$onset is a factor
class(mers$onset)

#fix errors by calling on specific rows
mers$hospitalized[890]<-c("2015-02-20")
# remove a row with -
mers<-mers[-471,]

# use lubridate function ymd to change to date
mers$onset2<-ymd(mers$onset)
mers$hospitalized2<-ymd(mers$hospitalized)
#check class of new df
class(mers$onset2)
#set the min onset to day 0 and omit NA
day0<-min(na.omit(mers$onset2))
#to determine length of epid and make it numeric
mers$epi.day<-as.numeric(mers$onset2-day0)
#use ggplot2 package and ggplot function to explore graphic options
ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day))+
  labs(x="Epidemic day", y="Case count", title="Global count of MERS cases by date of symptom onset", caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day, fill=country))+
  labs(x="Epidemic day", y="Case count", title="Global count of MERS cases by date of symptom onset", caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day, position="fill"))+
  labs(x="Epidemic day", y="Case count", title="Global count of MERS cases by date of symptom onset", caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day, position="fill"))+
  labs(x="Epidemic day", y="Case count", title="Global count of MERS cases by date of symptom onset", caption="Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")+coord_flip()

mers$infection.period<-mers$hospitalized2-mers$onset2

class(mers$infection.period)

mers$infection.period<-as.numeric(mers$infection.period, units="days")

ggplot(data=mers)+
  geom_histogram(aes(x=infection.period),bins=50)+
  labs(x="Infectious period", y="Frequency",title="Distribution of calculated MERS infectious period
", caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#use an ifelse function to replace infectious periods that were negative (nosocomia infections) and replace with a 0
mers$infection.period2<-ifelse(mers$infection.period<0,0,mers$infection.period)

ggplot(data=mers)+
  geom_histogram(aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Distribution of calculated MERS infectious period (positive values only)
", caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_density(mapping=aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Probability density for MERS infectious period (positive values only)"
  , caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_area(stat="bin",mapping=aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Are plot for MERS infectious period (positive values only)",caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_dotplot(mapping=aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Are plot for MERS infectious period (positive values only)",caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_bar(stat="bin",mapping=aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Are plot for MERS infectious period (positive values only)",caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers)+
  geom_dotplot(stat="bin",mapping=aes(x=infection.period2))+
  labs(x="Infectious period",y="Frequency", title="Are plot for MERS infectious period (positive values only)",caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers,aes(x=epi.day, y=infection.period2))+
  geom_smooth(method="loess")

ggplot(data=mers, aes(x=epi.day, y=infection.period2,color=country))+
  geom_smooth(method="loess")

ggplot(data=mers,mapping=aes(x=epi.day,y=infection.period2))+
  geom_point(mapping=aes(color=country))+
  facet_wrap(~country)+
  scale_y_continuous(limits=c(0,50))+
  labs(x="Epidemic day", y="Infectious period",title="MERS infectious period (positive values only) over time", caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=subset(mers,gender%in%c("M","F")&country%in%c("KSA","Oman","Iran","Jordan","Qatar","South Korea","UAE")))+
  geom_point(mapping=aes(x=epi.day,y=infection.period2,color=country))+
  facet_grid(gender~country)+
  scale_y_continuous(limits=c(0,50))+
  labs(x="Epidemic day", y="Infectious period", title="MERS infectious period by gender and country
", caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#EXERCISE: try to determine case fatality
#created a date with ymd
mers$death1<-ymd(mers$death)
#created a numeric value for days until death
mers$casefatal<-as.numeric(mers$death1-day0)
#if there was a value in this new column>0 I replaced it with 1
casnum<-mers$casefatal[which(mers$casefatal>0)]<-1
# the total number of deaths 
numdeaths<-sum(mers$casefatal,na.rm=TRUE)
#dividing total number of deaths by total observations
casefatality<-numdeaths/1740

ggplot(data=mers)+
  geom_histogram(aes(x=country,y=casefatal))+
  labs(x="Number of deaths",y="Frequency")
#use table function to show deaths per country
tabledeath<-table(mers$casefatal,mers$country)
#use table to show total cases per country
tablecountry<-table(mers$country)
#output
# as.vector(tablecountry)
#[1]   11    1    8    2   34 1380    4    1   11   17  186    2   80    2    1
# as.vector(tabledeath)
#[1]   0   0   2   0   9 444   2   0   5   6  36   0  11   1   1
# made tables vectors
tc<-as.vector(tablecountry)
td<-as.vector(tabledeath)
#found the casefataly for each country
td/tc
#[1] 0.0000000 0.0000000 0.2500000 0.0000000 0.2647059 0.3217391 0.5000000 0.0000000 0.4545455 0.3529412 0.1935484 0.0000000
#[13] 0.1375000 0.5000000 1.0000000
cfr<-td/tc
plot(cfr)
