#Chrissy Casey
#clcasey@uga.edu
#05/15/2018
#Data Modeling in R

library(tidyverse)
library(magrittr)
library(GGally)
library(ggplot2)
library(modelr)

#Task 1: read in the data from the previous module
ld.prism.pop<-read_csv("C:/Users/workshop/Documents/Casey/1pop.ld.prism.csv")

#This would make a 3 x 3 plot
#ggpairs(df,columns=c("x","y","z"))

ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))
ld.prism.pop

# I took the log of the size and made a new column with these values
ld.prism.pop$log10<-log10(ld.prism.pop$size)

# we want to add 1 to cases because if there was zero cases the log of zero is undefined and R can interpret
ld.prism.pop$log10plus1<-log10(ld.prism.pop$cases+1)
ld.prism.pop

ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10","log10plus1"))

#I converted the ld.prism.pop$prcp to a tibble format inoder to run the sample_n command
set.seed(222);

# random <- tibble(ld.prism.pop)
# small<-random%>%sample_n(100)

random <- ld.prism.pop %>% sample_n(100)
random

myPlot<-ggplot(data=random, mapping = aes(x=prcp, y=avtemp)) + 
  geom_point()

myPlot

myPlot+geom_smooth(method = "lm")

myModel<-lm(avtemp~prcp,data=random)

summary(myModel)
summary(myModel)$coefficients[2,1]
summary(myModel)$coefficients[2,4]

#Task7
coef(lm(avtemp ~ prcp, data = random))
# the slope is significantly different the p-value is <0.5

#Task 8
#first I call the large dataset then I group by year and summarize by the sum of size in a new column total. 
#Then the data is passed to ggplot
ld.prism.pop%>%group_by(year)%>%summarize(total=sum(size))%>% ggplot(.)+geom_point(aes(x=year,y=total))

#Task 9
by_state<-ld.prism.pop%>%group_by(state)
by_state

#Task 10
by_state%<>%nest
by_state

#Task 11: displaying GA data in console window
by_state$data[[10]]

#Task 12: Write a function for a data fram and returns a linear model 
linGrowth<-function(df){
  lm(size~year,data=df)
}
#can't run with by_state because when I nested it it changed the column "size" so I got an error "object size not found"
#so I checked the model on ld.pop.prism since it has year and size and it returned coefficients 
linGrowth(ld.prism.pop)

models<-purrr::map(by_state$data,linGrowth)
models

detach("package:maps", unload=TRUE)
#Needed to include purr::map because couldn't detach maps and it was using maps::map function instead of purr::map
by_state %<>% mutate(model = purrr::map(data, linGrowth))

by_state %<>% mutate(resids = map2(data, model, add_residuals))

by_state
#resids is in a list of tibbles 
#task 15 write a function 
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = map(resids,sum_resids))
by_state

#Task 16 write a function to return the slopes of a linear model
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))
by_state
slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

#Task 17 plot the growth rate (slope value) for all states
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 18 plot the total residuals
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 19
by_state2<-ld.prism.pop%>%group_by(state)
by_state2%<>%nest

#Task 20
runCor<-function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}
by_state2%<>%mutate(spCor=purrr::map(data,runCor))

spCors<-unnest(by_state2,spCor)
spCors%<>%arrange(desc(spCor))
spCors$state<-factor(spCors$state,levels=unique(spCors$state))
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle=90,hjust=1))






