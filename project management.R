#Chrissy Casey
#clcasey@uga.edu
#05/16/2018
#Part 3: project management 
#exercise copy MERS case.csv file to working directory
#exercise load mers and make a plot
library(ggplot2)

MERS<-read.csv("C:/Users/workshop/Documents/Casey/R-workshop-1/cases.csv")
head(MERS)
ggplot(data=MERS, mapping=aes(x=country))+
  geom_bar()+labs(x=Country, y=counts,title="Number of Cases per Country")
