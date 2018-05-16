#Using read.csv function to read west nile virus data and the ggplot function to create histogram
#Chrissy Casey
#clcasey@uga.edu
#05/14/2018
#load libraries first
library(ggplot2)
library(magrittr)

#read data as .csv
wnv<-read.csv("file:///C:/Users/workshop/Documents/Casey/wnv.csv")

#just plot wnv cases using geom_histogram function
ggplot(data=wnv)+geom_histogram(aes(x=wnv$Total))

#added labels
ggplot(data=wnv)+geom_bar(mapping=aes(x=Year,y=Total, fill=State),stat="identity")

#took the log of the total number cases and renamed file
logcase<-log(wnv$Total)

#look at a histogram of the log of cases
hist(logcase)

#Created a new column in the df wnv for the logcase
wnv$log<-logcase

#plotted the log of cases
ggplot(data=wnv)+geom_histogram(aes(x=wnv$log))+labs(x="Log of Total Number of Cases", y="Counts",Title="Log of Total number of cases")

#plotted the number of cases on a log scale
ggplot(data=wnv,aes(x=wnv$Total))+geom_histogram()+scale_x_log10()+labs(x="Log Scale of Total Number of Cases", y="Counts",Title="Total number of cases with log scale")

#created a new object - vector of case fatality rate
CFR<-(wnv$Fatal/wnv$Total)

# created a new column for the vector created above in the df wnv
wnv$CFR<-CFR

#plotted the case fatality
ggplot(data=wnv)+
  geom_histogram(aes(x=wnv$CFR))+
  labs(x="Case Fatality Rate",y="Frequency", title="Case Falatity Rate per state by year")

#Exercise verifying that the values in the total column are correct using arirthmetic operators
total<-sum(wnv$EncephMen,wnv$Fever,wnv$Other)
total
#returned 27605

#used sum function to sum entire total column
sum(wnv$Total)
#returned 27605

#exercise to find annunal case count for each state rounded down to nearest dozen
wnv$dozen<-wnv$Total%/%12
# use to find rouning error
wnv$errors<-wnv$Total%%12

sum(wnv$errors)

mean<-function(x){
  #computes the mean of neuroinvasive disease rate for all states
  #
  #Args:
  #  x:vector of values to be averaged
  #  w: vector of total values
  #
  #Returns: the mean
  #Computation 
  s<-sum(x)
  n<-length(x)
  m<-s/n
  return(m)
}
mean(wnv$EncephMen)

ster<-function(x){
  #computes the standard error of neuroinvasive disease
  #args:
  #returns: the standard error
  #computation
  s<-sqrt(var(x))
  n<-length(x)
  d<-sqrt(n)
  u<-s/d
  return(u)
}
ster(wnv$EncephMen)
#returns 4.839354

#checked by taking sd/sqrt(of sample size)
error<-sd(wnv$EncephMen)/sqrt(length(wnv$EncephMen))
error
#returns 4.839354

#skipped pipe exercise to try a loop exercise
times<-seq(1:10)
some.algorithm<-function(t){
  y<-t*10
}
output<-c()
for(t in times) {
  output<-c(output,some.algorithm(t))
}
plot(times,output,types="p")

#Exercise to loop over all years in the WNV data
output <- c()

for(y in unique(wnv$Year)){

  #Total number of states reporting cases
  states <- length(wnv$Total[wnv$Year == y])
  
  #total number of reported cases
  cases <- sum(wnv$Total[wnv$Year == y])
  
  #total number of fatalities
  fatal<-sum(wnv$Fatal[wnv$Year==y])
  
  #and case fatality rate
  numcfr<-mean(wnv$CFR[wnv$Year==y])
  
  output <- rbind(output, c(y, states, cases, fatal, numcfr))
  }
output
