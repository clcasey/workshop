#Chrissy Casey
#clcasey@uga.edu
#05/15/2018 
#Day 2 Workshop: Data Wrangling

#loading libraries 
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
# read in data
ld<-read_csv("C:/Users/workshop/Documents/Casey/lyme.csv")
pop<-read_csv("C:/Users/workshop/Documents/Casey/pop.csv")
prism<-read_csv("C:/Users/workshop/Documents/Casey/climate.csv")

#Exercise view data and run commands and determine what they do 
pop

#this command selects for pop years starting with 2 and puts them after the fips column
pop %<>% select(fips,starts_with("pop2"))
pop

#within the pop data it collects on the year columns and organizes them into a column starting with 2000 and omits cells with NA
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
pop

#the next line of code creates a new column from str_year and removes pop and relabels the column year
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
pop

#this command changes the column year to an integer
pop %<>% mutate(year=as.integer(year))
pop

#this command removes the first zero that is in the values listed in the flips column
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop

#this command makes the values in the fips column an integer
pop %<>% mutate(fips=as.integer(fips))
pop

#the following command removed the str_year
pop%<>%select(-str_year)
pop

#to remove the summary state data I would convert the flips column back to a string of characters function as.characters
#and remove rows that have fips values ending in 000
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld

ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)

#this function pads county fips codes less than 3 an adds them to the state code
fips.builder<-function(st,ct){
  if (str_length(ct)==3){   #if county fip is 3 then paste state and county fip code together as an integer 
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer 
  }
  else if (str_length(ct)==2){   #if county fip is 2 then paste state, add zero, to county fip code together as an integer 
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {   #if county fip is 1(don't need to say because it knows length) then paste state, add zero, to county fip code together as an integer 
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}
ld
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))  # need to do rowwise because there is data in 2 columns/rows
ld %<>% select(-c(STCODE,CTYCODE,str_year))
ld

#inner join adds data together and tells you what it is joined by
ld.prism<- inner_join(ld,prism)

#code join population data with lyme and climate data
pop.ld.prism<-inner_join(ld.prism,pop)
pop.ld.prism

cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
cases_by_year
ld

#exercise to to sort data by state and mean cases. I started with state, 
#then grouped county year, and realized I could just write one that add them all together in the order I wanted  
avg_case_state<- ld %>% ungroup %>% group_by(state) %>%
  summarize(mean=mean(cases)) %>% arrange(desc(mean))
avg_case_state

avg_case_county_yr<- ld %>% ungroup %>% group_by(county,year) %>%
  summarize(mean=mean(cases)) %>% arrange(desc(mean))
avg_case_county_yr

#below is a command for average number of cases across state, county and year
avg_st_ct_yr<- ld %>% ungroup %>% group_by(state,county,year) %>%
  summarize(mean=mean(cases)) %>% arrange(desc(mean))
avg_st_ct_yr

#2002 was the worst year
avg_st_ct_yr[1:20,]
#The top three states are New York, Connecticut, Massachusetts

#saving data using diffferent functions
save(pop.ld.prism, file="pop.ld.prism.rda")
write.csv(pop.ld.prism, file="pop.ld.prism.csv")
write_csv(pop.ld.prism, "pop.ld.prism.csv")
write_csv(pop.ld.prism,path= "1pop.ld.prism.csv")

#exercise using Map package
county_map <- map_data("county")
state_map <- map_data("state")

pop.ld.prism
#the command below reorders the population data by the fips code
ag.fips <- group_by(pop.ld.prism,fips)

#this commmand summarizes the ag.fips by the sum cases for the fip code
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y

#this command adds the columns for state and count to the fips in the data frame ld.16y
ld.16y<-left_join(select(pop.ld.prism,c(state,county,fips)),ld.16y)
ld.16y

#The distinct function returns only differeny values within the ld.16y
ld.16y<-distinct(ld.16y)
ld.16y

#this command renames the columns state with region and county with subregion
ld.16y %<>% rename(region=state,subregion=county)
ld.16y

#this command removes the word county from the column subregion
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y

#this command changes the Region name from captial to lower case
ld.16y$region<-tolower(ld.16y$region)
ld.16y

#this is the same as above just on the subregion column is now lower case
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y

#this creates a new column that takes the log of the cases plus one
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
ld.16y

#this creates a map of the ly.16y data 
map.ld.16y<-left_join(county_map,ld.16y)

#this creates the same map 
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
