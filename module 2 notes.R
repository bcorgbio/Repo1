library(ggplot2)
library(tidyverse)
setwd("~/experimental_methods_in_organismal_bio/Scripts/Repo1")

iris <- group_by(iris,Species)
summarize(iris, mean_length=mean(Sepal.Length))

#or

iris%>%
  group_by(Species) %>% 
  summarize(mean_length=mean(Sepal.Length))

#new
iris <- group_by(iris,Species)
iris_mean <- summarise(iris,mean_length=mean(Sepal.Length))
ggplot(data=iris_mean,aes(x=Species,y=mean_length))+geom_bar(stat="identity")

#or

iris %>% 
  group_by(Species) %>% 
  summarize(mean_length=mean(Sepal.Length)) %>% 
  ggplot(aes(x=Species,y=mean_length))+geom_bar(stat="identity")

# ?dplyr command will pull up data manipulation commands

## can use ... in a function command to call all values in the table; this means
## you do not need to write out all of the values in the function


library(ggplot2)
library(tidyverse)
library(dplyr)
setwd("~/experimental_methods_in_organismal_bio/Scripts/Repo1")

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

##goal is to assess how amplitude changes with speed, but the 
## main data table is reported in motor voltage, not something easily usable

pseed2 <- pseed %>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed.bl%>%
  print()

pseed2%>%
  select(fish)%>%
  unique()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()

#graph too complicated, so use alpha in geometric point to dim overlapping data points
#alpha on a scale from 0-1, 0 is dimmest and 1 is no dimming

pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

library(features)

exp1 <- pseed2 %>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

fget(f1)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)                      

f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

f.tib <- fget(f2)[2:3] %>% 
  as_tibble() %>% 
  filter(curvature<0) %>% 
  mutate(peaks=round(crit.pts,0)) %>% 
  print()

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

pseed2%>%
  summarize(n=length(unique(date)))

find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

#facet_grid breaks the plot up into a grid according to values in the data

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#geom_smooth function in the end of the ggplot command gives the best fit line
pseed2

amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#code above plots the means vs. the speed and a linear regression line for each fish

pseed2

#creating a variable that represents the sum of the amplitude data, left and right
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

#when showing the data, we hide one fin to clean the data. Otherwise each value spans two cells.
pseed2 %>% 
  filter(fin=="R")

#pivot the data wider to give the left and right fins their own amplitude columns
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 
#this code give the summed amplitude and the amplitude for the left and right column

pseed.wide

#2. mean maximum for all amp.sums
pseed.sum.max <- pseed.wide %>% 
  group_by(fish,m.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) %>% 
  mutate(max.amp.sum = T ) %>% 
  mutate(amp.sum.mean = mean(max.amp.sum)) %>% 
  print()

find.peaks <- function(x,y,mult=100) 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
  filter(curvature<0)%>% 
  mutate(peak=round(crit.pts,0))
return(f$peaks) 

pseed.wide %>%
  as_tibble(pseed.sum.max) %>% 
  filter(fish%in%unique(fish)[1:3])%>%
  group_by(fish,m.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  
  pseed.wide

#run on grouped tibble by experiment

f2 <- features(x = frame, y=amp.sum)

pseed.sum.max %>%
  group_by(fish,m.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

#join the peak data with the original tibble  

pseed.wide %>% 
  filter(curvature<0) %>% 
  mutate(peak=round(crit.pts,0)) %>% 
  return(f$peaks)

pseed.wide


pseed.max %>% 
  group_by(fish, date) %>% 
  summarize(find.peaks(x=frame,y=amp.sum))

left_join(fish, peak)





#could find the peaks for each experiment which is unqiuely named and then join that to the og data and then go from code in lab:
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)




pseed.sum.max <- fget(features(x = x, y=y*mult))[2:3] %>% 
  as_tibble(pseed.sum.max) %>% 
  filter(curvature<0) %>% 
  mutate(peaks=round(crit.pts,0)) %>% 
  return(f$peaks)

mutate(peaks=frame %in% amp.sum$peaks) %>% 
  mutate(amp.sum.mean=mean(amp.sum$peaks))
print()




#3. custom function that computes the SE of the mean
amp.sum.se <- function(x) {
  n <- sum(fish)
  (x - (amp.sum-amp.sum.mean)/n)
}

stored_vector <- amp.sum.se()

pseed.sum.max <- pseed.sum.max %>% 
  group_by(fish) %>% 
  add_column(amp.sum.se)

#peaks code
pseed.sum.max <- pseed2 %>% 
  group_by(date,fish,bl.s) %>% 
  mutate(peak=frame %in% find.peaks(frame,amp.sum)) %>% 
  filter(peak==T)

pseed.sum.max %>% 
  group_by(fish,bl.s) %>% 
  summarize(amp.sum.mean=mean(amp.sum))

