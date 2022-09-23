library(ggplot2)
library(tidyverse)
library(dplyr)
library(features)

#CPK: no need to load dplyr and ggplot, both are included in tidyverse

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#1. recreating pseed.wide
pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()


#CPK: Awesome, very concise!!

#2. Code giving the mean maximum of all of the amp.sum
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble() %>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
return(f$peaks) 
}

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  group_by(date,fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum)) %>% 
  print()

#CPK:we didn't want to summarize amp by date. So . . 
pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum)) %>% 
  print()

#3.function for se
se <- function(x){
  sd(x) / sqrt(length(x))}

#code creating a column for se and visualizing the change
pseed.sum.max <- pseed.wide%>%
    group_by(date,fish,bl.s)%>%
    mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
    filter(peak==T) %>% 
    group_by(date,fish,bl.s) %>% 
    summarize(amp.sum.se=SE(amp.sum)) %>% 
    left_join(pseed.sum.max,by=c("date"="date","fish"="fish","bl.s"="bl.s"))%>%
    print()  
  
#CPK, this is one way of doing it, but this is more concise, i.e., adding se with mean. Also note, no need to speciy "on" in the join if there are matching column names.

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) %>% 
  group_by(fish,bl.s) %>% 
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se=SE(amp.sum))
 


#4. code plotting amp.sum.mean vs specific swim speed with error bars
pseed.sum.max %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se))

#CPK: Excellent! ^

#5. reading new file as a tibble and merging it with pseed.sum.max
pseed.met.rate <-read_csv("pseed.met.rate.csv")
as_tibble(pseed.met.rate)
#CPK: This is unneeded [-1]

# pseed.sum.max <- pseed.sum.max %>% 
#   left_join(pseed.met.rate,by=c("date"="date","fish"="fish","bl.s"="bl.s")) %>% 
#   print()

#CPK, this won't work ^^ bc I removed date as a group. Commented out. And we wanted the mean met rate for each fish at each speed. So . . 

pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate%>%
              group_by(fish,bl.s)%>%
              summarize(met.rate.mean=mean(met.rate)))

#6. plot of metabolic power output of each fish vs mean maximum of amp.sum
pseed.sum.max %>%
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish))+geom_point()

#CPK: Plotting according to my pseed.sum.max.And, when we say plot "something 1 vs something 2" we generally be y vs x, so in this case, met rate (on the y) vs amp.sum (on the x). 


pseed.sum.max %>%
  ggplot(aes(x=amp.sum.mean,y=met.rate.mean,col=fish))+geom_point()

#CPK: Really good work. Getting there!!


