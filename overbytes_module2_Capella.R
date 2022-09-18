#Mod 2 Project Report

#setup
library(tidyverse)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


#1 pseed.wide data tibble
pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 


#2 mean max of all amp.sums
  #tibble = pseed.sum.max
  #column = amp.sum.mean
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  group_by(date,fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum))


#3 add amp.sum.se to pseed.sum.max
SE <- function(x) {
  sd(x) / sqrt(length(x))
}

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T) %>% 
  group_by(date,fish,bl.s) %>% 
  summarize(amp.sum.se=SE(amp.sum)) %>% 
  left_join(pseed.sum.max,by=c("date"="date","fish"="fish","bl.s"="bl.s"))%>%
  print()
  

#4 - plot the mean amp.sum vs. specific swimming speed
  #SE error bars & color by fish
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=bl.s, colour=fish)) + 
  geom_errorbar(aes(ymin=bl.s-amp.sum.se, ymax=bl.s+amp.sum.se), width=.005) +
  geom_point()


#5 merge pseed.met.rate with pseed.sum.max
pseed.met.rate <- read_csv("pseed.met.rate.csv")

pseed.met.rate$bl.s <- as.numeric(as.character(pseed.met.rate$bl.s))
pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))

pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate,by=c("date"="date","fish"="fish","bl.s"="bl.s"))%>%
  print()


#6 plot met.rate vs amp.sum.mean
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=met.rate, colour=fish)) +
  geom_point()

