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

#CPK: Awesome, very concise!!

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

#CPK: Awesome, very concise again! But see below.

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
  
#CPK: It would have been more concise to define your se function early and calculated se with mean. [-1]. Also, this ^^ is a repeat of what's above. 

pseed.sum.max <-pseed.wide%>%
  group_by(fish,speed,date)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>% 
  group_by(fish,bl.s)%>% # grouped by fish and speed
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se=SE(amp.sum))


#4 - plot the mean amp.sum vs. specific swimming speed
  #SE error bars & color by fish
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=bl.s, colour=fish)) + 
  geom_errorbar(aes(ymin=bl.s-amp.sum.se, ymax=bl.s+amp.sum.se), width=.005) +
  geom_point()

#CPK: Why do error bars include bl.s? [-1]. Aren't we concerned with SEM of amp.sum? We're interested in how amp.sum varies with bl.s, no? So we need bl.s on the x and amp.sum on the y. [-1] And let's use the pipe for this ....

pseed.sum.max%>%
  ggplot(aes(x=bl.s,y=amp.sum.mean, colour=fish)) + 
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=.005) +
  geom_point()


#5 merge pseed.met.rate with pseed.sum.max
pseed.met.rate <- read_csv("pseed.met.rate.csv")

#pseed.met.rate$bl.s <- as.numeric(as.character(pseed.met.rate$bl.s))
#pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))

#CPK: This ^^ is commented out because it messes up joining with the kinematic data


#pseed.sum.max <- pseed.sum.max %>% 
 # left_join(pseed.met.rate,by=c("date"="date","fish"="fish","bl.s"="bl.s"))%>%
  #print()

#CPK: You don't need to specify "by" if the same columns exist in the merge tibbles. And we need one value (mean) for met.rate for each speed.commented out bc it overwrite pseed.sum.max

pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate%>%
              group_by(fish,bl.s)%>%
              summarize(met.rate.mean=mean(met.rate)))


#6 plot met.rate vs amp.sum.mean
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=met.rate, colour=fish)) +
  geom_point()

#CPK: Let's use the pipe:

pseed.sum.max %>% 
  ggplot( aes(x=amp.sum.mean, y=met.rate.mean, colour=fish)) +
  geom_point()


#CPK: Really good work. Getting there!



