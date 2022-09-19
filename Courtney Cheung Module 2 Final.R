library(tidyverse)
library(ggplot2)
library(dplyr)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

# pipe1 
pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

# mean maximum of all amp.sums
# tibble = pseed.sum.max 
# column = amp.sum.mean

pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  mutate(max.amp.sum = T ) %>% 
  mutate(amp.sum.mean = mean(max.amp.sum)) %>% 
  print()

# finding peaks
find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}
# sum of peaks 
pseed.wide_T <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

# custom function that computes SE 
SE <- function(x) {
  sd(x) / sqrt(length(x))
}
pseed.wide_SE <- pseed.wide_T %>% 
  group_by(fish,bl.s) %>% 
pseed.wide_SE$bl.s <- factor(pseed.wide_SE$bl.s)
pseed.sum.max$bl.s <- factor(pseed.sum.max$bl.s)
pseed.sum.max <- pseed.wide_SE %>% 
  left_join(pseed.sum.max,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()

# use ggplot to plot amp.sum vs specific swimming speed 
# add error bars that correspond to SE of amp.sum 
pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))
ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=bl.s, colour=fish)) + 
  geom_errorbar(aes(ymin=bl.s-amp.sum.se, ymax=bl.s+amp.sum.se), width=.005) +
  geom_point()

#read file as tibble and merge with new pseed.sum.max tibble 
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))
pseed.met.rate$bl.s <- as.numeric(as.character(pseed.met.rate$bl.s))
pseed.met.rate2 <- pseed.met.rate %>% 
  select(fish,bl.s,met.rate) %>% 
  print()
pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate2,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()
pseed.sum.max.met <- pseed.sum.max%>%
  left_join(pseed.met.rate2,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()
summarize(amp.met.rate=mean(met.rate))

# use ggplot to plot the metabolic power output
# plotting of each fish vs mean maximum of amp.sum 
pseed.sum.max %>%
ggplot(pseed.sum.max.met, aes(x=amp.sum.mean, y=met.rate, colour=fish)) +
  geom_point()