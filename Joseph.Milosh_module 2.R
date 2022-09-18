library(ggplot2)
library(tidyverse)
library(dplyr)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble() %>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
return(f$peaks) 
}
#code above gives the peaks and critical points for each fish in each experiment; this gives us the pixel value for where the peak occurs. If we were to join this with the other data set then we would get ride of all the non-peak values

pseed.sum.max <- pseed2 %>% 
  group_by(date,fish,bl.s) %>% 
  mutate(peak=frame %in% find.peaks(frame,amp.sum)) %>% 
  filter(peak==T) %>% 
  group_by(fish,bl.s) %>% 
  summarize(amp.sum.mean=mean(amp.sum))
pseed.sum.max

#function for se
se <- function(x){
  sd(x) / sqrt(length(x))}

#code creating a column for se and visualizing the change
pseed.sum.max <- pseed.sum.max %>% 
  group_by(fish) %>% 
  mutate(amp.sum.se=se(amp.sum.mean)) %>% 
  print()

#code plotting amp.sum.mean vs specific swim speed
pseed.sum.max %>%
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish))+geom_point()+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se))

#reading new file as a tibble and merging it with pseed.sum.max
pseed.met.rate <-read_csv("pseed.met.rate.csv")
as_tibble(pseed.met.rate)
pseed.sum.max <- pseed.sum.max %>% 
  left_join(pseed.met.rate,by=c("fish"="fish")) %>% 
  print()

#metabolic power output of each fish vs mean maximum of amp.sum
pseed.sum.max %>%
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish))+geom_point()