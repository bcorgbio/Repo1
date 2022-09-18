#Mod 2 Project Report

#1 In a script named “groupname_module2.R”,
  #combine the code above so that you can
  #establish the pseed.wide data tibble.
#2 Compute the mean maximum* of all the amp.sums
  #for each specific swimming speed for each fish
  #just like we did for mean maximum amplitude of
  #each fin (i.e., the mean of all the max amplitudes
  #across each experiment for each fish). Make sure
  #this is contained in a new tibble named pseed.sum.max.
  #Call this column amp.sum.mean.
#3 Create a custom function that computes the standard
  #error of the mean (SE). [see below] and add a column
  #in your summary table pseed.sum.max for SE and call
  #it amp.sum.se.
#4 Using ggplot, plot the mean amp.sum vs. specific
  #swimming speed and add error bars that correspond
  #to the SE of amp.sum. Be sure to color your points
  #and error bars by specimen (fish).
  #point graph? geom_point
#5 Download this file, read it as a tibble and merge it
  #with the your new pseed.sum.max tibble. [see below].
#6 Use ggplot to plot the metabolic power output of each
  #fish vs. mean maximum of amp.sum.
#7 Include the code for all the tasks listed above in
  #the “groupname_module2.R” script and upload it to this link.


#setup
library(tidyverse)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#1 pseed.wide data tibble
  #add m.s and cm.s columns to pseed as pseed2
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
      #do I need print here
  #add body lengths to pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
  #add column with cm.s/bl
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
  #give L and R fins their own amp columns & sum values for amp sum
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#1a - attempt to make 1 pipe
  #think i need to change this to pseed.wide
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

find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

pseed.wide_T <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.sum.max <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum))


#making 1 pipe
pseed.sum.max <- pseed.wide%>%
  group_by(date,fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)%>%
  group_by(date,fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum))


#3 Create a custom function that computes the standard
  #error of the mean (SE). [see below] and add a column
  #in your summary table pseed.sum.max for SE and call
  #it amp.sum.se.
   # = (std dev of frequency distribution) / square root(n)
SE <- function(x) {
  sd(x) / sqrt(length(x))
}

pseed.wide_SE <- pseed.wide_T %>% 
  group_by(fish,bl.s) %>% 
  summarize(amp.sum.se=SE(amp.sum))


pseed.wide_SE$bl.s <- factor(pseed.wide_SE$bl.s)
pseed.sum.max$bl.s <- factor(pseed.sum.max$bl.s)

pseed.sum.max <- pseed.wide_SE %>% 
  left_join(pseed.sum.max,by=c("fish"="fish","bl.s"="bl.s"))%>%
  print()


#4 - plot the mean amp.sum vs. specific
  #swimming speed and add error bars that correspond
  #to the SE of amp.sum. Be sure to color your points
  #and error bars by specimen - fish.

pseed.sum.max$bl.s <- as.numeric(as.character(pseed.sum.max$bl.s))

ggplot(pseed.sum.max, aes(x=amp.sum.mean, y=bl.s, colour=fish)) + 
  geom_errorbar(aes(ymin=bl.s-amp.sum.se, ymax=bl.s+amp.sum.se), width=.005) +
  geom_point()


#5 Download this file, read it as a tibble and merge it
  #with the your new pseed.sum.max tibble. [see below].
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


#6 Use ggplot to plot the metabolic power output of each
  #fish vs. mean maximum of amp.sum.

ggplot(pseed.sum.max.met, aes(x=amp.sum.mean, y=met.rate, colour=fish)) +
  geom_point()




