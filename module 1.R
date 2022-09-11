library(ggplot2)
library(tidyverse)
library(ggplot2)
setwd("~/Downloads")
#a dat variable containing the scales dataset.
dat <- read.csv("scales.csv") 
#line of code reporting the class of each column in the dataset
sapply(dat,class)
#line of code reporting the dimensions of the dataset
dim(dat)
head(dat)
class(dat$N)
class(dat[,1])
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
length(species)
dat$species==species[1]
dat %>% 
  count(species,quadrant) %>%
  print()
#code that produces a summary of the number of scales punctured for each species at each location
dat %>% 
  count(species,specimen,name = "n.specimens") %>%
  print()
#code that produces a summary of the number of specimens sampled for each species.
dat %>%
  group_by(species) %>%
  summarise(n = n())
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
#code creating a pdf of plot figures
}pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()