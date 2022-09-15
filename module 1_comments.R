library(ggplot2)
library(tidyverse)
library(ggplot2)
setwd("~/Downloads")
#CPK: I suppose this is Courtney's script, though I could only tell that from the upload history on github. We needed unique and identifying filename. Also, please you git to submit. [-2]
#a dat variable containing the scales dataset.
dat <- read.csv("scales.csv") 
#line of code reporting the class of each column in the dataset
sapply(dat,class)
#line of code reporting the dimensions of the dataset
dim(dat)
#CPK: don't need the next few lines [-1]
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
#CPK: we didn't need this^^, but rather number of scales per species [-1] a la . . 
dat %>% 
  count(species) %>%
  print()


dat %>% 
  count(species,specimen,name = "n.specimens") %>%
  print()
#code that produces a summary of the number of specimens sampled for each species.
dat %>%
  group_by(species) %>%
  summarise(n = n())

#CPK: This is repeated below [-1]
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
#code creating a pdf of plot figures
}

#CPK: This pdf needed your name in its filename [-1]
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Seems the tasks weren't understood that well. Please be careful to include only what you need (and no less) according to the prompts at the end of the project descriptions