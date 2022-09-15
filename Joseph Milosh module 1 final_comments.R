library(ggplot2)
library(tidyverse)
setwd("~/experimental_methods_in_organismal_bio/Scripts/Repo1")

#CPK: No need set the working directory when working in an R project.


#dat variable containing the scales dataset
#dat <- read.csv("~/experimental_methods_in_organismal_bio/Data/scales.csv")

#CPK: How could I (or anyone else) load this file unluss we have this same directory? [-1]. Commented and changed so this will run and load the "scales.csv" file in the directory

dat <- read.csv("scales.csv")


#single line of code reporting the class of each column in the dataset
sapply(dat, class)

#single line of code reporting the dimensions of the dataset (1842 rows, 4 columns)
dim(dat)

#code that produces a summary of the number of scales punctured for each species at each location
dat %>% 
  count(species,quadrant) %>%
  print()
  
#CPK: We didn't need scales for each species at location, just number per species [-1], like . . . 

dat %>% 
  count(species) %>%
  print()

#Code that produces a summary of the number of specimens sampled for each species (code from line 19 and 23 are the same)
dat %>% 
  count(species,specimen,name = "n.specimens") %>%
  print()

#CPK: This isn't the number of specimens per species [-1]. We needed . . . 


dat %>% 
  count(species,specimen) %>%
  count(species,name = "n.specimens")


#Code that produces 6 figures, one for each species that includes a boxplot of puncture force verus quadrant

#CPK: this is repeated below. Don't need [-1]
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

#Code creating a pdf of plot figures
pdf("species.quadrant.joe.milosh.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: Solid work. Just be careful to produce only what we need and exactly what's asked for.