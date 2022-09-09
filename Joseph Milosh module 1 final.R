library(ggplot2)
library(tidyverse)

#dat variable containing the scales dataset
dat <- read.csv("~/experimental_methods_in_organismal_bio/Data/scales.csv")

#single line of code reporting the class of each column in the dataset
sapply(dat, class)

#single line of code reporting the dimensions of the dataset (1842 rows, 4 columns)
dim(dat)

#code that produces a summary of the number of scales punctured for each species at each location
dat %>% 
  count(species,specimen,quadrant) %>%
  print()
  
  #Code that produces a summary of the number of specimens sampled for each species (code from line 19 and 23 are the same)
dat %>% 
  count(species,specimen,name = "n.specimens") %>%
  print()

dat %>%
  group_by(species, specimen) %>%
  summarise(n=n())%>%
  print(n= 25)

#Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force verus quadrant.
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

pdf("species.quadrant.Joe.Milosh.pdf")


