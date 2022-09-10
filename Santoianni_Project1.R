#downloading functions
library(ggplot2)
library(tidyverse)

#setting up folder access
setwd("~/Desktop/The-Overbytes") 

#naming the data
dat <- read.csv("scales.csv")

#most efficient way identifying class type
sapply(dat,class)

#getting sneak peaks
dim(dat)

#summarizing and grouping our data 
dat %>%
  group_by(species) %>%
  summarise(n = n())

#counting the specimens in species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#plotting the mean puncture force for each quadrant in each species
for(i in species){p<-dat%>%
  filter(species==i)%>%
  ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
print(p)}

#saving a PDF
pdf("Santoianni.plot.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

list.files(pattern=".pdf")
