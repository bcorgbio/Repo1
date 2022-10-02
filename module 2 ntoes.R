library(ggplot2)
library(tidyverse)
setwd("~/experimental_methods_in_organismal_bio/Scripts/Repo1")

iris <- group_by(iris,Species)
summarize(iris, mean_length=mean(Sepal.Length))

#or

iris%>%
  group_by(Species) %>% 
  summarize(mean_length=mean(Sepal.Length))

#new
iris <- group_by(iris,Species)
iris_mean <- summarise(iris,mean_length=mean(Sepal.Length))
ggplot(data=iris_mean,aes(x=Species,y=mean_length))+geom_bar(stat="identity")

#or

iris %>% 
  group_by(Species) %>% 
  summarize(mean_length=mean(Sepal.Length)) %>% 
  ggplot(aes(x=Species,y=mean_length))+geom_bar(stat="identity")

