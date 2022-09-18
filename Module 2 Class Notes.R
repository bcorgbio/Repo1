#9/12 Class Notes

#cmd + shift + m = types the pipe function ( %>% )


library(tidyverse)
iris <- group_by(iris, Species)
summarize(iris, mean_length=mean(Sepal.length))

iris %>%
  group_by(Species) %>%
  summarize(mean_length=mean(Sepal.Length)) %>% 
  ggplot(aes(x=Species,y=mean_length))+geom_bar(stat="identity")

#long format = more rows (R and L listed in one columns)
#wide format = more columns (R and L listed in different columns)