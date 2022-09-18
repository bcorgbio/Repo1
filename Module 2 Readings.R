#Chap 5
library(tidyverse)
library(nycflights13)

#filter()
  #subset observations based on values
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
  #wrapping the whole line tells R to print as well as save

1 / 49 * 49 == 1
  #FALSE (b/c numbers are approximations)
near(1 / 49 * 49, 1)
  #TRUE

filter(flights, month == 11 | month == 12)
(nov_dec <- filter(flights, month %in% c(11, 12)))

#!(x & y) is the same as
  #!x | !y, and !(x | y) is the same as
  #!x & !y
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#tell R to return TRUE & NA values
df <- tibble(x = c(1, NA, 3))
  #TRUE
filter(df, x > 1)
  #TRUE and NA
filter(df, is.na(x) | x > 1)

#arrival delay of 2+ hours
filter(flights, arr_delay > 2)

#flew to Houston
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, dest %in% c("IAH", "HOU"))

#United, American, or Delta
filter(flights, carrier %in% c("AA", "UA", "DL"))

#departed in july, august, or sept
filter(flights, month %in% c(8, 9, 10))

#arrived >2 hours late, but didn't leave late
filter(flights, arr_delay > 60 & dep_delay <= 0)

#delayed by 1+ hours, made up over 30 min in flight
filter(flights, dep_delay >= 60 & arr_delay < 30)

#departed bw midnight and 6am
filter(flights, dep_time >= 0 & dep_time <= 600)
filter(flights, between(dep_time, 0, 600))

#missing dep_time
filter(flights, is.na(dep_time))
  #cancelled flights

NA^0
  #not missing because any number ^0 = 1

NA | TRUE
  #not missing because one condition is TRUE

FALSE & NA
  #not missing (but false) bc F is not TRUE

NA*0

#arrange()
arrange(flights, year, month, day)
  #each successive column is used to break
    #ties in previous

arrange(flights, desc(dep_delay))

df <- tibble(x=c(5, 2, NA))
arrange(df, x)
  #NA sorted at bottom

(arrange(df, desc(is.na(x))))
  #NA sorted at top

#sort by most delayed flights & which left earliest
arrange(flights, desc(dep_delay), dep_time)

#sort by distance
arrange(flights, distance)
arrange(flights, desc(distance))

#select()
select(flights, year, month, day)
select(flights, year:day)
select(flights, starts_with("dep"))
select(flights, ends_with("delay"))
select(flights, contains("delay"))
#other helper functions:
  #matches("(.)\\1")
  #num_range("x", 1:3) = matches x1, x1, and x3
select(flights, time_hour, air_time, everything())
  #moves columns to beginning of data frame

select(flights, time_hour, time_hour)
  #column can only be called once

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))
  #any_of() allows NAs

select(flights, contains("TIME"))


#mutate()
  #any functions must be vectorized
    #(take vector as input)
    #(and give same length as output)
flights_sml <- select(flights,
    year:day,
    ends_with("delay"),
    distance,
    air_time)

mutate(flights_sml,
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60)
  #adds new columns

transmute(flights,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours)
  #keeps only new columns

#lead() and lag() refer to leading and lagging values
(x <- 1:10)
lag(x)
lead(x)

#cummulative
cumsum(x)
cumean(x)

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
  #ranks smallest numbers 1st
min_rank(desc(y))
  #ranks largest numbers 1st

#summarise()
summarise(flights, delay = mean(dep_delay, na.rm=T))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm=T))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dest != "HNL")
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

#sum(!is.na(x))
  #count the number of non-missing values

#n_distinct(x)
  #count the number of distinct (unique) values


#Project Examples
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
  #joining speed & vol

pseed.bl%>%
  print()

pseed2%>%
  select(fish)%>%
  unique()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()

pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

library(features)

exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1

fget(f1)

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

pseed2 %>%
  filter(fin=="R")

pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 









