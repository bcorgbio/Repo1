library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

#merge tibbles
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")

#linear model
anole.lm <- lm(HTotal~SVL,anole2)
coef(anole.lm)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")
  #HTotal predicted by SVL

SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")

summary(anole.lm)

#nonlinear model
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)

summary(anole.allo)

#The AIC can be described as favoring the model
  #that provides the best fit to the data with
  #as few parameters as possible.
#AICc from the MuMIn package
anole.aic <- AICc(anole.lm,anole.allo)

#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

print(anole.aicw)
      #fit    delta         w
  #1 303.6785 2.220885 0.2477884
  #2 301.4576 0.000000 0.7522116

  #second model has lower AIC score
    #difference of <4 = roughly equivalent
    #difference of 4-8 = little support for a lesser fit
    #difference of >10 = a poor fit
#roughly equivalent models


anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")
  #visualize relationship for each ecomorph

#model including ecomorph variable
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)

anova(anole.log.eco.lm)
  #assessing the effect of a categorical variable (Ecomorph2)
  #in the context of how HTotal covaries with SVL

anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)

anole.log.aic <- AICc(anole.log.lm,anole.log.eco.lm)
aicw(anole.log.aic$AICc)
       #fit    delta            w
  #1 -34.79896 34.93394 2.595324e-08
  #2 -69.73290  0.00000 1.000000e+00
#model with ecomorph2 paramter = better fit than without

anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()
  #deviations from model (residuals) = greatest for twig & trunk-ground

p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
print(p.eco)
  #better summarization of the data, including the median residual for each ecomorph

p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
  #includes mean residual for each ecomorph

#**So far we’ve concluded that ecomorph is an important explanatory variable
  #when it comes to anole hindlimb-SVL relationships
#BUT any trait we seek to study likely covaries with phylogenetic position (evolutionary relationships)
  #it may be that any patterns we’ve uncovered thus far are the result of phylogeny, not merely ecomorph alone.

#PCMs:
  #phylogenetic comparative methods
  #any statistical operations that include the use of phylogenies (info from phylogenetic trees)
  #Phylogenetic generalized least squares (PGLS)
    #variance and covariance of the residuals are structured given a model of trait and the closeness of relationships in a phylogenetic tree

anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

#Next Q: under what process or mode does a trait (or several) evolve over a tree, from the root to the tips?
  #Brownian motion model of character evolution
    #a “random-walk” process
    #At each point in time, the value of the character walks higher or lower from the previous value
    #Over time, the sum of these changes describes the change in the value.
    #rate parameter σ2
      #How much the value can change over time
      #the higher the value, the greater the changes at each point in time.
    #resembles genetic drift model


#PGLS under BM, no ecomorph
pgls.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, w ecomorph
pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")


#PGLS under OU, no ecomorph
pgls.OU1 <- gls(HTotal ~SVL, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under OU, w, ecomorph
pgls.OU2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.OU1,pgls.OU2)
aicw(anole.phylo.aic$AICc)
      #fit     delta            w
#1 -63.66118 29.319176 4.225281e-07
#2 -92.98036  0.000000 9.827289e-01
    #best fit - phylognetically corrected regression model that includes Ecomorph2 with traits evolving under BM
#3 -57.31864 35.661714 1.772520e-08
#4 -84.89770  8.082653 1.727062e-02

#determine whether Ecomorph is a significant factor in predicting
  #hindlimb-SVL relationship in a phylogenetic context
anova(pgls.BM2)

anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))

#mutate and redefine our anole.log data to include a column for
  #phylogenetically corrected residuals and then plot them against ecomorph
p.eco.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)
print(p.eco.phylo)

#putting data tibble in longer format, with a column of both
  #residual values and another identifying to which type of
  #residual the value belongs
anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")
  #residuals condense quite a bit when we consider phylogeny




