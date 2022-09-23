#1 Combine the code above so that you can establish the anole.log data tibble.
#2 Using the log-transformed data, construct two simple linear models that
  #assess the effect of perch diameter and height by including these as
  #covariates in your models. Be sure to use + notation rather than *,
  #assuming there is no interaction (there isn’t, trust me!).
#3 Explore how both perch diameter and height effect the hindlimb-SVL
  #relationship by plotting the residuals of your simple linear models
  #against these covariates. This will require mutating a data tibble to
  #include residuals from both models. Please produce two separate plots.
#4 Under a BM model of trait evolution and using the tree provided,
  #construct phylogenetic least squares models of the hindlimb-SVL
  #relationships that include the unique combinations of these two covariates,i.e,
    #* A PGLS model with the hindlimb-SVL relationship + perch height
    #* A PGLS model with the hindlimb-SVL relationship + perch diameter
    #* A PGSL model with the hindlimb-SVL relationship + perch height + perch diameter
#5 Assess the fit of each of these three models using AICc and AICw and
  #comment on (with comments in the script) whether one or both of the
  #covariates is a significant predictor of hinglimb length in a phylogenetic
  #context.
#6 Produce a plot of your own design that concisely visualizes the
  #effect of your covariate(s) on the hindlimb residuals of the best
  #fitting PGLS model.

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

#1 Combine the code above so that you can establish the anole.log data tibble.
anole.log <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)%>%
  print()


#2 Using the log-transformed data, construct two simple linear models that
  #assess the effect of perch diameter and height by including these as
  #covariates in your models. Be sure to use + notation rather than *,
  #assuming there is no interaction (there isn’t, trust me!).

anole.lm1 <- lm(HTotal~SVL + PH,anole.log)
anole.lm2 <- lm(HTotal~SVL+ArbPD,anole.log)

#3 Explore how both perch diameter and height effect the hindlimb-SVL
  #relationship by plotting the residuals of your simple linear models
  #against these covariates. This will require mutating a data tibble to
  #include residuals from both models. Please produce two separate plots.

summary(anole.lm1)
anole.log.res <- anole.log %>%
  mutate(lm1res=residuals(anole.lm1))%>%
  mutate(lm2res=residuals(anole.lm2))

anole.log.res%>%
  ggplot(aes(Ecomorph2,lm1res))+geom_point()

lm1.res.plot <- anole.log.res%>%
  ggplot(aes(x=Ecomorph2,y=lm1res)) +geom_boxplot()
print(lm1.res.plot)

anole.log.res%>%
  ggplot(aes(Ecomorph2,lm2res))+geom_point()

lm2.res.plot <- anole.log.res%>%
  ggplot(aes(x=Ecomorph2,y=lm2res)) +geom_boxplot()
print(lm2.res.plot)

lmtest.res.plot <- anole.log.res%>%
  ggplot(aes(x=PH,y=lm1res)) +geom_boxplot()
print(lmtest.res.plot)

lmtest2.res.plot <- anole.log.res%>%
  ggplot(aes(x=HTotal,y=lm1res)) + geom_boxplot()
print(lmtest2.res.plot)

#4 Under a BM model of trait evolution and using the tree provided,
  #construct phylogenetic least squares models of the hindlimb-SVL
  #relationships that include the unique combinations of these two covariates,i.e,
    #* A PGLS model with the hindlimb-SVL relationship + perch height
    #* A PGLS model with the hindlimb-SVL relationship + perch diameter
    #* A PGSL model with the hindlimb-SVL relationship + perch height + perch diameter

#PGLS under BM, + perch height
pgls.BM1 <- gls(HTotal ~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

#PGLS under BM, + perch diameter
pgls.BM2 <- gls(HTotal ~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

#PGLS under BM, + perch height + perch diameter
pgls.BM3 <- gls(HTotal ~SVL+PH+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")


#5 Assess the fit of each of these three models using AICc and AICw and
  #comment on (with comments in the script) whether one or both of the
  #covariates is a significant predictor of hind-limb length in a phylogenetic
  #context.

anole.aic <- AICc(pgls.BM1,pgls.BM2,pgls.BM3)
aicw(anole.aic$AICc)
#        fit     delta           w
#1 -64.77956 10.746149 0.003247185
#2 -73.81081  1.714901 0.296905077
#3 -75.52571  0.000000 0.699847738

anova(pgls.BM3)
  #both covariates are significant predictors of hind-limb-SVL relationship
    #both p-values are <0.05)


#6 Produce a plot of your own design that concisely visualizes the
  #effect of your covariate(s) on the hindlimb residuals of the best
  #fitting PGLS model.

pgls.BM4 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

anole.log.BMres <- anole.log.res%>%
  mutate(BM3.res=residuals(pgls.BM3))%>%
  mutate(BM4.res=residuals(pgls.BM4))

anole.log.BMres.long <- anole.log.BMres%>%
  dplyr::select(BM3.res,BM4.res)%>%
  pivot_longer(cols=c("BM3.res","BM4.res"))%>%
  print()
  
    #we think this might be something like the the 2 facet plot at the end
      #of the project examples with residuals on the y-axis, 1 plot of the
      #residuals from the model with both covariates and the other with the
      #residuals from a model with no covariates
      
  
  
  
  
  