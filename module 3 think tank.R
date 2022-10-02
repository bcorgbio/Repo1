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
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

#SVL = snowt to vent (anus) length

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#C() calls the column that we are trying to adress

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")

anole.lm <- lm(HTotal+SVL~PH,anole2)

coef(anole.lm)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")

SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")

summary(anole.lm)

anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)

summary(anole.allo)

#AICc from the MuMIn package
anole.aic <- AICc(anole.lm,anole.allo)

#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

print(anole.aicw)

anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")

anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)

anova(anole.log.eco.lm)

anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)

anole.log.aic <- AICc(anole.log.lm,anole.log.eco.lm)
aicw(anole.log.aic$AICc)

anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()

p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
print(p.eco)

p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

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

anova(pgls.BM2)

anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))

p.eco.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

print(p.eco.phylo)

anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")

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

#not sure exactly what we're graphing here
# seems like it should be residuals vs. covariates somehow

#adding residuals from models to tibble
summary(anole.lm1)
anole.log.res <- anole.log %>%
  mutate(lm1res=residuals(anole.lm1))%>%
  mutate(lm2res=residuals(anole.lm2))

#point plot of model 1's residuals sorted by ecomorph
anole.log.res%>%
  ggplot(aes(Ecomorph2,lm1res))+geom_point()

#box plot of model 1's residuals sorted by ecomorph
lm1.res.plot <- anole.log.res%>%
  ggplot(aes(x=Ecomorph2,y=lm1res)) +geom_boxplot()
print(lm1.res.plot)

#point plot of model 2's residuals sorted by ecomorph
anole.log.res%>%
  ggplot(aes(Ecomorph2,lm2res))+geom_point()

#box plot of model 1's residuals sorted by ecomorph
lm2.res.plot <- anole.log.res%>%
  ggplot(aes(x=Ecomorph2,y=lm2res)) +geom_boxplot()
print(lm2.res.plot)

#box plot of PH vs. model 1's residuals
lmtest.res.plot <- anole.log.res%>%
  ggplot(aes(x=PH,y=lm1res)) +geom_boxplot()
print(lmtest.res.plot)

#box plot of PH vs. model 2's residuals
lmtest2.res.plot <- anole.log.res%>%
  ggplot(aes(x=HTotal,y=lm2res)) + geom_boxplot()
print(lmtest2.res.plot)

# I think the code below gives you a data tibble with all of the residuals and a plot of PH and PD vs their residuals!
anole.log.res <- anole.log %>% 
  mutate(lmPHres=residuals(anolePH.lm)) %>% 
  mutate(lmPDres=residuals(anolePD.lm))

print(anole.log.res)

anolePH.log.res.plot <- anole.log.res %>% 
  ggplot(aes(x=PH,y=lmPHres))+geom_point()
anolePH.log.res.plot

anolePD.log.res.plot <- anole.log.res %>% 
  ggplot(aes(x=PD,y=lmPDres))+geom_point()
anolePH.log.res.plot

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