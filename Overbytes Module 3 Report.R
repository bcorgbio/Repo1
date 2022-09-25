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

#1 anole.log data tibble
anole.log <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)%>%
  print()


#2 Linear models for effects of ArbPD and PH
anole.lm1 <- lm(HTotal~SVL + PH,anole.log)
anole.lm2 <- lm(HTotal~SVL+ArbPD,anole.log)

#3 Plot of residuals vs. covariates

  #adding residuals from models to tibble
anole.log.res <- anole.log %>%
  mutate(lmPHres=residuals(anole.lm1))%>%
  mutate(lmPDres=residuals(anole.lm2))

  #plots
anolePH.log.res.plot <- anole.log.res %>% 
  ggplot(aes(x=PH,y=lmPHres))+geom_point()
anolePH.log.res.plot

anolePD.log.res.plot <- anole.log.res %>% 
  ggplot(aes(x=ArbPD,y=lmPDres))+geom_point()
anolePD.log.res.plot

#4 BM Models
anole.tree <- read.tree("anole.tre")

#PGLS under BM, + perch height
pgls.BM1 <- gls(HTotal ~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

#PGLS under BM, + perch diameter
pgls.BM2 <- gls(HTotal ~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

#PGLS under BM, + perch height + perch diameter
pgls.BM3 <- gls(HTotal ~SVL+PH+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")


#5 Assessing fit

anole.aic <- AICc(pgls.BM1,pgls.BM2,pgls.BM3)
aicw(anole.aic$AICc)
#        fit     delta           w
#1 -64.77956 10.746149 0.003247185
#2 -73.81081  1.714901 0.296905077
#3 -75.52571  0.000000 0.699847738

anova(pgls.BM3)
#both covariates are significant predictors of hind-limb-SVL relationship
#both p-values are <0.05)


#6 Effect of your covariate(s) on  hindlimb residuals ofbest fitting PGLS model

pgls.BM4 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

anole.log.BMres <- anole.log.res%>%
  mutate("No.covariates"=residuals(pgls.BM4))%>%
  mutate("With.covariates"=residuals(pgls.BM3))

anole.log.BMres.long <- anole.log.BMres%>%
  dplyr::select(No.covariates,With.covariates)%>%
  pivot_longer(cols=c("No.covariates","With.covariates"))%>%
  print() 

anole.log.BMres.long %>% 
  ggplot(aes(x=name,y=value)) +geom_boxplot() + stat_summary(fun=mean, geom="point", size=3) +xlab("Model")+ylab("Residuals")

