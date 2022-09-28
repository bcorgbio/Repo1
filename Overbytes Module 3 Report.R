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
    #PH covariate
anolePH.log.res.plot <- anole.log.res %>%
  ggplot(aes(x=Ecomorph2,y=lmPHres)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)
anolePH.log.res.plot

    #ArbPD covariate
anolePD.log.res.plot <- anole.log.res %>%
  ggplot(aes(x=Ecomorph2,y=lmPDres)) + geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)
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
  #pgls.BM3 has lowest AIC score

anova(pgls.BM3)
#both covariates are significant predictors of hind-limb-SVL relationship
#both p-values are <0.05)


#6 Effect of your covariate(s) on  hindlimb residuals of best fitting PGLS model
  #model with no covariates
pgls.BM4 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log.res, method = "ML")

  #mutating residuals from both models into 1 tibble
anole.log.BMres <- anole.log.res%>%
  mutate(With.covariates=residuals(pgls.BM3)) %>% 
  mutate(No.covariates=residuals(pgls.BM4))

  #plot comparing residuals
anole.log.BMres %>%
  dplyr::select(Ecomorph2,No.covariates,With.covariates)%>%
  pivot_longer(cols=c("No.covariates","With.covariates"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y")+ylab("Residuals")

