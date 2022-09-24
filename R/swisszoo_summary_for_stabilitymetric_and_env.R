########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)
library(trend)
library(adiv)

# read summary results 
r_swisszoo<-readRDS("../Results/for_swisslake/zooplankton/summary_table_detail_version.RDS")

r_swisszoo$cvsq_real<-NA # square of CV for the real community data
r_swisszoo$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_swisszoo$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_swisszoo$phi_LdM<-NA # Loreau's variance ratio
r_swisszoo$skw_real <-NA # skewness for the real community data
r_swisszoo$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_swisszoo$phi_skw<-NA # skewness ratio
r_swisszoo$iCV<-NA # inverse of CV: stability metric
r_swisszoo$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_swisszoo$t_med<-NA
#r_swisszoo$tmax_med<-NA
#r_swisszoo$tmin_med<-NA
r_swisszoo$t_skw<-NA
#r_swisszoo$tmax_skw<-NA
#r_swisszoo$tmin_skw<-NA
r_swisszoo$t_var<-NA # variability of annual temperature
r_swisszoo$t_med_celcius<-NA
r_swisszoo$t_skw_celcius<-NA
r_swisszoo$t_var_celcius<-NA
# strength of linear trend estimated with parametric and non-parametric method
r_swisszoo$t.lm.slope<-NA
r_swisszoo$t.lm.slope.sig<-NA
r_swisszoo$t.sens.slope<-NA
r_swisszoo$t.sens.slope.sig<-NA # based on 95%CI
r_swisszoo$t.lm.slope.celcius<-NA
r_swisszoo$t.lm.slope.sig.celcius<-NA
r_swisszoo$t.sens.slope.celcius<-NA
r_swisszoo$t.sens.slope.sig.celcius<-NA # based on 95%CI

# some diversity index calculation (https://search.r-project.org/CRAN/refmans/adiv/html/specieseve.html)
r_swisszoo$GiniSimpson<-NA
r_swisszoo$Simpson<-NA
r_swisszoo$Shannon<-NA
r_swisszoo$Heip<-NA
r_swisszoo$McIntosh<-NA
r_swisszoo$SmithWilson<-NA
r_swisszoo$Pielou<-NA # this is Shannon/log(richness)

for(i in 1:nrow(r_swisszoo)){
  siteid<-r_swisszoo$newsite[i]
  nsp<-r_swisszoo$nsp[i]
  m<-readRDS(paste("../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tailanal_with_env_zoo_",siteid,".RDS",sep=""))
  m$t_in_celcius<-(m$t/10)-273.15
  saveRDS(m,paste("../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tailanal_with_env_celcius_zoo_",siteid,".RDS",sep=""))
  
  # Everything in K/10 scale
  r_swisszoo$t_med[i]<-median(m$t)
  r_swisszoo$t_skw[i]<-myskns(m$t)
  r_swisszoo$t_var[i]<-median(m$t)/IQR(m$t,type=7)
  #--------------------------
  # Now everything in celcius scale
  r_swisszoo$t_med_celcius[i]<-median(m$t_in_celcius)
  r_swisszoo$t_skw_celcius[i]<-myskns(m$t_in_celcius)
  r_swisszoo$t_var_celcius[i]<-abs(median(m$t_in_celcius))/IQR(m$t_in_celcius,type=7)
  # note celcius scale temp can be negative, so take abs value for t_var_celcius
  #----------------------------
  
  m2<-m
  m2$year<-as.integer(rownames(m2))
  model <- lm(t ~ year, data = m2)
  r_swisszoo$t.lm.slope[i]<-unname(model$coefficients[2])
  
  tempo<-summary(model)
  tempo<-tempo$coefficients[,4]
  tempo<-unname(tempo[2])
  r_swisszoo$t.lm.slope.sig[i]<-ifelse(tempo<0.05,1,0)
  
  model2<- lm(t_in_celcius ~ year, data = m2)
  r_swisszoo$t.lm.slope.celcius[i]<-unname(model2$coefficients[2])
  
  tempo<-summary(model2)
  tempo<-tempo$coefficients[,4]
  tempo<-unname(tempo[2])
  r_swisszoo$t.lm.slope.sig.celcius[i]<-ifelse(tempo<0.05,1,0)
  
  tempo_sens<-sens.slope(m$t, conf.level = 0.95)
  r_swisszoo$t.sens.slope[i]<-unname(tempo_sens$estimates["Sen's slope"])
  r_swisszoo$t.sens.slope.sig[i]<-ifelse(tempo_sens$p.value<0.05,1,0) # 1 means significant trend
  
  tempo_sens<-sens.slope(m$t_in_celcius, conf.level = 0.95)
  r_swisszoo$t.sens.slope.celcius[i]<-unname(tempo_sens$estimates["Sen's slope"])
  r_swisszoo$t.sens.slope.sig.celcius[i]<-ifelse(tempo_sens$p.value<0.05,1,0) # 1 means significant trend
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_swisszoo$cvsq_real[i]<-df$cvsq_real
  r_swisszoo$cvsq_indep[i]<-df$cvsq_indep
  r_swisszoo$phi[i]<-df$phi
  r_swisszoo$phi_LdM[i]<-df$phi_LdM
  r_swisszoo$skw_real[i]<-df$skw_real
  r_swisszoo$skw_indep[i]<-df$skw_indep
  r_swisszoo$phi_skw[i]<-df$phi_skw
  r_swisszoo$iCV[i]<-df$iCV
  r_swisszoo$iCValt[i]<-df$iCValt
  m1<-rbind(m,apply(m,FUN=sum,MARGIN = 2))
  m1<-tail(m1,1)
  divind<-adiv::specieseve(m1, method = "full", tol = 1e-8)
  divind<-as.data.frame(divind)
  r_swisszoo$GiniSimpson[i]<- divind$GiniSimpson
  r_swisszoo$Simpson[i]<- divind$Simpson
  r_swisszoo$Shannon[i]<- divind$Shannon
  r_swisszoo$Heip[i]<- divind$Heip
  r_swisszoo$McIntosh[i]<- divind$McIntosh
  r_swisszoo$SmithWilson[i]<- divind$SmithWilson
  r_swisszoo$Pielou[i]<- divind$Shannon/log(r_swisszoo$nsp[i])
}
r_swisszoo$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_swisszoo,"../Results/for_swisslake/zooplankton/stability_metric_and_env.RDS")
