########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)

# read summary results 
r_zoop2014<-readRDS("../Results/for_zoop_2014/summary_table_detail_version.RDS")

r_zoop2014$cvsq_real<-NA # square of CV for the real community data
r_zoop2014$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_zoop2014$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_zoop2014$phi_LdM<-NA # Loreau's variance ratio
r_zoop2014$skw_real <-NA # skewness for the real community data
r_zoop2014$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_zoop2014$phi_skw<-NA # skewness ratio
r_zoop2014$iCV<-NA # inverse of CV: stability metric
r_zoop2014$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_zoop2014$t_med<-NA
r_zoop2014$tmax_med<-NA
r_zoop2014$tmin_med<-NA
r_zoop2014$t_skw<-NA
r_zoop2014$tmax_skw<-NA
r_zoop2014$tmin_skw<-NA
r_zoop2014$t_var<-NA # variability of annual temperature
r_zoop2014$trend_t_tau<-NA # tau of Mann-Kendall trend test, for shorter time series it is difficult to see a trend
r_zoop2014$trend_t_tau_sig<-NA # is the trend significant?

# strength of linear trend estimated with parametric and non-parametric method
r_zoop2014$t.lm.slope<-NA
r_zoop2014$t.lm.slope.sig<-NA
r_zoop2014$t.sens.slope<-NA
r_zoop2014$t.sens.slope.sig<-NA # based on 95%CI

for(i in 1:nrow(r_zoop2014)){
  siteid<-r_zoop2014$newsite[i]
  nsp<-r_zoop2014$nsp[i]
  m<-readRDS(paste("../DATA/for_zoop_2014/wrangled_data/",siteid,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  
  r_zoop2014$t_med[i]<-median(m$t)
  r_zoop2014$tmax_med[i]<-median(m$tmax)
  r_zoop2014$tmin_med[i]<-median(m$tmin)
  r_zoop2014$t_skw[i]<-myskns(m$t)
  r_zoop2014$tmax_skw[i]<-myskns(m$tmax)
  r_zoop2014$tmin_skw[i]<-myskns(m$tmin)
  r_zoop2014$t_var[i]<-median(m$t)/IQR(m$t,type=7)
  
  m2<-m
  m2$year<-as.integer(rownames(m2))
  model <- lm(t ~ year, data = m2)
  r_zoop2014$t.lm.slope[i]<-unname(model$coefficients[2])
  
  tempo<-summary(model)
  tempo<-tempo$coefficients[,4]
  tempo<-unname(tempo[2])
  r_zoop2014$t.lm.slope.sig[i]<-ifelse(tempo<0.05,1,0)
  
  tempo_sens<-sens.slope(m$t, conf.level = 0.95)
  r_zoop2014$t.sens.slope[i]<-unname(tempo_sens$estimates["Sen's slope"])
  r_zoop2014$t.sens.slope.sig[i]<-ifelse(tempo_sens$p.value<0.05,1,0) # 1 means significant trend
  
  trend_mk<-mk.test(m$t)
  r_zoop2014$trend_t_tau<-unname(trend_mk$estimates["tau"]) # tau of Mann-Kendall trend test, for shorter time series it is difficult to see a trend
  r_zoop2014$trend_t_tau_sig<-ifelse(trend_mk$p.value<0.05,1,0) # 1 means significant trend
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_zoop2014$cvsq_real[i]<-df$cvsq_real
  r_zoop2014$cvsq_indep[i]<-df$cvsq_indep
  r_zoop2014$phi[i]<-df$phi
  r_zoop2014$phi_LdM[i]<-df$phi_LdM
  r_zoop2014$skw_real[i]<-df$skw_real
  r_zoop2014$skw_indep[i]<-df$skw_indep
  r_zoop2014$phi_skw[i]<-df$phi_skw
  r_zoop2014$iCV[i]<-df$iCV
  r_zoop2014$iCValt[i]<-df$iCValt
}
r_zoop2014$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_zoop2014,"../Results/for_zoop_2014/stability_metric_and_env.RDS")