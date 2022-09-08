########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)
library(trend)

# read summary results 
r_BBS<-readRDS("../Results/for_BBS/summary_table_detail_version.RDS")

r_BBS$cvsq_real<-NA # square of CV for the real community data
r_BBS$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BBS$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BBS$phi_LdM<-NA # Loreau's variance ratio
r_BBS$skw_real <-NA # skewness for the real community data
r_BBS$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BBS$phi_skw<-NA # skewness ratio
r_BBS$iCV<-NA # inverse of CV: stability metric
r_BBS$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_BBS$t_med<-NA
r_BBS$tmax_med<-NA
r_BBS$tmin_med<-NA
r_BBS$t_skw<-NA
r_BBS$tmax_skw<-NA
r_BBS$tmin_skw<-NA
r_BBS$t_var<-NA # variability of annual temperature
r_BBS$trend_t_tau<-NA # tau of Mann-Kendall trend test, for shorter time series it is difficult to see a trend
r_BBS$trend_t_tau_sig<-NA # is the trend significant?

# strength of linear trend estimated with parametric and non-parametric method
r_BBS$t.lm.slope<-NA
r_BBS$t.lm.slope.sig<-NA
r_BBS$t.sens.slope<-NA
r_BBS$t.sens.slope.sig<-NA # based on 95%CI

for(i in 1:nrow(r_BBS)){
  siteid<-r_BBS$siteid[i]
  nsp<-r_BBS$nsp[i]
  m<-readRDS(paste("../DATA/for_BBS/wrangled_data/",siteid,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  
  r_BBS$t_med[i]<-median(m$t)
  r_BBS$tmax_med[i]<-median(m$tmax)
  r_BBS$tmin_med[i]<-median(m$tmin)
  r_BBS$t_skw[i]<-myskns(m$t)
  r_BBS$tmax_skw[i]<-myskns(m$tmax)
  r_BBS$tmin_skw[i]<-myskns(m$tmin)
  r_BBS$t_var[i]<-median(m$t)/IQR(m$t,type=7)
  
  m2<-m
  m2$year<-as.integer(rownames(m2))
  model <- lm(t ~ year, data = m2)
  r_BBS$t.lm.slope[i]<-unname(model$coefficients[2])
  
  tempo<-summary(model)
  tempo<-tempo$coefficients[,4]
  tempo<-unname(tempo[2])
  r_BBS$t.lm.slope.sig[i]<-ifelse(tempo<0.05,1,0)
  
  tempo_sens<-sens.slope(m$t, conf.level = 0.95)
  r_BBS$t.sens.slope[i]<-unname(tempo_sens$estimates["Sen's slope"])
  r_BBS$t.sens.slope.sig[i]<-ifelse(tempo_sens$p.value<0.05,1,0) # 1 means significant trend
  
  trend_mk<-mk.test(m$t)
  r_BBS$trend_t_tau<-unname(trend_mk$estimates["tau"]) # tau of Mann-Kendall trend test, for shorter time series it is difficult to see a trend
  r_BBS$trend_t_tau_sig<-ifelse(trend_mk$p.value<0.05,1,0) # 1 means significant trend
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_BBS$cvsq_real[i]<-df$cvsq_real
  r_BBS$cvsq_indep[i]<-df$cvsq_indep
  r_BBS$phi[i]<-df$phi
  r_BBS$phi_LdM[i]<-df$phi_LdM
  r_BBS$skw_real[i]<-df$skw_real
  r_BBS$skw_indep[i]<-df$skw_indep
  r_BBS$phi_skw[i]<-df$phi_skw
  r_BBS$iCV[i]<-df$iCV
  r_BBS$iCValt[i]<-df$iCValt
}
r_BBS$REALM<-as.factor("Terrestrial")
##########################################################
saveRDS(r_BBS,"../Results/for_BBS/stability_metric_and_env.RDS")
