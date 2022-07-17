########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)

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
r_swisszoo$tmax_med<-NA
r_swisszoo$tmin_med<-NA
r_swisszoo$t_skw<-NA
r_swisszoo$tmax_skw<-NA
r_swisszoo$tmin_skw<-NA


for(i in 1:nrow(r_swisszoo)){
  siteid<-r_swisszoo$newsite[i]
  nsp<-r_swisszoo$nsp[i]
  m<-readRDS(paste("../DATA/for_swisslake/wrangled_data/zooplankton/input_mat_for_tailanal_with_env_zoo_",siteid,".RDS",sep=""))
  
  r_swisszoo$t_med[i]<-median(m$t)
  r_swisszoo$tmax_med[i]<-median(m$tmax)
  r_swisszoo$tmin_med[i]<-median(m$tmin)
  r_swisszoo$t_skw[i]<-myskns(m$t)
  r_swisszoo$tmax_skw[i]<-myskns(m$tmax)
  r_swisszoo$tmin_skw[i]<-myskns(m$tmin)
  
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
}
r_swisszoo$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_swisszoo,"../Results/for_swisslake/zooplankton/stability_metric_and_env.RDS")
