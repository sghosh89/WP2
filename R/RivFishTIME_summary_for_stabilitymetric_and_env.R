########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)

# read summary results 
r_fish<-readRDS("../Results/for_RivFishTIME/summary_table_detail_version.RDS")

r_fish$cvsq_real<-NA # square of CV for the real community data
r_fish$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_fish$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_fish$phi_LdM<-NA # Loreau's variance ratio
r_fish$skw_real <-NA # skewness for the real community data
r_fish$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_fish$phi_skw<-NA # skewness ratio
r_fish$iCV<-NA # inverse of CV: stability metric
r_fish$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_fish$t_med<-NA
r_fish$tmax_med<-NA
r_fish$tmin_med<-NA
r_fish$t_skw<-NA
r_fish$tmax_skw<-NA
r_fish$tmin_skw<-NA
r_fish$t_var<-NA # variability of annual temperature

for(i in 1:nrow(r_fish)){
  siteid<-r_fish$siteid[i]
  nsp<-r_fish$nsp[i]
  m<-readRDS(paste("../DATA/for_RivFishTIME/wrangled_data/",siteid,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  
  r_fish$t_med[i]<-median(m$t)
  r_fish$tmax_med[i]<-median(m$tmax)
  r_fish$tmin_med[i]<-median(m$tmin)
  r_fish$t_skw[i]<-myskns(m$t)
  r_fish$tmax_skw[i]<-myskns(m$tmax)
  r_fish$tmin_skw[i]<-myskns(m$tmin)
  r_fish$t_var[i]<-median(m$t)/IQR(m$t,type=7)
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_fish$cvsq_real[i]<-df$cvsq_real
  r_fish$cvsq_indep[i]<-df$cvsq_indep
  r_fish$phi[i]<-df$phi
  r_fish$phi_LdM[i]<-df$phi_LdM
  r_fish$skw_real[i]<-df$skw_real
  r_fish$skw_indep[i]<-df$skw_indep
  r_fish$phi_skw[i]<-df$phi_skw
  r_fish$iCV[i]<-df$iCV
  r_fish$iCValt[i]<-df$iCValt
}
r_fish$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_fish,"../Results/for_RivFishTIME/stability_metric_and_env.RDS")
