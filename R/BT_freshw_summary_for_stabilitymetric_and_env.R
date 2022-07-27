########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)

# read summary results 
r_BT_freshw<-readRDS("../Results/for_BioTIME/Freshwater_plotlevel/summary_table.RDS")

r_BT_freshw$cvsq_real<-NA # square of CV for the real community data
r_BT_freshw$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BT_freshw$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BT_freshw$phi_LdM<-NA # Loreau's variance ratio
r_BT_freshw$skw_real <-NA # skewness for the real community data
r_BT_freshw$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BT_freshw$phi_skw<-NA # skewness ratio
r_BT_freshw$iCV<-NA # inverse of CV: stability metric
r_BT_freshw$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_BT_freshw$t_med<-NA
r_BT_freshw$tmax_med<-NA
r_BT_freshw$tmin_med<-NA
r_BT_freshw$t_skw<-NA
r_BT_freshw$tmax_skw<-NA
r_BT_freshw$tmin_skw<-NA
r_BT_freshw$t_var<-NA # variability of annual temperature

for(i in 1:nrow(r_BT_freshw)){
  siteid<-r_BT_freshw$STUDY_ID[i]
  newsite<-r_BT_freshw$newsite[i]
  nsp<-r_BT_freshw$nsp[i]
  if(siteid==newsite){
    m<-readRDS(paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",siteid,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  }else{
    m<-readRDS(paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",siteid,"/",newsite,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  }
  
  r_BT_freshw$t_med[i]<-median(m$t)
  r_BT_freshw$tmax_med[i]<-median(m$tmax)
  r_BT_freshw$tmin_med[i]<-median(m$tmin)
  r_BT_freshw$t_skw[i]<-myskns(m$t)
  r_BT_freshw$tmax_skw[i]<-myskns(m$tmax)
  r_BT_freshw$tmin_skw[i]<-myskns(m$tmin)
  r_BT_freshw$t_var[i]<-median(m$t)/IQR(m$t,type=7)
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_BT_freshw$cvsq_real[i]<-df$cvsq_real
  r_BT_freshw$cvsq_indep[i]<-df$cvsq_indep
  r_BT_freshw$phi[i]<-df$phi
  r_BT_freshw$phi_LdM[i]<-df$phi_LdM
  r_BT_freshw$skw_real[i]<-df$skw_real
  r_BT_freshw$skw_indep[i]<-df$skw_indep
  r_BT_freshw$phi_skw[i]<-df$phi_skw
  r_BT_freshw$iCV[i]<-df$iCV
  r_BT_freshw$iCValt[i]<-df$iCValt
}
r_BT_freshw$REALM<-as.factor("Freshwater")
##########################################################
saveRDS(r_BT_freshw,"../Results/for_BioTIME/Freshwater_plotlevel/stability_metric_and_env.RDS")