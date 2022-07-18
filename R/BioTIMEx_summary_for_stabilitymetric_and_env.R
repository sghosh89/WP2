########################################################################################
# This code will make a table of stability metric and summarized env stats given 
# a community timeseries with env data for the same time-span
########################################################################################

source("./get_stability_metric.R")
library(tidyverse)

# read summary results 
r_BTx<-readRDS("../Results/for_BioTIMEx/summary_table_detail_version.RDS")

r_BTx$cvsq_real<-NA # square of CV for the real community data
r_BTx$cvsq_indep<-NA # square of CV of the community if all the sp. behave independently
r_BTx$phi<-NA  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
r_BTx$phi_LdM<-NA # Loreau's variance ratio
r_BTx$skw_real <-NA # skewness for the real community data
r_BTx$skw_indep<-NA # skewness of the community if all the sp. behave independently
r_BTx$phi_skw<-NA # skewness ratio
r_BTx$iCV<-NA # inverse of CV: stability metric
r_BTx$iCValt <-NA # inverse of CV alternative for skewed dist.: stability metric

# some env stats: median and skewness of annual t, tmax, tmin distribution for the study years
r_BTx$t_med<-NA
r_BTx$tmax_med<-NA
r_BTx$tmin_med<-NA
r_BTx$t_skw<-NA
r_BTx$tmax_skw<-NA
r_BTx$tmin_skw<-NA


for(i in 1:nrow(r_BTx)){
  siteid<-r_BTx$siteid[i]
  nsp<-r_BTx$nsp[i]
  
  resloci<-paste("../DATA/for_BioTIMEx/wrangled_data/",r_BTx$STUDY_ID[i],"/",sep="")
  
  # to get number of years used for the study which also includes env variables
  if(r_BTx$STUDY_ID[i]==r_BTx$newsite[i]){
    m<-readRDS(paste(resloci,"input_mat_for_tailanal_with_env.RDS",sep=""))
  }else{
    m<-readRDS(paste(resloci,"input_mat_for_tailanal_with_env_",r_BTx$STUDY_ID[i],"_",r_BTx$newsite[i],".RDS",sep=""))
  }
  
  r_BTx$t_med[i]<-median(m$t)
  r_BTx$tmax_med[i]<-median(m$tmax)
  r_BTx$tmin_med[i]<-median(m$tmin)
  r_BTx$t_skw[i]<-myskns(m$t)
  r_BTx$tmax_skw[i]<-myskns(m$tmax)
  r_BTx$tmin_skw[i]<-myskns(m$tmin)
  
  # now extract only species time-series (without env variable)
  m<-m[,1:nsp]
  
  df<-get_stability_metric(m=m)
  r_BTx$cvsq_real[i]<-df$cvsq_real
  r_BTx$cvsq_indep[i]<-df$cvsq_indep
  r_BTx$phi[i]<-df$phi
  r_BTx$phi_LdM[i]<-df$phi_LdM
  r_BTx$skw_real[i]<-df$skw_real
  r_BTx$skw_indep[i]<-df$skw_indep
  r_BTx$phi_skw[i]<-df$phi_skw
  r_BTx$iCV[i]<-df$iCV
  r_BTx$iCValt[i]<-df$iCValt
}
##########################################################
saveRDS(r_BTx,"../Results/for_BioTIMEx/stability_metric_and_env.RDS")
