#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
library(dplyr)
#---------------------------------------
resloc<-"../Results/for_swisslake/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
resloc<-"../Results/for_swisslake/zooplankton/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------

# ============= for lake Zurich =======================
lakenamelist<-c("ZH","LU","SEM","HAL","GRE","BAL")

for(i in c(1:length(lakenamelist))){
  
  lakename<-lakenamelist[i]
  
  resloc_output<-paste("../Results/for_swisslake/zooplankton/zoo_",lakename,"/",sep="")
  if(!dir.exists(resloc_output)){
    dir.create(resloc_output)
  }
  
  resloc_input<-"../DATA/for_swisslake/wrangled_data/zooplankton/"
  df<-readRDS(paste(resloc_input,"input_mat_for_tailanal_zoo_",lakename,".RDS",sep=""))
  tot_target_sp<-ncol(df)
  saveRDS(tot_target_sp,paste(resloc_input,"tot_target_sp_",lakename,".RDS",sep="")) # nsp
  
  #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
  df$yr<-as.integer(rownames(df))
  df<-df%>%filter(yr%in%c(1979:2019))
  rownames(df)<-df$yr
  nrow(df)>=20 # check
  #-----------------------adding environmental variable in the matrix-----------------------------
  tempdat<-env_swisszoo%>%filter(newsite==lakename)%>%filter(yr%in%rownames(df))%>%dplyr::select(yr,t,tmax,tmin)
  tempdat$tmax_n<- -tempdat$tmax
  
  # check if all TRUE
  all(rownames(df)==tempdat$yr)==T
  
  df$t<-tempdat$t
  df$tmax<-tempdat$tmax
  df$tmin<-tempdat$tmin
  df$tmax_n<-tempdat$tmax_n
  df<-df%>%dplyr::select(-yr)
  
  saveRDS(df,paste(resloc_input,"input_mat_for_tailanal_with_env_zoo_",lakename,".RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- tail analysis ----------------
  res<-tail_analysis(mat = df, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)
}
