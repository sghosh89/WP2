#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(tidyverse)
library(dplyr)
library(readxl)
#---------------------------------------
resloc<-"../Results/for_zoop_2014/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-----------------------------------------
#tab_zoop2014<-read.csv("../DATA/for_zoop_2014/wrangled_data/zoop2014_starting_table.csv")
good_LakeID<-tab_zoop2014$STUDY_ID

#=================== create results folder for each study sites ==================

for(i in 1:length(good_LakeID)){
  k<-paste(resloc,good_LakeID[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:length(good_LakeID)){
  siteid<-good_LakeID[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../DATA/for_zoop_2014/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"inputmat_for_tailanal.RDS",sep="")) # dataframe with species-group timeseries along column
  tot_target_sp<-ncol(df)
  saveRDS(tot_target_sp,paste(resloc_input,"tot_target_sp.RDS",sep="")) # nsp
  #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
  df$yr<-as.integer(rownames(df))
  df<-df%>%filter(yr%in%c(1979:2019))
  rownames(df)<-df$yr
  #-----------------------adding environmental variable in the matrix-----------------------------
  tempdat<-env_zoop2014%>%filter(STUDY_ID%in%siteid)%>%filter(yr%in%rownames(df))%>%dplyr::select(yr,t,tmax,tmin)
  tempdat$tmax_n<- -tempdat$tmax
  
  # check if all TRUE
  all(rownames(df)==tempdat$yr)==T
  
  df$t<-tempdat$t
  df$tmax<-tempdat$tmax
  df$tmin<-tempdat$tmin
  df$tmax_n<-tempdat$tmax_n
  df<-df%>%dplyr::select(-yr)
  
  saveRDS(df,paste(resloc_input,"input_mat_for_tailanal_with_env.RDS",sep="")) # dataframe with species timeseries along column
  #-------------------------------------------------------------------
  res<-tail_analysis(mat = df, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," siteid=",siteid," ----------\n")
}



