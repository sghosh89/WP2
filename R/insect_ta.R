#------------------------------------
source("tail_analysis.R")
#----------------------------------
library(dplyr)
library(tidyverse)
#--------- read data: don't overwrite this variables ---------------------------------
#meta_Roel<-read.csv("../DATA/for_insectRoel/20yrFreshwater_Metadata.csv")
#data_Roel<-readRDS("../DATA/for_insectRoel/20yrFreshwaterData 202106.rds")

xtbl<-data_Roel%>%dplyr::distinct(Rank,.keep_all=T)
xtbl<-xtbl[order(xtbl$Rank),]
xtbl<-xtbl%>%dplyr::select(Rank,Level)
#---------------------------------------
resloc<-"../Results/for_insectRoel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}

Datasource_ID<-readRDS("../DATA/for_insectRoel/wrangled_data/Datasource_ID.RDS")
for(i in 1:length(Datasource_ID)){
  did<-paste(resloc,Datasource_ID[i],"/",sep="")
  if(!dir.exists(did)){
    dir.create(did)
  }
  pidlist<-readRDS(paste("../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/Plot_ID_list.RDS",sep=""))
  badpidlist<-readRDS(paste("../DATA/for_insectRoel/wrangled_data/",Datasource_ID[i],"/bad_pidlist.RDS",sep=""))
  goodpidlist<-setdiff(pidlist,badpidlist)
  saveRDS(goodpidlist,paste("../Results/for_insectRoel/",Datasource_ID[i],"/goodpidlist.RDS",sep=""))
  for(j in 1:length(goodpidlist)){
    pid<-paste(did,goodpidlist[j],"/",sep="")
    if(!dir.exists(pid)){
      dir.create(pid)
    }
  }
}

#--------------------------get env stats for insect database-------------------------------------------
meta_Roel2<-meta_Roel
meta_Roel2$lonlat<-paste(meta_Roel$Longitude,meta_Roel$Latitude,sep="_")

# extract the env data for the lonlat (from CHELSA)
env_t<-read.csv("./../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv")
env_tmax<-read.csv("./../DATA/wrangled_env_data/tasmax_annualval_extracted_lonlat.csv")
env_tmin<-read.csv("./../DATA/wrangled_env_data/tasmin_annualval_extracted_lonlat.csv")
env_t$lonlat<-paste(env_t$CENT_LONG,env_t$CENT_LAT,sep="_")
env_t<-env_t%>%dplyr::select(-CENT_LONG, -CENT_LAT)%>%rename(t=meanval)
env_tmax$lonlat<-paste(env_tmax$CENT_LONG,env_tmax$CENT_LAT,sep="_")
env_tmax<-env_tmax%>%dplyr::select(-CENT_LONG, -CENT_LAT)%>%rename(tmax=meanval)
env_tmin$lonlat<-paste(env_tmin$CENT_LONG,env_tmin$CENT_LAT,sep="_")
env_tmin<-env_tmin%>%dplyr::select(-CENT_LONG, -CENT_LAT)%>%rename(tmin=meanval)
env_t<-left_join(env_t,env_tmax,by=c("lonlat","yr"))
env_t<-left_join(env_t,env_tmin,by=c("lonlat","yr"))

env_t<-left_join(meta_Roel2,env_t,by="lonlat")

write.csv(env_t,"../DATA/for_insectRoel/wrangled_data/annual_tas_CHELSA_1979_2019_insect_lonlat.csv",row.names = F)

#----------- Now compute and plot the tail stats ---------------------

for(i in 1:length(Datasource_ID)){
  did<-Datasource_ID[i]
  goodpidlist<-readRDS(paste("../Results/for_insectRoel/",did,"/goodpidlist.RDS",sep=""))
  for(j in 1:length(goodpidlist)){
    pid<-goodpidlist[j]
    resloc_output<-paste(resloc,did,"/",pid,"/",sep="")
    resloc_input<-paste("../DATA/for_insectRoel/wrangled_data/",did,"/",pid,"/",sep="")
    
    df<-readRDS(paste(resloc_input,"commonspecies_timeseries.RDS",sep="")) # dataframe with species timeseries along column
    #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
    df$yr<-as.integer(rownames(df))
    df<-df%>%filter(yr%in%c(1979:2019))
    rownames(df)<-df$yr
    
    tot_target_sp<-ncol(df)
    saveRDS(tot_target_sp,paste(resloc_input,"tot_target_sp.RDS",sep="")) # nsp
    
    #-----------------------adding environmental variable in the matrix-----------------------------
    tempdat<-env_t%>%filter(Datasource_ID%in%did)%>%filter(Plot_ID%in%pid)%>%
      filter(yr%in%rownames(df))%>%dplyr::select(yr,t,tmax,tmin)
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
    
    if(ncol(df)>=2){
      res<-tail_analysis(mat = df, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)
      cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," ----------\n")
    }else{
      cat("------- i= ",i," j=",j," did =", did, " pid = ",pid," not enough sp present ----------\n")
    }
    
  }
}

