#------------------------------------
source("./tail_analysis.R")
#----------------------------------
library(tidyverse)
library(dplyr)
#---------------------------------------
resloc<-"../Results/for_BBS/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
`%notin%` <- Negate(`%in%`)
#-----------------------------------------
# prepare the metadata
fshort_list<-readRDS("../DATA/for_BBS/wrangled_data/sourcefile_list.RDS")
uroutes<-readRDS("../DATA/for_BBS/wrangled_data/unique_routes_all.RDS")
uroutes<-data.frame(Country_State_Route=uroutes)
x_meta<-read.csv("../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/routes.csv")
x_meta<-x_meta%>%unite("Country_State_Route",CountryNum,StateNum,Route,sep="_")
metadata<-inner_join(uroutes,x_meta,by="Country_State_Route")%>%
  rename(Stratum_code=Stratum)

bbs_strata1<-read.csv("../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname.csv")
bbs_strata1<-bbs_strata1%>%dplyr::select(Stratum_code=Stratum,Stratum_name=Name,Stratum_area=Area.Km2)
#bbs_strata2<-read.csv("../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/BBS_physiographic_strataname_statemap.csv")
metadata<-inner_join(metadata,bbs_strata1,by="Stratum_code")
saveRDS(metadata,"../DATA/for_BBS/wrangled_data/unique_routes_all_metadata.RDS")
#---------------------------------------------

metadata$lonlat<-paste(metadata$Longitude,metadata$Latitude,sep="_")

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
  
env_t<-left_join(env_t,metadata,by="lonlat")

write.csv(env_t,"../DATA/for_BBS/wrangled_data/annual_tas_CHELSA_1979_2019_bbs_lonlat.csv",row.names = F)

#=================== create results folder for each study sites/routes ==================

for(i in 1:nrow(uroutes)){
  k<-paste(resloc,uroutes$Country_State_Route[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:nrow(uroutes)){
  siteid<-uroutes$Country_State_Route[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../DATA/for_BBS/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"input_mat_for_tailanal.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------- analysis without raresp ----------------
  id<-which(colnames(df)=="raresp")
  if(length(id)>0){
    df<-df[,-id]
  }
  tot_target_sp<-ncol(df)
  saveRDS(tot_target_sp,paste(resloc_input,"tot_target_sp.RDS",sep="")) # nsp
  #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
  df$yr<-as.integer(rownames(df))
  df<-df%>%filter(yr%in%c(1979:2019))
  rownames(df)<-df$yr
  
  #-----------------------adding environmental variable in the matrix-----------------------------
  tempdat<-env_t%>%filter(Country_State_Route%in%siteid)%>%filter(yr%in%rownames(df))%>%dplyr::select(yr,t,tmax,tmin)
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
  #cat("---------- i= ",i," routeid=",siteid," ----------\n")
}

#zz=readRDS("../Results/for_BBS/124_11_126/NonParamStat.RDS")
#tempo<-zz$Corl - zz$Coru
#indI<-zz$posnI
#indI<-which(indI==1,arr.ind = T)
#tempo[indI]<-NA
#diag(tempo)<-NA





