#------------------------------------
source("./tail_analysis.R")
#----------------------------------
library(tidyverse)
#---------------------------------------
resloc<-"../Results/for_RivFishTIME/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#-----------------------------------------
# plot the sampling sites
good_TimeSeriesID_q3q4<-readRDS("../DATA/for_RivFishTIME/wrangled_data/good_TimeSeriesID_q3q4.RDS")
x<-read.csv("../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_SurveyTable.csv") # a dataframe
x_meta<-read.csv("../DATA/for_RivFishTIME/raw_data/RivFishTIME_accessed_08dec2020/1873_2_RivFishTIME_TimeseriesTable.csv")
z<-x %>% distinct(TimeSeriesID, .keep_all = TRUE)%>%dplyr::select(TimeSeriesID,UnitAbundance)
x_meta<-inner_join(z,x_meta,by="TimeSeriesID")

x_meta<-x_meta%>%filter(TimeSeriesID%in%good_TimeSeriesID_q3q4)%>%select(-X)
write.csv(x_meta,"../DATA/for_RivFishTIME/wrangled_data/metadata_RF_good_TimeSeriesID_q3q4.csv",row.names = F)

#---------------------------------------------

x_meta$lonlat<-paste(x_meta$Longitude,x_meta$Latitude,sep="_")

# extract the env data for the lonlat (from CHELSA)
env_t<-read.csv("./../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv")
env_tmax<-read.csv("./../DATA/wrangled_env_data/tasmax_annualval_extracted_lonlat.csv")
env_tmin<-read.csv("./../DATA/wrangled_env_data/tasmin_annualval_extracted_lonlat.csv")
env_t$lonlat<-paste(env_t$CENT_LONG,env_t$CENT_LAT,sep="_")
env_t<-env_t%>%select(-CENT_LONG, -CENT_LAT)%>%rename(t=meanval)
env_tmax$lonlat<-paste(env_tmax$CENT_LONG,env_tmax$CENT_LAT,sep="_")
env_tmax<-env_tmax%>%select(-CENT_LONG, -CENT_LAT)%>%rename(tmax=meanval)
env_tmin$lonlat<-paste(env_tmin$CENT_LONG,env_tmin$CENT_LAT,sep="_")
env_tmin<-env_tmin%>%select(-CENT_LONG, -CENT_LAT)%>%rename(tmin=meanval)
env_t<-left_join(env_t,env_tmax,by=c("lonlat","yr"))
env_t<-left_join(env_t,env_tmin,by=c("lonlat","yr"))

x_meta<-left_join(x_meta,env_t,by="lonlat")

write.csv(x_meta,"../DATA/for_RivFishTIME/wrangled_data/annual_tas_CHELSA_1979_2019_RF_lonlat.csv",row.names = F)

#--------------------------------------------------------------
library(maps)
wd<-map_data("world")
wd<-wd%>%filter(long<50 & lat>-50)
g1<-ggplot()+coord_fixed()+xlab("")+ylab("")
g1<-g1+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")
g1<-g1+theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             panel.background=element_rect(fill="white", colour="white"), axis.line=element_line(colour="white"),
             legend.position="none",axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
g1<-g1+geom_point(data=x_meta,aes(y=Latitude,x=Longitude),color="blue",alpha=0.1)+
  ggtitle(paste("RivFishTIME: ",nrow(x_meta)," sites: min 20 years",sep=""))
g1
ggsave(paste(resloc,"sites_on_map.pdf",sep =""),
       width = 20, height = 10, units = "cm")


#=================== create results folder for each study sites ==================

for(i in 1:length(good_TimeSeriesID_q3q4)){
  k<-paste(resloc,good_TimeSeriesID_q3q4[i],sep="")
  if(!dir.exists(k)){
    dir.create(k)
  }
}

#------------ Now compute and plot the tail stats ---------------------

for(i in 1:length(good_TimeSeriesID_q3q4)){
  siteid<-good_TimeSeriesID_q3q4[i]
  resloc_output<-paste(resloc,siteid,"/",sep="")
  
  resloc_input<-paste("../DATA/for_RivFishTIME/wrangled_data/",siteid,"/",sep="")
  df<-readRDS(paste(resloc_input,"commonspecies_timeseries.RDS",sep="")) # dataframe with species timeseries along column
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
  tempdat<-x_meta%>%filter(TimeSeriesID%in%siteid)%>%filter(yr%in%rownames(df))%>%select(yr,t,tmax,tmin)
  tempdat$tmax_n<- -tempdat$tmax
  
  # check if all TRUE
  all(rownames(df)==tempdat$yr)==T
  
  df$t<-tempdat$t
  df$tmax<-tempdat$tmax
  df$tmin<-tempdat$tmin
  df$tmax_n<-tempdat$tmax_n
  df<-df%>%select(-yr)
  
  saveRDS(df,paste(resloc_input,"input_mat_for_tailanal_with_env.RDS",sep="")) # dataframe with species timeseries along column

  #----------- tail analysis ----------------
  res<-tail_analysis(mat = df, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)
  cat("---------- i= ",i," siteid=",siteid," ----------\n")
}
