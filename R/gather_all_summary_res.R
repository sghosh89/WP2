# Now we will gather all summary table we got from analyzing different databases

library(tidyverse)
library(dplyr)
#=====================================================================
# gather stability metric summary for all data you have analyzed
`%notin%` <- Negate(`%in%`)
md<-read.csv("../DATA/metadata_summary.csv")

#----------------------------- for BioTIME, freshwater -----------------------------------------------------
df<-md%>%filter(source=="BioTIME" & REALM=="Freshwater")%>%filter(TAXA%in%c("fish","freshwater invertebrates"))
length(unique(df$newsite))==nrow(df) #This should be TRUE

df<-df%>%dplyr::select(STUDY_ID,TAXA,ORGANISMS)%>%distinct(STUDY_ID,.keep_all = T)
sm_BT_freshw<-readRDS("../Results/for_BioTIME/Freshwater_plotlevel/stability_metric_and_env.RDS")
sm_BT_freshw$source<-"BioTIME"
df$STUDY_ID<-as.integer(df$STUDY_ID)
sm_BT_freshw<-left_join(sm_BT_freshw, df,by = "STUDY_ID")

sm_BT_freshw$LONGITUDE<-NA
sm_BT_freshw$LATITUDE<-NA

id<-which(sm_BT_freshw$STUDY_ID==57)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/mylonlat.csv")
sm_BT_freshw$LONGITUDE[id]<-mylonlat$LONGITUDE
sm_BT_freshw$LATITUDE[id]<-mylonlat$LATITUDE

id<-which(sm_BT_freshw$STUDY_ID==478)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/478/mylonlat.csv")
sm_BT_freshw$LONGITUDE[id]<-mylonlat$LONGITUDE
sm_BT_freshw$LATITUDE[id]<-mylonlat$LATITUDE

id<-which(sm_BT_freshw$STUDY_ID==430)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/430/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_430_basin_",mylonlat$basin,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_freshw<-left_join(sm_BT_freshw,mylonlat,by="newsite")
sm_BT_freshw$LONGITUDE<-dplyr::coalesce(sm_BT_freshw$LONGITUDE.x,sm_BT_freshw$LONGITUDE.y)
sm_BT_freshw$LATITUDE<-dplyr::coalesce(sm_BT_freshw$LATITUDE.x,sm_BT_freshw$LATITUDE.y)
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_freshw$STUDY_ID==229)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/229/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_229_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_freshw<-left_join(sm_BT_freshw,mylonlat,by="newsite")
sm_BT_freshw$LONGITUDE<-dplyr::coalesce(sm_BT_freshw$LONGITUDE.x,sm_BT_freshw$LONGITUDE.y)
sm_BT_freshw$LATITUDE<-dplyr::coalesce(sm_BT_freshw$LATITUDE.x,sm_BT_freshw$LATITUDE.y)
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_freshw$STUDY_ID==238)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/238/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_238_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_freshw<-left_join(sm_BT_freshw,mylonlat,by="newsite")
sm_BT_freshw$LONGITUDE<-dplyr::coalesce(sm_BT_freshw$LONGITUDE.x,sm_BT_freshw$LONGITUDE.y)
sm_BT_freshw$LATITUDE<-dplyr::coalesce(sm_BT_freshw$LATITUDE.x,sm_BT_freshw$LATITUDE.y)
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_freshw$STUDY_ID==247)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_247_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_freshw<-left_join(sm_BT_freshw,mylonlat,by="newsite")
sm_BT_freshw$LONGITUDE<-dplyr::coalesce(sm_BT_freshw$LONGITUDE.x,sm_BT_freshw$LONGITUDE.y)
sm_BT_freshw$LATITUDE<-dplyr::coalesce(sm_BT_freshw$LATITUDE.x,sm_BT_freshw$LATITUDE.y)
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_freshw$STUDY_ID==253)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/253/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_253_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_freshw<-left_join(sm_BT_freshw,mylonlat,by="newsite")
sm_BT_freshw$LONGITUDE<-dplyr::coalesce(sm_BT_freshw$LONGITUDE.x,sm_BT_freshw$LONGITUDE.y)
sm_BT_freshw$LATITUDE<-dplyr::coalesce(sm_BT_freshw$LATITUDE.x,sm_BT_freshw$LATITUDE.y)
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                             initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                             L,U,f_nind,f_nL,f_nU,f_nneg,
                                             cvsq_real,cvsq_indep,phi,phi_LdM,
                                             skw_real,skw_indep,phi_skw,
                                             iCV,iCValt,LONGITUDE,LATITUDE,
                                             t_med,tmax_med,tmin_med,
                                             t_skw,tmax_skw,tmin_skw,t_var,
                                             trend_t_tau,trend_t_tau_sig))

#----------------------------- for BioTIME: terrestrial -----------------------------------------------------
df<-md%>%filter(source=="BioTIME" & REALM=="Terrestrial")%>%filter(TAXA%in%c("birds","terrestrial invertebrates"))
length(unique(df$newsite))==nrow(df) #This should be TRUE

sm_BT_terres<-readRDS("../Results/for_BioTIME/Terrestrial_plotlevel/stability_metric_and_env.RDS")
sm_BT_terres$source<-"BioTIME"

df<-df%>%filter(STUDY_ID%in%sm_BT_terres$STUDY_ID)%>%dplyr::select(STUDY_ID,TAXA,ORGANISMS)%>%distinct(STUDY_ID,.keep_all = T)

terres_tbl_for_map<-readRDS("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")
terres_tbl_for_map<-terres_tbl_for_map%>%filter(STUDY_ID%in%sm_BT_terres$STUDY_ID)

df$STUDY_ID<-as.integer(df$STUDY_ID)
sm_BT_terres<-left_join(sm_BT_terres, df,by = "STUDY_ID")
sm_BT_terres$LONGITUDE<-NA
sm_BT_terres$LATITUDE<-NA

singlesite<-terres_tbl_for_map%>%filter(LATmin==LATmax & LONmin==LONmax)%>%dplyr::select(STUDY_ID,LONGITUDE=LONmin,LATITUDE=LATmin)
sm_BT_terres<-left_join(sm_BT_terres,singlesite,by="STUDY_ID")
sm_BT_terres$LONGITUDE<-dplyr::coalesce(sm_BT_terres$LONGITUDE.x,sm_BT_terres$LONGITUDE.y)
sm_BT_terres$LATITUDE<-dplyr::coalesce(sm_BT_terres$LATITUDE.x,sm_BT_terres$LATITUDE.y)
sm_BT_terres<-sm_BT_terres%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))



id<-which(sm_BT_terres$STUDY_ID==67)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/67/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_67_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_terres<-left_join(sm_BT_terres,mylonlat,by="newsite")
sm_BT_terres$LONGITUDE<-dplyr::coalesce(sm_BT_terres$LONGITUDE.x,sm_BT_terres$LONGITUDE.y)
sm_BT_terres$LATITUDE<-dplyr::coalesce(sm_BT_terres$LATITUDE.x,sm_BT_terres$LATITUDE.y)
sm_BT_terres<-sm_BT_terres%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_terres$STUDY_ID==215)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/215/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_215_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_terres<-left_join(sm_BT_terres,mylonlat,by="newsite")
sm_BT_terres$LONGITUDE<-dplyr::coalesce(sm_BT_terres$LONGITUDE.x,sm_BT_terres$LONGITUDE.y)
sm_BT_terres$LATITUDE<-dplyr::coalesce(sm_BT_terres$LATITUDE.x,sm_BT_terres$LATITUDE.y)
sm_BT_terres<-sm_BT_terres%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))

id<-which(sm_BT_terres$STUDY_ID==420)
mylonlat<-read.csv("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/420/mylonlat.csv")
mylonlat$newsite<-paste("STUDY_ID_420_LAT",mylonlat$LATITUDE,"_LON",mylonlat$LONGITUDE,sep="")
mylonlat<-mylonlat%>%dplyr::select(newsite,LONGITUDE,LATITUDE)
sm_BT_terres<-left_join(sm_BT_terres,mylonlat,by="newsite")
sm_BT_terres$LONGITUDE<-dplyr::coalesce(sm_BT_terres$LONGITUDE.x,sm_BT_terres$LONGITUDE.y)
sm_BT_terres$LATITUDE<-dplyr::coalesce(sm_BT_terres$LATITUDE.x,sm_BT_terres$LATITUDE.y)
sm_BT_terres<-sm_BT_terres%>%dplyr::select(-c(LONGITUDE.x,LONGITUDE.y,LATITUDE.x,LATITUDE.y))


sm_BT_terres<-sm_BT_terres%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                             initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                             L,U,f_nind,f_nL,f_nU,f_nneg,
                                             cvsq_real,cvsq_indep,phi,phi_LdM,
                                             skw_real,skw_indep,phi_skw,
                                             iCV,iCValt,LONGITUDE,LATITUDE,
                                             t_med,tmax_med,tmin_med,
                                             t_skw,tmax_skw,tmin_skw,t_var,
                                             trend_t_tau,trend_t_tau_sig))

#----------------------------- for BioTIMEx -----------------------------------------------------
sm_BTx<-readRDS("../Results/for_BioTIMEx/stability_metric_and_env.RDS")
# rearrange
sm_BTx<-sm_BTx%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                             initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                             L,U,f_nind,f_nL,f_nU,f_nneg,
                                             cvsq_real,cvsq_indep,phi,phi_LdM,
                                             skw_real,skw_indep,phi_skw,
                                             iCV,iCValt,LONGITUDE,LATITUDE,
                                             t_med,tmax_med,tmin_med,
                                             t_skw,tmax_skw,tmin_skw,t_var,trend_t_tau,trend_t_tau_sig))

#----------------------------- for BBS -----------------------------------------------------
sm_BBS<-readRDS("../Results/for_BBS/stability_metric_and_env.RDS")
sm_BBS$source<-"BBS"
sm_BBS$TAXA <-"Birds"
sm_BBS$ORGANISMS <-"Birds"
sm_BBS<-rename(sm_BBS, newsite = siteid) # each siteid is renamed as newsite to be nested within the stratum 
sm_BBS<-rename(sm_BBS, STUDY_ID = Stratum_name) # stratum name renamed as STUDY_ID
sm_BBS<-rename(sm_BBS, LATITUDE = Latitude) 
sm_BBS<-rename(sm_BBS, LONGITUDE = Longitude) 

# rearrange
sm_BBS<-sm_BBS%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                 initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                 L,U,f_nind,f_nL,f_nU,f_nneg,
                                 cvsq_real,cvsq_indep,phi,phi_LdM,
                                 skw_real,skw_indep,phi_skw,
                                 iCV,iCValt,LONGITUDE,LATITUDE,
                                 t_med,tmax_med,tmin_med,
                                 t_skw,tmax_skw,tmin_skw,t_var,
                                 trend_t_tau,trend_t_tau_sig))

#----------------------------- for RivFishTIME -----------------------------------------------------
sm_RF<-readRDS("../Results/for_RivFishTIME/stability_metric_and_env.RDS")
sm_RF$source<-"RivFishTIME"
sm_RF$TAXA <-"Fish"
sm_RF$ORGANISMS <-"Fish"
sm_RF<-rename(sm_RF, newsite = siteid) # each siteid is renamed as newsite to be nested within the hydrobasin 
sm_RF<-rename(sm_RF, STUDY_ID = HydroBasin) # Hydrobasin renamed as STUDY_ID
sm_RF<-rename(sm_RF, LATITUDE = Latitude) 
sm_RF<-rename(sm_RF, LONGITUDE = Longitude) 

# rearrange
sm_RF<-sm_RF%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                               initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                               L,U,f_nind,f_nL,f_nU,f_nneg,
                               cvsq_real,cvsq_indep,phi,phi_LdM,
                               skw_real,skw_indep,phi_skw,
                               iCV,iCValt,LONGITUDE,LATITUDE,
                               t_med,tmax_med,tmin_med,
                               t_skw,tmax_skw,tmin_skw,t_var,
                               trend_t_tau,trend_t_tau_sig))
#----------------------------- for insectRoel -----------------------------------------------------
sm_insect<-readRDS("../Results/for_insectRoel/stability_metric_and_env.RDS")
sm_insect$source<-"InsectRoel"
sm_insect<-rename(sm_insect, LATITUDE = Latitude) 
sm_insect<-rename(sm_insect, LONGITUDE = Longitude) 

# rearrange
sm_insect<-sm_insect%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                       initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                       L,U,f_nind,f_nL,f_nU,f_nneg,
                                       cvsq_real,cvsq_indep,phi,phi_LdM,
                                       skw_real,skw_indep,phi_skw,
                                       iCV,iCValt,LONGITUDE,LATITUDE,
                                       t_med,tmax_med,tmin_med,
                                       t_skw,tmax_skw,tmin_skw,t_var,
                                       trend_t_tau,trend_t_tau_sig))

#--------------------------------------- for swisslake zoo -----------------------------------------------------
sm_swisslake_zoo<-readRDS("../Results/for_swisslake/zooplankton/stability_metric_and_env.RDS")
sm_swisslake_zoo<-rename(sm_swisslake_zoo, LATITUDE = CENT_LAT) 
sm_swisslake_zoo<-rename(sm_swisslake_zoo, LONGITUDE = CENT_LONG) 

sm_swisslake_zoo<-sm_swisslake_zoo%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                                     initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                                     L,U,f_nind,f_nL,f_nU,f_nneg,
                                                     cvsq_real,cvsq_indep,phi,phi_LdM,
                                                     skw_real,skw_indep,phi_skw,
                                                     iCV,iCValt,LONGITUDE,LATITUDE,
                                                     t_med,tmax_med,tmin_med,
                                                     t_skw,tmax_skw,tmin_skw,t_var,
                                                     trend_t_tau,trend_t_tau_sig))

#------------------------------- for zoop2014 ----------------------------------------------
sm_zoop<-readRDS("../Results/for_zoop_2014/stability_metric_and_env.RDS")
sm_zoop<-rename(sm_zoop, LATITUDE = CENT_LAT) 
sm_zoop<-rename(sm_zoop, LONGITUDE = CENT_LONG) 

sm_zoop<-sm_zoop%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                   initR,nsp,nyr_used,startyr,endyr,nint,nind,npos,nL,nU,nneg,
                                   L,U,f_nind,f_nL,f_nU,f_nneg,
                                   cvsq_real,cvsq_indep,phi,phi_LdM,
                                   skw_real,skw_indep,phi_skw,
                                   iCV,iCValt,LONGITUDE,LATITUDE,
                                   t_med,tmax_med,tmin_med,
                                   t_skw,tmax_skw,tmin_skw,t_var,
                                   trend_t_tau,trend_t_tau_sig))
#================================================================================================
sm_all<-rbind(sm_BT_freshw,sm_BT_terres,sm_BTx,sm_BBS,sm_RF,sm_swisslake_zoo,sm_zoop,sm_insect)
#sm_all<-rbind(sm_BTx,sm_BBS,sm_RF,sm_swisslake_zoo,sm_zoop,sm_insect)
sm_all$TAXA<-tolower(sm_all$TAXA)
sm_all<-sm_all%>%filter(nyr_used>=20)
write.csv(sm_all,"../Results/stability_metric_and_env_all.csv",row.names=F)







