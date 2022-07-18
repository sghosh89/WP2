# Now we will gather all summary table we got from analyzing different databases

library(tidyverse)
library(dplyr)
#=====================================================================
# gather stability metric summary for all data you have analyzed

md<-read.csv("../DATA/metadata_summary.csv")
#----------------------------- for BioTIME: freshwater -----------------------------------------------------
df<-md%>%filter(source=="BioTIME" & REALM=="Freshwater")
length(unique(df$newsite))==nrow(df) #This should be TRUE
df<-df%>%dplyr::select(newsite,TAXA,ORGANISMS,CENT_LAT,CENT_LONG)
 
sm_BT_freshw<-readRDS("../Results/for_BioTIME/Freshwater_plotlevel/stability_metric_and_env.RDS")
sm_BT_freshw$source<-"BioTIME"

sm_BT_freshw<-left_join(sm_BT_freshw,df,by="newsite")
colnames(sm_BT_freshw)
# rearrange
sm_BT_freshw<-sm_BT_freshw%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                         initR,nsp, nind,npos,nL,nU,nneg,
                                         L,U,f_nind,f_nL,f_nU,f_nneg,
                                         cvsq_real,cvsq_indep,phi,phi_LdM,
                                         skw_real,skw_indep,phi_skw,
                                         iCV,iCValt,CENT_LAT,CENT_LONG))

#----------------------------- for BioTIME: terrestrial -----------------------------------------------------
df<-md%>%filter(source=="BioTIME" & REALM=="Terrestrial")
length(unique(df$newsite))==nrow(df) #This should be TRUE
df<-df%>%dplyr::select(newsite,TAXA,ORGANISMS,CENT_LAT,CENT_LONG)

sm_BT_terres<-readRDS("../Results/for_BioTIME/Terrestrial_plotlevel/stability_metric_and_env.RDS")
sm_BT_terres$source<-"BioTIME"

sm_BT_terres<-left_join(sm_BT_terres,df,by="newsite")
colnames(sm_BT_terres)
# rearrange
sm_BT_terres<-sm_BT_terres%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                             initR,nsp, nind,npos,nL,nU,nneg,
                                             L,U,f_nind,f_nL,f_nU,f_nneg,
                                             cvsq_real,cvsq_indep,phi,phi_LdM,
                                             skw_real,skw_indep,phi_skw,
                                             iCV,iCValt,CENT_LAT,CENT_LONG))

#----------------------------- for BioTIMEx -----------------------------------------------------
sm_BTx<-readRDS("../Results/for_BioTIMEx/stability_metric_and_env.RDS")
# rearrange
sm_BTx<-sm_BTx%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                             initR,nsp, nind,npos,nL,nU,nneg,
                                             L,U,f_nind,f_nL,f_nU,f_nneg,
                                             cvsq_real,cvsq_indep,phi,phi_LdM,
                                             skw_real,skw_indep,phi_skw,
                                             iCV,iCValt,CENT_LAT,CENT_LONG))

#----------------------------- for BBS -----------------------------------------------------
sm_BBS<-readRDS("../Results/for_BBS/stability_metric_and_env.RDS")
sm_BBS$source<-"BBS"
sm_BBS$TAXA <-"Birds"
sm_BBS$ORGANISMS <-"Birds"
sm_BBS<-rename(sm_BBS, newsite = siteid) # each siteid is renamed as newsite to be nested within the stratum 
sm_BBS<-rename(sm_BBS, STUDY_ID = Stratum_name) # stratum name renamed as STUDY_ID
sm_BBS<-rename(sm_BBS, CENT_LAT = Latitude) 
sm_BBS<-rename(sm_BBS, CENT_LONG = Longitude) 

# rearrange
sm_BBS<-sm_BBS%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                 initR,nsp, nind,npos,nL,nU,nneg,
                                 L,U,f_nind,f_nL,f_nU,f_nneg,
                                 cvsq_real,cvsq_indep,phi,phi_LdM,
                                 skw_real,skw_indep,phi_skw,
                                 iCV,iCValt,CENT_LAT,CENT_LONG))
#----------------------------- for RivFishTIME -----------------------------------------------------
sm_RF<-readRDS("../Results/for_RivFishTIME/stability_metric_and_env.RDS")
sm_RF$source<-"RivFishTIME"
sm_RF$TAXA <-"Fish"
sm_RF$ORGANISMS <-"Fish"
sm_RF<-rename(sm_RF, newsite = siteid) # each siteid is renamed as newsite to be nested within the hydrobasin 
sm_RF<-rename(sm_RF, STUDY_ID = HydroBasin) # Hydrobasin renamed as STUDY_ID
sm_RF<-rename(sm_RF, CENT_LAT = Latitude) 
sm_RF<-rename(sm_RF, CENT_LONG = Longitude) 

# rearrange
sm_RF<-sm_RF%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                 initR,nsp, nind,npos,nL,nU,nneg,
                                 L,U,f_nind,f_nL,f_nU,f_nneg,
                                 cvsq_real,cvsq_indep,phi,phi_LdM,
                                 skw_real,skw_indep,phi_skw,
                                 iCV,iCValt,CENT_LAT,CENT_LONG))
#----------------------------- for insectRoel -----------------------------------------------------
sm_insect<-readRDS("../Results/for_insectRoel/stability_metric_and_env.RDS")
sm_insect$source<-"InsectRoel"
sm_insect<-rename(sm_insect, CENT_LAT = Latitude) # each siteid is renamed as newsite to be nested within the stratum 
sm_insect<-rename(sm_insect, CENT_LONG = Longitude) # stratum name renamed as STUDY_ID

# rearrange
sm_insect<-sm_insect%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                 initR,nsp, nind,npos,nL,nU,nneg,
                                 L,U,f_nind,f_nL,f_nU,f_nneg,
                                 cvsq_real,cvsq_indep,phi,phi_LdM,
                                 skw_real,skw_indep,phi_skw,
                                 iCV,iCValt,CENT_LAT,CENT_LONG))

#--------------------------------------- for swisslake zoo -----------------------------------------------------
sm_swisslake_zoo<-readRDS("../Results/for_swisslake/zooplankton/stability_metric_and_env.RDS")
sm_swisslake_zoo<-sm_swisslake_zoo%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                                     initR,nsp, nind,npos,nL,nU,nneg,
                                                     L,U,f_nind,f_nL,f_nU,f_nneg,
                                                     cvsq_real,cvsq_indep,phi,phi_LdM,
                                                     skw_real,skw_indep,phi_skw,
                                                     iCV,iCValt,CENT_LAT,CENT_LONG))

#------------------------------- for zoop2014 ----------------------------------------------
sm_zoop<-readRDS("../Results/for_zoop_2014/stability_metric_and_env.RDS")
sm_zoop<-sm_zoop%>%dplyr::select(c(source,STUDY_ID,newsite,REALM,TAXA,ORGANISMS,
                                                     initR,nsp, nind,npos,nL,nU,nneg,
                                                     L,U,f_nind,f_nL,f_nU,f_nneg,
                                                     cvsq_real,cvsq_indep,phi,phi_LdM,
                                                     skw_real,skw_indep,phi_skw,
                                                     iCV,iCValt,CENT_LAT,CENT_LONG))

#================================================================================================
sm_all<-rbind(sm_BT_freshw,sm_BT_terres,sm_BTx,sm_BBS,sm_RF,sm_swisslake_zoo,sm_zoop,sm_insect)
write.csv(sm_all,"../Results/stability_metric_and_env_all.csv",row.names=F)







