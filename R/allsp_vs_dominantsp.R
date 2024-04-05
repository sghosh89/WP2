library(here)
library(tidyverse)
library(BiodiversityR)
source(here("R/get_stability_metric.R"))
`%notin%` <- Negate(`%in%`)

sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sm_bf<-sm_all%>%filter(TAXA%in%c("birds","fish"))
table(sm_bf$source)
sm_bf<-sm_bf%>%dplyr::select(source,STUDY_ID, newsite, REALM, TAXA, initR, nsp, nyr_used, iCV)


#=============== first for BBS database =========================
resloc<-here("DATA/for_BBS/wrangled_data")

#fshort_list<-readRDS(paste(resloc,"/sourcefile_list.RDS",sep=""))

tab_BBS<-sm_bf%>%filter(source=="BBS")
tab_BBS$iCV_allsp<-NA
tab_BBS$max_accumfreq_commonsp<-NA
tab_BBS$max_proportion_ab_raresp<-NA
tab_BBS$min_proportion_ab_commonsp<-NA

for (i in 1:nrow(tab_BBS)){
  resloc2<-paste(resloc,tab_BBS$newsite[i],sep="/")
  x<-readRDS(paste(resloc2,"/sourcefile.RDS",sep=""))
  y<-x%>%dplyr::select(Year,AOU,Stop1to50)
  y<-y%>%spread(AOU,Stop1to50,fill=0)%>%as.data.frame()
  rownames(y)<-y$Year # and colnames are species code
  y<-y[,-1]
  saveRDS(y, paste(resloc2,"/allspecies_timeseries.RDS",sep=""))
  
  iCV_allsp<-get_stability_metric(y)
  tab_BBS$iCV_allsp[i]<-iCV_allsp$iCV
    
  rad<-rankabundance(y)
  rad<-as.data.frame(rad)
  rad$spname<-rownames(rad)
  
  # now read the common sp used 
  yc<-readRDS(paste(resloc2,"/input_mat_for_tailanal_with_env_celcius.RDS",sep=""))
  id<-which(colnames(yc)%in%rownames(rad))# species always will be included in all sp
  yc<-yc[,id]
  commonsp<-colnames(yc)
  
  #sprank<-match(rownames(rad),colnames(yc))
  #range(sprank,na.rm=T)
  commonsp_rad<-rad%>%filter(spname%in%commonsp)
  raresp_rad<-rad%>%filter(spname%notin%commonsp)
  
  if(tab_BBS$initR[i]==tab_BBS$nsp[i]){
    # then allspecies are considered as common sp, no raresp
    tab_BBS$max_proportion_ab_raresp[i]<-NA
  }else{
    tab_BBS$max_proportion_ab_raresp[i]<-max(raresp_rad$proportion)
  }
  
  tab_BBS$max_accumfreq_commonsp[i]<-max(commonsp_rad$accumfreq)# contribution by common sp to the total abundance
  tab_BBS$min_proportion_ab_commonsp[i]<-min(commonsp_rad$proportion)
  
}  
saveRDS(tab_BBS, paste(resloc,"/tab_BBS_radsummary.RDS",sep=""))

#============== Now for RivFishTIME database =================
resloc<-here("DATA/for_RivFishTIME/wrangled_data")

tab_RF<-sm_bf%>%filter(source=="RivFishTIME")
tab_RF$iCV_allsp<-NA
tab_RF$max_accumfreq_commonsp<-NA
tab_RF$max_proportion_ab_raresp<-NA
tab_RF$min_proportion_ab_commonsp<-NA

for (i in 1:nrow(tab_RF)){
  resloc2<-paste(resloc,tab_RF$newsite[i],sep="/")
  y<-readRDS(paste(resloc2,"/allspecies_timeseries.RDS",sep=""))
  
  iCV_allsp<-get_stability_metric(y)
  tab_RF$iCV_allsp[i]<-iCV_allsp$iCV
  
  rad<-rankabundance(y)
  rad<-as.data.frame(rad)
  rad$spname<-rownames(rad)
  
  # now read the common sp used 
  yc<-readRDS(paste(resloc2,"/input_mat_for_tailanal_with_env_celcius.RDS",sep=""))
  id<-which(colnames(yc)%in%rownames(rad))# species always will be included in all sp
  yc<-yc[,id]
  commonsp<-colnames(yc)
  
  #sprank<-match(rownames(rad),colnames(yc))
  #range(sprank,na.rm=T)
  commonsp_rad<-rad%>%filter(spname%in%commonsp)
  raresp_rad<-rad%>%filter(spname%notin%commonsp)
  
  if(tab_RF$initR[i]==tab_RF$nsp[i]){
    # then allspecies are considered as common sp, no raresp
    tab_RF$max_proportion_ab_raresp[i]<-NA
  }else{
    tab_RF$max_proportion_ab_raresp[i]<-max(raresp_rad$proportion)
  }
  
  tab_RF$max_accumfreq_commonsp[i]<-max(commonsp_rad$accumfreq)# contribution by common sp to the total abundance
  tab_RF$min_proportion_ab_commonsp[i]<-min(commonsp_rad$proportion)
  
}  
saveRDS(tab_RF, paste(resloc,"/tab_RF_radsummary.RDS",sep=""))

#============== Now for BioTIMEx database =================
resloc<-here("DATA/for_BioTIMEx/wrangled_data")

tab_BTx<-sm_bf%>%filter(source=="BioTIMEx")
tab_BTx$iCV_allsp<-NA
tab_BTx$max_accumfreq_commonsp<-NA
tab_BTx$max_proportion_ab_raresp<-NA
tab_BTx$min_proportion_ab_commonsp<-NA

for (i in 1:nrow(tab_BTx)){
  resloc2<-paste(resloc,tab_BTx$STUDY_ID[i],sep="/")
  study_id<-tab_BTx$STUDY_ID[i]
  new_site<-tab_BTx$newsite[i]
  allrawdata<-read.csv(here("DATA/for_BioTIMEx/wrangled_data/",study_id,"/allrawdata.csv"))
  
  if(study_id=="oneida_fish_gillnets"){
    allrawdata<-allrawdata%>%filter(ID_SPECIES%notin%c("Esox masquinongy x Esox lucius"))
    x<-allrawdata%>%filter(SITE==new_site)
    spmat<-x%>%group_by(ID_SPECIES,YEAR)%>%summarise(ABUNDANCE=mean(ABUNDANCE))%>%ungroup()
    spmat<-spmat%>%spread(ID_SPECIES,ABUNDANCE,fill=0)%>%as.data.frame()
    rownames(spmat)<-spmat$YEAR
    spmat<-spmat[,-1]
    y<-spmat
  }else{
    allrawdata$SITE<-tolower(allrawdata$SITE)
    allrawdata$SAMPLE_DESC<-tolower(allrawdata$SAMPLE_DESC)
    xx<-allrawdata%>%filter(SITE%in%c(""))
    sort(unique(xx$SAMPLE_DESC))
    xx$SAMPLE_DESC<-substr(xx$SAMPLE_DESC,1,nchar(xx$SAMPLE_DESC)-9)
    
    id<-which(xx$SAMPLE_DESC%in%c("bernhards_bay","bernhards_bay_","bernhards_bay_1"))
    xx$SAMPLE_DESC[id]<-"bernhards_bay"
    id<-which(xx$SAMPLE_DESC%in%c("billington_bay","billington_bay_","billington_bay_2"))
    xx$SAMPLE_DESC[id]<-"billington_bay"
    id<-which(xx$SAMPLE_DESC%in%c("buoy_117","buoy_117_","buoy_117_2"))
    xx$SAMPLE_DESC[id]<-"buoy_117"
    id<-which(xx$SAMPLE_DESC%in%c("buoy_125","buoy_125_","buoy_125_2"))
    xx$SAMPLE_DESC[id]<-"buoy_125"
    id<-which(xx$SAMPLE_DESC%in%c("buoy_125_north","buoy_125_north_","buoy_125_north_2"))
    xx$SAMPLE_DESC[id]<-"buoy_125_north"
    id<-which(xx$SAMPLE_DESC%in%c("buoy_133","buoy_133_","buoy_133_2"))
    xx$SAMPLE_DESC[id]<-"buoy_133"
    id<-which(xx$SAMPLE_DESC%in%c("delmarter_bay","delmarter_bay_","delmarter_bay_2" ))
    xx$SAMPLE_DESC[id]<-"delmarter_bay"
    id<-which(xx$SAMPLE_DESC%in%c("shackelton_point_deep","shackelton_point_deep_","shackelton_point_deep_2"))
    xx$SAMPLE_DESC[id]<-"shackelton_point_deep"
    id<-which(xx$SAMPLE_DESC%in%c("shackelton_point_shallow","shackelton_point_shallow_","shackelton_point_shallow_1","shackelton_point_shallow_2"))
    xx$SAMPLE_DESC[id]<-"shackelton_point_shallow"
    id<-which(xx$SAMPLE_DESC%in%c("three_mile_bay","three_mile_bay_","three_mile_bay_2"))
    xx$SAMPLE_DESC[id]<-"three_mile_bay"
    xx$SITE<-xx$SAMPLE_DESC
    
    allrawdata<-allrawdata%>%filter(SITE%notin%c(""))
    allrawdata<-rbind(allrawdata,xx)
    
    tempo<-allrawdata%>%group_by(SITE)%>%summarise(ny=n_distinct(YEAR))%>%ungroup()
    goodsite<-tempo$SITE[which(tempo$ny>=20)]
    # 10 good sites
    
    allrawdata<-allrawdata%>%filter(SITE%in%goodsite)
    x<-allrawdata%>%filter(SITE==new_site)
    spmat<-x%>%group_by(ID_SPECIES,YEAR)%>%summarise(ABUNDANCE=mean(ABUNDANCE))%>%ungroup()
    spmat<-spmat%>%spread(ID_SPECIES,ABUNDANCE,fill=0)%>%as.data.frame()
    rownames(spmat)<-spmat$YEAR
    spmat<-spmat[,-1]
    y<-spmat
  }
  
  iCV_allsp<-get_stability_metric(y)
  tab_BTx$iCV_allsp[i]<-iCV_allsp$iCV
  
  rad<-rankabundance(y)
  rad<-as.data.frame(rad)
  rad$spname<-rownames(rad)
  
  # now read the common sp used 
  yc<-readRDS(paste(resloc2,"/input_mat_for_tailanal_with_env_celcius_",study_id,"_",new_site,".RDS",sep=""))
  id<-which(colnames(yc)%in%rownames(rad))# species always will be included in all sp
  yc<-yc[,id]
  commonsp<-colnames(yc)
  
  #sprank<-match(rownames(rad),colnames(yc))
  #range(sprank,na.rm=T)
  commonsp_rad<-rad%>%filter(spname%in%commonsp)
  raresp_rad<-rad%>%filter(spname%notin%commonsp)
  
  if(tab_BTx$initR[i]==tab_BTx$nsp[i]){
    # then allspecies are considered as common sp, no raresp
    tab_BTx$max_proportion_ab_raresp[i]<-NA
  }else{
    tab_BTx$max_proportion_ab_raresp[i]<-max(raresp_rad$proportion)
  }
  
  tab_BTx$max_accumfreq_commonsp[i]<-max(commonsp_rad$accumfreq)# contribution by common sp to the total abundance
  tab_BTx$min_proportion_ab_commonsp[i]<-min(commonsp_rad$proportion)
  
}  
saveRDS(tab_BTx, paste(resloc,"/tab_BTx_radsummary.RDS",sep=""))


#============== Now for BioTIME database =================
resloc<-here("DATA/for_BioTIME/wrangled_data")

tab_BT<-sm_bf%>%filter(source=="BioTIME")
tab_BT$iCV_allsp<-NA
tab_BT$max_accumfreq_commonsp<-NA
tab_BT$max_proportion_ab_raresp<-NA
tab_BT$min_proportion_ab_commonsp<-NA

for (i in 1:nrow(tab_BT)){
  
  if(tab_BT$STUDY_ID[i]==tab_BT$newsite[i]){
    resloc2<-paste(resloc,"/",tab_BT$REALM[i],"_plotlevel/",tab_BT$newsite[i],sep="")
  }else{
    resloc2<-paste(resloc,"/",tab_BT$REALM[i],"_plotlevel/",tab_BT$STUDY_ID[i],"/",tab_BT$newsite[i],sep="")
  }
  
  y<-readRDS(paste(resloc2,"/allspecies_timeseries_and_metadata.RDS",sep=""))
  y<-y$spmat
  
  iCV_allsp<-get_stability_metric(y)
  tab_BT$iCV_allsp[i]<-iCV_allsp$iCV
  
  rad<-rankabundance(y)
  rad<-as.data.frame(rad)
  rad$spname<-rownames(rad)
  
  # now read the common sp used 
  yc<-readRDS(paste(resloc2,"/input_mat_for_tailanal_with_env_celcius.RDS",sep=""))
  id<-which(colnames(yc)%in%rownames(rad))# species always will be included in all sp
  yc<-yc[,id]
  commonsp<-colnames(yc)
  
  #sprank<-match(rownames(rad),colnames(yc))
  #range(sprank,na.rm=T)
  commonsp_rad<-rad%>%filter(spname%in%commonsp)
  raresp_rad<-rad%>%filter(spname%notin%commonsp)
  
  if(tab_BT$initR[i]==tab_BT$nsp[i]){
    # then allspecies are considered as common sp, no raresp
    tab_BT$max_proportion_ab_raresp[i]<-NA
  }else{
    tab_BT$max_proportion_ab_raresp[i]<-max(raresp_rad$proportion)
  }
  
  tab_BT$max_accumfreq_commonsp[i]<-max(commonsp_rad$accumfreq)# contribution by common sp to the total abundance
  tab_BT$min_proportion_ab_commonsp[i]<-min(commonsp_rad$proportion)
  
}  
saveRDS(tab_BT, paste(resloc,"/tab_BT_radsummary.RDS",sep=""))

# gather all
tab_rad_summary<-rbind(tab_BT,tab_BTx,tab_BBS,tab_RF)
saveRDS(tab_rad_summary,here("Results/tab_rad_summary.RDS"))



