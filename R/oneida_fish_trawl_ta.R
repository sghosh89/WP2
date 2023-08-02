# This script will do the copula analysis, will generate necessary plots, will show how a species 
# tail dep. fluctuates with the rest of species in the phyto-plankton community

source("tail_analysis.R")
library(tidyverse)
`%notin%` <- Negate(`%in%`)

dataset_id<-"oneida_fish_trawl"
md<-read.csv("../DATA/for_BioTIMEx/wrangled_data/annual_tas_CHELSA_1979_2019_BTx_oneida_fish_lonlat.csv")
md_now<-md%>%filter(STUDY_ID==dataset_id)

resloc<-'../Results/for_BioTIMEx/'
if(!dir.exists(resloc)){
  dir.create(resloc)
}

if(!dir.exists(paste(resloc,dataset_id,"/",sep=""))){
  dir.create(paste(resloc,dataset_id,"/",sep=""))
}

nrow(md_now)>1
for(i in 1:nrow(md_now)){
  resloc_o<-paste(resloc,dataset_id,"/",md_now$newsite[i],"/",sep="")
  if(!dir.exists(resloc_o)){
    dir.create(resloc_o)
  }
}

resloc_input<-paste("../DATA/for_BioTIMEx/wrangled_data/",dataset_id,"/",sep="")
usite<-md_now%>%distinct(newsite)

for(i in 1:nrow(usite)){
  site<-usite$newsite[i]
  input_tailanal<-readRDS(paste(resloc_input,dataset_id,"_",site,"_inputmatrix_tailanal.RDS",sep=""))
  tot_target_sp<-ncol(input_tailanal)
  saveRDS(tot_target_sp,paste(resloc_input,"tot_target_sp_newsite_",site,".RDS",sep="")) # nsp
  #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
  input_tailanal$yr<-as.integer(rownames(input_tailanal))
  input_tailanal<-input_tailanal%>%filter(yr%in%c(1979:2019))
  rownames(input_tailanal)<-input_tailanal$yr
  #check again
  suffyr<-(nrow(input_tailanal)>=20)
  if(suffyr==T){
    #-----------------------adding environmental variable in the matrix-----------------------------
    tempdat<-md_now%>%filter(STUDY_ID%in%dataset_id)%>%filter(newsite==site)%>%filter(yr%in%rownames(input_tailanal))%>%dplyr::select(yr,t,tmax,tmin)
    tempdat$tmax_n<- -tempdat$tmax
    
    # check if all TRUE
    all(rownames(input_tailanal)==tempdat$yr)==T
    
    input_tailanal$t<-tempdat$t
    input_tailanal$tmax<-tempdat$tmax
    input_tailanal$tmin<-tempdat$tmin
    input_tailanal$tmax_n<-tempdat$tmax_n
    input_tailanal<-input_tailanal%>%dplyr::select(-yr)
    
    saveRDS(input_tailanal,paste(resloc_input,"input_mat_for_tailanal_with_env_",dataset_id,"_",site,".RDS",sep="")) # dataframe with species timeseries along column
    resloc_output<-paste(resloc,dataset_id,"/",site,"/",sep="")
    
    res<-tail_analysis(mat = input_tailanal, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)
  }
  
}

#----------------------------------------------------------





