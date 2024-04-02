#rm(list=ls())
library(here)
library(tidyverse)

source(here("R/tail_analysis.R"))

#====================================
# now do tail-analysis for nbin>2

sbf30<-read.csv(here("Results/stability_metric_and_env_all_nyr_min30.csv"))

for(i in 1:nrow(sbf30)){
  sid<-sbf30$STUDY_ID[i]
  ns<-sbf30$newsite[i]
  db<-sbf30$source[i]
  nyr<-sbf30$nyr_used[i]  
  
  nbin<-3
  if(db=="BioTIME"){
    rlm<-sbf30$REALM[i]
    if(sid==ns){
      resloc<-here(paste("DATA/for_BioTIME/wrangled_data/",rlm,"_plotlevel/",sid,sep=""))
      resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/nbin_",nbin,sep=""))
    }else{
      resloc<-here(paste("DATA/for_BioTIME/wrangled_data/",rlm,"_plotlevel/",sid,"/",ns,sep=""))
      resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/",ns,"/nbin_",nbin,sep=""))
    }
    
    input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  } 
  
  if(db=="BioTIMEx"){
    resloc<-here(paste("DATA/for_BioTIMEx/wrangled_data/",sid,"/",sep=""))
    resloc_output<-here(paste("Results/for_BioTIMEx/",sid,"/",ns,"/nbin_",nbin,sep=""))
    input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env_",sid,"_",ns,".RDS",sep=""))
  } 
  
  if(db=="RivFishTIME"){
    resloc<-here(paste("DATA/for_RivFishTIME/wrangled_data/",ns,"/",sep=""))
    resloc_output<-here(paste("Results/for_RivFishTIME/",ns,"/nbin_",nbin,sep=""))
    input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  }
  
  tot_target_sp<-length(setdiff(colnames(input_tailanal),c("t","tmax","tmin","tmax_n","year")))
  
  resloc_output<-paste(resloc_output,"/",sep="")
  if(!dir.exists(resloc_output)){
    dir.create(resloc_output)
  }
  res<-tail_analysis(mat = input_tailanal, 
                     tot_target_sp=tot_target_sp, 
                     resloc = resloc_output, 
                     nbin = nbin)
  
  
  if(nyr>=40){
    nbin<-4
    if(db=="BioTIME"){
      rlm<-sbf30$REALM[i]
      if(sid==ns){
        resloc<-here(paste("DATA/for_BioTIME/wrangled_data/",rlm,"_plotlevel/",sid,sep=""))
        resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/nbin_",nbin,sep=""))
      }else{
        resloc<-here(paste("DATA/for_BioTIME/wrangled_data/",rlm,"_plotlevel/",sid,"/",ns,sep=""))
        resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/",ns,"/nbin_",nbin,sep=""))
      }
      
      input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
    } 
    
    if(db=="BioTIMEx"){
      resloc<-here(paste("DATA/for_BioTIMEx/wrangled_data/",sid,"/",sep=""))
      resloc_output<-here(paste("Results/for_BioTIMEx/",sid,"/",ns,"/nbin_",nbin,sep=""))
      input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env_",sid,"_",ns,".RDS",sep=""))
    } 
    
    if(db=="RivFishTIME"){
      resloc<-here(paste("DATA/for_RivFishTIME/wrangled_data/",ns,"/",sep=""))
      resloc_output<-here(paste("Results/for_RivFishTIME/",ns,"/nbin_",nbin,sep=""))
      input_tailanal<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
    }
    
    tot_target_sp<-length(setdiff(colnames(input_tailanal),c("t","tmax","tmin","tmax_n","year")))
    
    resloc_output<-paste(resloc_output,"/",sep="")
    if(!dir.exists(resloc_output)){
      dir.create(resloc_output)
    }
    res<-tail_analysis(mat = input_tailanal, 
                       tot_target_sp=tot_target_sp, 
                       resloc = resloc_output, 
                       nbin = nbin)
    
    
  }
  
}















