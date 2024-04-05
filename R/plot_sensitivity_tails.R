library(here)
library(tidyverse)
library(ggpubr)
library(gridExtra)

sbf30<-read.csv(here("Results/stability_metric_and_env_all_nyr_min30.csv"))
sbf30<-sbf30%>%dplyr::select(source,STUDY_ID,newsite,REALM,TAXA,initR,nsp,nyr_used,L,U)
sbf30$L_nbin3<-NA
sbf30$U_nbin3<-NA

for(i in 1:nrow(sbf30)){
  sid<-sbf30$STUDY_ID[i]
  ns<-sbf30$newsite[i]
  db<-sbf30$source[i]
  nyr<-sbf30$nyr_used[i]  
  
  nbin<-3
  if(db=="BioTIME"){
    rlm<-sbf30$REALM[i]
    if(sid==ns){
      resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/nbin_",nbin,sep=""))
    }else{
      resloc_output<-here(paste("Results/for_BioTIME/",rlm,"_plotlevel/",sid,"/",ns,"/nbin_",nbin,sep=""))
    }
  } 
  
  if(db=="BioTIMEx"){
    resloc_output<-here(paste("Results/for_BioTIMEx/",sid,"/",ns,"/nbin_",nbin,sep=""))
  } 
  
  if(db=="RivFishTIME"){
    resloc_output<-here(paste("Results/for_RivFishTIME/",ns,"/nbin_",nbin,sep=""))
  }
  
  resloc_output<-paste(resloc_output,"/",sep="")
  zz<-readRDS(paste(resloc_output,"NonParamStat.RDS",sep=""))
  z<-zz$Corl - zz$Coru
  indI<-zz$posnI
  indI<-which(indI==1,arr.ind = T)
  z[indI]<-NA
  diag(z)<-NA
  
  tot_target_sp<-length(setdiff(colnames(z),c("t","tmax","tmin","tmax_n","year")))
  
  z1<-z[1:tot_target_sp,1:tot_target_sp]
  z1posnI<-zz$posnI[1:tot_target_sp,1:tot_target_sp]
  z1posnN<-zz$posnN[1:tot_target_sp,1:tot_target_sp]
  z1_posnI_ind<-which(z1posnI==1, arr.ind=T)
  z1_posnN_ind<-which(z1posnN==1, arr.ind=T)
  z1[z1_posnN_ind]<-NA # this line was added to exclude -vely correlated species pair from nL,nU,L,U 
  # calculation, but it does not matter as for -vely correlated cells [sp_i,sp_j] and 
  # [sp_j,sp_i] nL,nU both will increase by same number, whereas L+U remains same and
  # both L, U will change by same +, - factor
  
  
  
  z1[upper.tri(z1)]<-NA # symmetric matrix, so consider only lower triangular part
  
  #nL<-sum(z1>0,na.rm = T)
  #nU<-sum(z1<0,na.rm = T)
  L_nbin3<-sum(z1[which(z1>0,arr.ind=T)])
  U_nbin3<-sum(z1[which(z1<0,arr.ind=T)])
  sbf30$L_nbin3[i]<-L_nbin3
  sbf30$U_nbin3[i]<-U_nbin3
}

sbf30$A<-sbf30$L+abs(sbf30$U)
sbf30$A_nbin3 <- sbf30$L_nbin3 + abs(sbf30$U_nbin3)

g1<-ggplot(sbf30,aes(x=A,y=A_nbin3))+geom_point(alpha=0.3,col="blue")+
  geom_smooth(method="lm",se=F)+theme_bw()+
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 4.6,col="blue")+
  stat_regline_equation(label.x = 0.1, label.y = 5,col="blue")+
  xlab("Community-level tail-dependent synchrony, A \n for nbin=2")+
  ylab("Community-level tail-dependent synchrony, A \n for nbin=3")+
  ggtitle(label="100 communities with minimum 30 years of data")

#========= now for nbin=4 =========================
sbf40<-sbf30%>%filter(nyr_used>=40)
sbf40$L_nbin4<-sbf40$U_nbin4<-NA
for(i in 1:nrow(sbf40)){
  sid<-sbf40$STUDY_ID[i]
  ns<-sbf40$newsite[i]
  db<-sbf40$source[i]
  nyr<-sbf40$nyr_used[i]  
  
  nbin<-4
  
  if(db=="BioTIMEx"){
    resloc_output<-here(paste("Results/for_BioTIMEx/",sid,"/",ns,"/nbin_",nbin,sep=""))
  } 
  
  resloc_output<-paste(resloc_output,"/",sep="")
  zz<-readRDS(paste(resloc_output,"NonParamStat.RDS",sep=""))
  z<-zz$Corl - zz$Coru
  indI<-zz$posnI
  indI<-which(indI==1,arr.ind = T)
  z[indI]<-NA
  diag(z)<-NA
  
  tot_target_sp<-length(setdiff(colnames(z),c("t","tmax","tmin","tmax_n","year")))
  
  z1<-z[1:tot_target_sp,1:tot_target_sp]
  z1posnI<-zz$posnI[1:tot_target_sp,1:tot_target_sp]
  z1posnN<-zz$posnN[1:tot_target_sp,1:tot_target_sp]
  z1_posnI_ind<-which(z1posnI==1, arr.ind=T)
  z1_posnN_ind<-which(z1posnN==1, arr.ind=T)
  z1[z1_posnN_ind]<-NA # this line was added to exclude -vely correlated species pair from nL,nU,L,U 
  # calculation, but it does not matter as for -vely correlated cells [sp_i,sp_j] and 
  # [sp_j,sp_i] nL,nU both will increase by same number, whereas L+U remains same and
  # both L, U will change by same +, - factor
  
  z1[upper.tri(z1)]<-NA # symmetric matrix, so consider only lower triangular part
  
  #nL<-sum(z1>0,na.rm = T)
  #nU<-sum(z1<0,na.rm = T)
  L_nbin4<-sum(z1[which(z1>0,arr.ind=T)])
  U_nbin4<-sum(z1[which(z1<0,arr.ind=T)])
  sbf40$L_nbin4[i]<-L_nbin4
  sbf40$U_nbin4[i]<-U_nbin4
}
sbf40$A_nbin4<- sbf40$L_nbin4 + abs(sbf40$U_nbin4)

g2<-ggplot(sbf40,aes(x=A,y=A_nbin3))+geom_point(col="blue",alpha=0.3)+
  geom_smooth(method="lm",se=F)+theme_bw()+
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 1.9,col="blue")+
  stat_regline_equation(label.x = 0.1, label.y = 2,col="blue")+
  geom_point(col="red",alpha=0.3, data=sbf40, aes(x=A,y=A_nbin4))+
  geom_smooth(method="lm",se=F,col="red",data=sbf40, aes(x=A,y=A_nbin4))+
  stat_cor(data=sbf40, aes(x=A,y=A_nbin4,label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 1.6,col="red")+
  stat_regline_equation(data=sbf40, aes(x=A,y=A_nbin4),label.x = 0.1, label.y = 1.7,col="red")+
  xlab("Community-level tail-dependent synchrony, A \n for nbin=2")+
  ylab("Community-level tail-dependent synchrony, A \n for nbin=3 and nbin=4")+
  ggtitle(label="24 communities with minimum 40 years of data")
#g2

resloc<-here("Results/res_Prelim_Report")
pdf((paste(resloc,"/plot_sensitivity_tails.pdf",sep="")),height=4,width=9)
grid.arrange(g1,g2,nrow=1)
dev.off()



