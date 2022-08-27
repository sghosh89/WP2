# make a similar intro fig timeseries example plot but with real data
#rm(list=ls())
library(here)
library(dplyr)
library(tidyverse)
library(gridExtra)
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sbf<-sm_all%>%filter(TAXA%in%c("fish","birds"))%>%
  dplyr::select(source,STUDY_ID,newsite,REALM,TAXA,
                initR,nsp,nyr_used,L,U,phi_LdM,iCValt,
                LONGITUDE,LATITUDE,
                t_med,t_skw,trend_t_tau,trend_t_tau_sig)

# choose data: no trend, no skewness
tempo<-sbf%>%filter(trend_t_tau_sig==0)
eps=0.2e-1
id<-which(abs(tempo$trend_t_tau-0)<eps & abs(tempo$t_skw-0)<eps)
tempo<-tempo[id,]# choose the first, ninth entry

tempo_ab<-tempo[c(1,9),]

# choose data: no trend, -ve sig. skewness
tempo<-sbf%>%filter(trend_t_tau_sig==0)
eps=0.2e-1
id<-which(abs(tempo$trend_t_tau-0)<eps & tempo$t_skw < -1.6)
tempo<-tempo[id,]# choose the fourth entry
tempo_c<-tempo[4,]

# choose data: +ve trend, no sig. skewness
tempo<-sbf%>%filter(trend_t_tau_sig==1)
eps=0.9e-1
id<-which(abs(tempo$t_skw-0)<eps)
tempo_d<-tempo[id,]# choose the fourth entry
tempo_d<-tempo_d[which.max(tempo_d$trend_t_tau),]

# choose data: +ve trend, with -ve skewness
tempo<-sbf%>%filter(trend_t_tau_sig==1)
id<-which.min(tempo$t_skw)
tempo_e<-tempo[id,]# choose the fourth entry
tempo_e<-tempo_e[which.max(tempo_e$trend_t_tau),]

# all fish data
alldat<-rbind(tempo_ab,tempo_c,tempo_d,tempo_e)
gp<-vector(mode = "list", length = nrow(alldat))
# now plot t_med time series for these five communities
for(i in 1:nrow(alldat)){
  if(alldat$source[i]=="RivFishTIME"){
    resloc<-here(paste("DATA/for_RivFishTIME/wrangled_data/",alldat$newsite[i],sep=""))
  }
  if(alldat$source[i]=="BioTIME"){
    resloc<-here(paste("DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",alldat$STUDY_ID[i],"/",alldat$newsite[i],sep=""))
  }
  m<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  m$year<-as.integer(rownames(m))
  gp[[i]]<-ggplot(m,aes(x=year,y=t))+
    geom_point()+
    geom_line()+
    geom_hline(yintercept = alldat$t_med[i],linetype = "dashed")+
    #geom_smooth(method="glm",fill="skyblue")+
    xlab("Year")+ylab("Temperature")+
    annotate(geom = 'text', 
             label = paste("trend= ",round(alldat$trend_t_tau[i],3),", skw= ",round(alldat$t_skw[i],3),sep=""), 
             x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2)+
    theme_bw()
}
resloc<-here("Results/some_assets")

g0<-paste(resloc,"/realdata_temperature_timeseries.jpg",sep="")
g1<-grid.arrange(gp[[2]],gp[[1]],gp[[3]],gp[[4]],gp[[5]],nrow=2)
g<-arrangeGrob(g1)
ggsave(g0, g,width = 30, height = 12, units = 'cm')
