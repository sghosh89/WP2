# make a similar intro fig timeseries example plot but with real data
#rm(list=ls())
library(here)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(trend)
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sbf<-sm_all%>%filter(TAXA%in%c("fish","birds"))%>%
  dplyr::select(source,STUDY_ID,newsite,REALM,TAXA,
                initR,nsp,nyr_used,L,U,phi_LdM,iCValt,
                LONGITUDE,LATITUDE,
                t_med,t_skw,trend_t_tau,trend_t_tau_sig,
                t.lm.slope,t.lm.slope.sig,
                t.sens.slope,t.sens.slope.sig)

#================= first do some exploration ===============================
tempo<-sbf%>%filter(t.lm.slope.sig!=t.sens.slope.sig)# 193 communities
# have mismatch significance level based on linear regression and sen's method
# plot one of such timeseries
range(tempo$nyr_used)

# one example with 41 datapoints
m<-readRDS("../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/input_mat_for_tailanal_with_env_oneida_fish_gillnets_Buoy 113.RDS")
m$year<-as.integer(rownames(m))
plot(m$year,m$t,type="b")
model<-lm(t~year,m)
summary(model) # marginal p value, sig in lm fit
sens.slope(m$t) # non sig in sens method

# another example with 20 datapoints
m<-readRDS("../DATA/for_BBS/wrangled_data/124_4_212/input_mat_for_tailanal_with_env.RDS")
m$year<-as.integer(rownames(m))
plot(m$year,m$t,type="b")
model<-lm(t~year,m)
summary(model) # marginal p value, non-sig in lm fit
sens.slope(m$t) # sig in sens method

# in this way, I feel for shorter timeseries sens method may be more effective in detecting a trend

# see another example, where both method estimates are similar but differ in significance
m<-readRDS("../DATA/for_BBS/wrangled_data/840_14_186/input_mat_for_tailanal_with_env.RDS")
m$year<-as.integer(rownames(m))
plot(m$year,m$t,type="b")
model<-lm(t~year,m)
summary(model) # marginal p value, sig in lm fit
sens.slope(m$t) # non-sig in sens method
#=====================================================================

# choose data: no trend, no skewness
tempo<-sbf%>%filter(t.lm.slope.sig==0 & t.sens.slope.sig==0)
eps=0.1e-2
id<-which(abs(tempo$t_skw-0)<eps)
tempo<-tempo[id,]# choose the first, second entry

tempo_ab<-tempo[c(1,2),]

# choose data: no trend, -ve sig. skewness
tempo<-sbf%>%filter(t.lm.slope.sig==0 & t.sens.slope.sig==0)
tempo_c<-tempo[which.min(tempo$t_skw),]

# choose data: +ve trend, no sig. skewness
tempo<-sbf%>%filter(t.lm.slope.sig==1 & t.sens.slope.sig==1)
eps<-1e-3
tempo_d<-tempo[abs(tempo$t_skw-0)<eps,]
tempo_d<-tempo_d[2,]# choose the second entry

# choose data: +ve trend, with +ve skewness
tempo<-sbf%>%filter(t.lm.slope.sig==1 & t.sens.slope.sig==1)
id<-which.max(tempo$t_skw)
tempo_e<-tempo[id,]

alldat<-rbind(tempo_ab,tempo_c,tempo_d,tempo_e)
gp<-vector(mode = "list", length = nrow(alldat))
# now plot t_med time series for these five communities
for(i in 1:nrow(alldat)){
  
  if(alldat$source[i]=="RivFishTIME"){
    resloc<-here(paste("DATA/for_RivFishTIME/wrangled_data/",alldat$newsite[i],sep=""))
  }
  if(alldat$source[i]=="BBS"){
    resloc<-here(paste("DATA/for_BBS/wrangled_data/",alldat$newsite[i],sep=""))
  }
  #if(alldat$source[i]=="BioTIME" & alldat$newsite[i]==57){
  #  resloc<-here(paste("DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",alldat$newsite[i],sep=""))
  #}
  #if(alldat$source[i]=="BioTIME" & alldat$STUDY_ID[i]==430){
  #  resloc<-here(paste("DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/",alldat$STUDY_ID[i],"/",alldat$newsite[i],sep=""))
  #}
  m<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env.RDS",sep=""))
  m$year<-as.integer(rownames(m))
  gp[[i]]<-ggplot(m,aes(x=year,y=t))+
    geom_point(col="gray")+
    geom_line(col="gray")+
    geom_hline(yintercept = alldat$t_med[i],linetype = "dashed")+
    #geom_smooth(method="glm",fill="skyblue")+
    xlab("Year")+ylab("Temperature")+
    annotate(geom = 'text', 
             label = paste("lm.slope= ",round(alldat$t.lm.slope[i],4),
                           " ,Sens'slope= ",round(alldat$t.sens.slope[i],4),
                           " ,slope.sig = ",alldat$t.lm.slope.sig[i],
                           ",\n skw= ",round(alldat$t_skw[i],4),sep=""), 
             x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2)+
    theme_bw()
}
resloc<-here("Results/some_assets")

g0<-paste(resloc,"/realdata_temperature_timeseries.jpg",sep="")
g1<-grid.arrange(gp[[1]],gp[[2]],gp[[3]],gp[[4]],gp[[5]],nrow=2)
g<-arrangeGrob(g1)
ggsave(g0, g,width = 40, height = 12, units = 'cm')
