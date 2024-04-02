
#---------------------PLOT IN CELCIUS SCALE---------------------------------

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
                t_med,t_skw,t_var,
                t_med_celsius,t_skw_celsius,t_varIQR_celsius,
                t.lm.slope,t.lm.slope.sig,
                t.sens.slope,t.sens.slope.sig,
                t.lm.slope.celsius,t.lm.slope.sig.celsius,
                t.sens.slope.celsius,t.sens.slope.sig.celsius)

#================= first do some exploration ===============================
tempo<-sbf%>%filter(t.lm.slope.sig!=t.sens.slope.sig)# 193 communities
# have mismatch significance level based on linear regression and sen's method
# plot one of such timeseries
range(tempo$nyr_used)

# one example with 41 datapoints
m<-readRDS("../DATA/for_BioTIMEx/wrangled_data/oneida_fish_gillnets/input_mat_for_tailanal_with_env_celcius_oneida_fish_gillnets_Buoy 113.RDS")
m$year<-as.integer(rownames(m))

plot(m$year,m$t_in_celcius,type="b")
model<-lm(t_in_celcius~year,m)
summary(model) # marginal p value, sig in lm fit
sens.slope(m$t_in_celcius) # non sig in sens method

# another example with 20 datapoints
m<-readRDS("../DATA/for_BBS/wrangled_data/124_4_212/input_mat_for_tailanal_with_env_celcius.RDS")
m$year<-as.integer(rownames(m))
plot(m$year,m$t_in_celcius,type="b")
model<-lm(t_in_celcius~year,m)
summary(model) # marginal p value, non-sig in lm fit
sens.slope(m$t_in_celcius) # sig in sens method

# in this way, I feel for shorter timeseries sens method may be more effective in detecting a trend

# see another example, where both method estimates are similar but differ in significance
m<-readRDS("../DATA/for_BBS/wrangled_data/840_14_186/input_mat_for_tailanal_with_env_celcius.RDS")
m$year<-as.integer(rownames(m))
plot(m$year,m$t_in_celcius,type="b")
model<-lm(t_in_celcius~year,m)
summary(model) # marginal p value, sig in lm fit
sens.slope(m$t_in_celcius) # non-sig in sens method
#=====================================================================

# choose data: no trend, no skewness
tempo<-sbf%>%filter(t.lm.slope.sig==0 & t.sens.slope.sig.celsius==0)
eps=0.1e-2
id<-which(abs(tempo$t_skw_celsius-0)<eps)
tempo<-tempo[id,]# choose the first, second entry

tempo_ab<-tempo[c(1,2),]

# choose data: no trend, -ve sig. skewness
tempo<-sbf%>%filter(t.lm.slope.sig==0 & t.sens.slope.sig.celsius==0)
tempo_c<-tempo[which.min(tempo$t_skw_celsius),]

# choose data: +ve trend, no sig. skewness
tempo<-sbf%>%filter(t.lm.slope.sig==1 & t.sens.slope.sig.celsius==1)
eps<-1e-3
tempo_d<-tempo[abs(tempo$t_skw_celsius-0)<eps,]
tempo_d<-tempo_d[2,]# choose the second entry

# choose data: +ve trend, with +ve skewness
tempo<-sbf%>%filter(t.lm.slope.sig==1 & t.sens.slope.sig.celsius==1)
id<-which.max(tempo$t_skw_celsius)
tempo_e<-tempo[id,]

alldat<-rbind(tempo_ab,tempo_c,tempo_d,tempo_e)
gp<-vector(mode = "list", length = nrow(alldat))
titles<-c("No trend, no skewness", 
          "No trend, no skewness, but higher MedianT, lower VarT than B",
          "No trend, extreme cold (-ve skewness)",
          "Increasing trend, no skewness",
          "Increasing trend, heatwaves (+ve skewness)")
# now plot t_med time series for these five communities
for(i in 1:nrow(alldat)){
  
  if(alldat$source[i]=="RivFishTIME"){
    resloc<-here(paste("DATA/for_RivFishTIME/wrangled_data/",alldat$newsite[i],sep=""))
  }
  if(alldat$source[i]=="BBS"){
    resloc<-here(paste("DATA/for_BBS/wrangled_data/",alldat$newsite[i],sep=""))
  }
  
  m<-readRDS(paste(resloc,"/input_mat_for_tailanal_with_env_celcius.RDS",sep=""))
  m$year<-as.integer(rownames(m))
  gp[[i]]<-ggplot(m,aes(x=year,y=t_in_celcius))+
    geom_point(col="gray")+
    geom_line(col="gray")+ggtitle(paste(" TrendT= ",round(alldat$t.sens.slope.celsius[i],3),
                                        ", SkewT= ",round(alldat$t_skw_celsius[i],3),
                                        ", VarT= ",round(alldat$t_varIQR_celsius[i],3),sep=""))+
    geom_hline(yintercept = alldat$t_med_celsius[i],linetype = "dashed")+
    xlab("Year")+ylab("Temperature (\u00B0C)")+xlim(1997,2020)+
    annotate(geom = 'text', color="blue",
             label = titles[i], 
             x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+
    annotate("text",  x=Inf, y = Inf, label = LETTERS[i+1], vjust=1.2, hjust=1.2, size=10)+
    theme_bw()+
    theme(text = element_text(size = 16),axis.text = element_text(size = 16),
          plot.title = element_text(size=14),
          #plot.tag.position = c(0.1, 0.9),
          plot.margin = margin(t = 8, r = 15, b = 4, l = 4, unit = "pt"),
          panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
}
resloc<-here("Results/res_Prelim_Report/traditional_stability_res")

pdf(paste(resloc,"/realdata_temperature_timeseries_celcius.pdf",sep=""),
    height=9, width=6)
grid.arrange(gp[[1]],gp[[2]],gp[[3]],gp[[4]],gp[[5]],nrow=5) 
dev.off()
#==============================
