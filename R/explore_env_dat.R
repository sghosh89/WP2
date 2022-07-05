##########################################################################################################
# This code is to visualize the temporal trend of environmental time series going to be used in analysis
##########################################################################################################
rm(list=ls())
library(tidyverse)
library(e1071)

# first read all the data/metadata collected for proj BioDyn
s<-read.csv("../Results/data_summary_with_temperature_yrspan_1979_2019.csv")
# histogram of temp
t<-read.csv("../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv")
t$lonlat<-paste(t$CENT_LONG,"_",t$CENT_LAT,sep="")
length(unique(t$lonlat)) #1891
tab<-t%>%group_by(lonlat)%>%summarise(skw=skewness(meanval,type=2,na.rm = T))%>%ungroup()
hist(tab$skw,50,main="skewness of annual Temp timeseries from all unq. lonlat", cex.main=0.8)
# so sites have both positive (cooling), and negative (warming) skewness
# rule of thumb: -0.5<skw<0.5 = not considered as sig skewed, 
# beyond that limit it is considered as skewed


# randomly choose 10 sites
set.seed(seed=101)
tg_site<-sample(unique(t$lonlat),100,replace=F)
t_sub<-t%>%filter(lonlat%in%tg_site)
ggplot(t_sub,aes(x=yr,y=meanval))+geom_line()+facet_wrap(~lonlat)+
  #geom_smooth(method="lm",se=F)+
  theme_bw()

tab2<-t%>%group_by(yr)%>%summarise(mt=mean(meanval))%>%ungroup()
ggplot(tab2,aes(x=yr,y=mt))+geom_line()+
  geom_smooth(method="lm",se=F)+
  theme_bw() # an overall increasing trend in temp aggregated over all sites
# so if we believe there is no such skewness effect at least we need to 
# capture this trend (as slope of linear fit? but it could be nonlinear) as a 
# driver in the model, not just the median




