# I am testing a toy script with BioTIME data (downloaded on 15/06/2022)
rm(list=ls())
library(tidyverse)

# first read all the data/metadata collected for proj BioDyn
s<-read.csv("../Results/data_summary_with_temperature_yrspan_1979_2019.csv")
#s<-read.csv("../Results/data_summary_with_env_yrspan_1979_2018.csv")
s$UID<-paste(s$source,s$STUDY_ID,sep=",")
s$dummy3<-paste(s$source,s$dummy,sep=",")
s$A<-s$L+abs(s$U) # total asymmetry
s<-s%>%dplyr::rename(
  stability=iCValt,
  VR=phi_LdM,
  R=nsp)

as.data.frame(table(s$TAXA))

#-------------------------------------

# how many newsites per UID
t1<-s%>% group_by(TAXA)%>%count(UID)%>%ungroup()%>%arrange(UID)

# first check whether lonlat are same per UID?
t2<-s%>% group_by(TAXA, UID)%>%count(dummy3)%>%ungroup()%>%arrange(UID)

# is there any lon_lat where you have two plots from same study_ID
t3<-s%>% group_by(dummy)%>%
  summarise(n=n_distinct(UID))%>%ungroup() # but they are coming from different taxa
# so if you are doing individual model for each taxa, u can use dummy as random factor
any(t3$n>1)

t2.2<-t2%>%filter(n<10)
as.data.frame(table(t2.2$TAXA))

s<-s%>%filter(TAXA%in%c("fish"))



car::vif(lmer(stability ~ (R + VR +A + t_skw)+(1|UID), data=s))

#model_group<-piecewiseSEM::multigroup(model_full,group="TAXA")
coefs(model_full)
plot(model_full,layout="circle")

#library(semEff)
#es<-bootEff(model_full, R = 5, ran.eff = "UID", parallel = "no")
#summary(es)

###########################
#library(nlme)
#library(lme4)
#library(piecewiseSEM)

# start with sT
#sT$TAXA<-as.factor(sT$TAXA)
#sT<-sT%>%filter(TAXA=="birds")
#model_full<-psem(
#  lmer(VR_LdM ~ (R+t_median)*TAXA+(1|STUDY_ID),data=sT),
#  lmer(A ~ (R+t_median)*TAXA+(1|STUDY_ID),data=sT),
#  lmer(R~t_median*TAXA+(1|STUDY_ID),data=sT),
#  lmer(stability_skw ~ (R + VR_LdM +A + t_median)*TAXA+(1|STUDY_ID), data=sT))

#=========== without random effect groupwise model ============
#model_full<-psem(
#  lm(VR_LdM ~ (R+t_median),data=sT),
#  lm(A ~ R+t_median,data=sT),
#  lm(R~t_median,data=sT),
#  lm(stability_skw ~ (R + VR_LdM +A + t_median), data=sT))
#summary(model_full)

#plot(model_full,layout="circle")
#class(model_full)
# now add habitat effect
#model_group<-piecewiseSEM::multigroup(model_full,
#                                      group="TAXA")
#model_group

###################################


#########################################
#######################################################################
# For each taxa, separately make mixed effect model in piecewiseSEM
#######################################################################

rm(list=ls())
library(tidyverse)
library(lme4)
library(piecewiseSEM)
source("plot_psem.R")

# first read all the data/metadata collected for proj BioDyn
# consider only Temperature effect to include data until 2019
s<-read.csv("../Results/data_summary_with_temperature_yrspan_1979_2019.csv")
#s<-read.csv("../Results/data_summary_with_env_yrspan_1979_2018.csv")
s$UID<-paste(s$STUDY_ID,s$dummy,sep="_")
s$A<-s$L+abs(s$U) # total asymmetry
s<-s%>%dplyr::rename(
  stability=iCValt,
  VR=phi_LdM,
  R=nsp)

as.data.frame(table(s$TAXA))

suq<-dplyr::distinct(s,dummy2,.keep_all=T) #1802 unique lon-lat
as.data.frame(table(suq$TAXA))
# maybe it's good to only consider the unique lonlat

s$TAXA<-as.factor(s$TAXA)
############ want rescaling? #################
mydat_scaled<-s
mydat_scaled$stability<-as.numeric(scale(mydat_scaled$stability))
mydat_scaled$R<-as.numeric(scale(mydat_scaled$R))
mydat_scaled$VR<- as.numeric(scale(mydat_scaled$VR))
mydat_scaled$A<- as.numeric(scale(mydat_scaled$A))

mydat_scaled$t_median<-as.numeric(scale(mydat_scaled$t_median))
mydat_scaled$t_skw<-as.numeric(scale(mydat_scaled$t_skw))
mydat_scaled$t_var<-as.numeric(scale(mydat_scaled$t_var))

mydat_scaled$tmax_median<-as.numeric(scale(mydat_scaled$tmax_median))
mydat_scaled$tmax_skw<-as.numeric(scale(mydat_scaled$tmax_skw))
mydat_scaled$tmax_var<-as.numeric(scale(mydat_scaled$tmax_var))

mydat_scaled$tmin_median<-as.numeric(scale(mydat_scaled$tmin_median))
mydat_scaled$tmin_skw<-as.numeric(scale(mydat_scaled$tmin_skw))
mydat_scaled$tmin_var<-as.numeric(scale(mydat_scaled$tmin_var))

#mydat_scaled$pr_median<-as.numeric(scale(mydat_scaled$pr_median))
#mydat_scaled$pr_skw<-as.numeric(scale(mydat_scaled$pr_skw))
#mydat_scaled$pr_var<-as.numeric(scale(mydat_scaled$pr_var))

s<-mydat_scaled

taxa<-"terrestrial plants"
dat<-s%>%filter(TAXA==taxa)
n<-nrow(dat)
model_psem<-piecewiseSEM::psem(
  lm(VR ~ R+t_skw,data=dat),
  lm(A ~ R+t_skw,data=dat),
  lm(R~t_skw,data=dat),
  lm(stability ~ (R + VR +A)+t_skw+R:t_skw+A:t_skw+VR:t_skw, data=dat))

cc<-coefs(model_psem)
cc
plot_psem(n,taxa,cc,layout="circle")






