#######################################################################
# For each taxa, separately make mixed effect model in piecewiseSEM
# NOTE >5 levels required for practical purpose to add random effects
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
##################################

resloc<-"./../Results/raugh_psem_res/"

# =========== for birds ==================
sink(paste(resloc,"/console_psem_tmed_tskw_.txt",sep=""),
     append=TRUE, split=TRUE)

#sink(paste(resloc,"/console_psem_tskw_.txt",sep=""),
#     append=TRUE, split=TRUE)
cat("=========================== taxa = birds =============================== \n")
taxa<-"birds"
dat<-s%>%filter(TAXA==taxa)
n<-nrow(dat)

model_psem<-piecewiseSEM::psem(
  lmer(VR ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(A ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(R~t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(stability ~ (R + VR +A)+t_skw+t_median+
         R:t_skw+ A:t_skw+ VR:t_skw+
         R:t_median+ A:t_median+ VR:t_median+
         (1|STUDY_ID), data=dat))

#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_median+(1|STUDY_ID),data=dat),
#  lmer(A ~ R+t_median+(1|STUDY_ID),data=dat),
#  lmer(R~t_median+(1|STUDY_ID),data=dat),
#  lmer(stability ~ (R + VR +A)+t_median+
#         R:t_median+ A:t_median+ VR:t_median+
#         (1|STUDY_ID), data=dat))

#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(A ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(R~t_skw+(1|STUDY_ID),data=dat),
#  lmer(stability ~ (R + VR +A)+t_skw+
#         R:t_skw+A:t_skw+VR:t_skw+
#         (1|STUDY_ID), data=dat))

cc<-coefs(model_psem)
print(summary(model_psem))
saveRDS(model_psem,paste(resloc,"model_psem_tmed_tskw_",taxa,".RDS",sep=""))
pdf(paste(resloc,"plot_psem_tmed_tskw_",taxa,".pdf",sep=""),width=5,height=3)
#pdf(paste(resloc,"plot_psem_tskw_",taxa,".pdf",sep=""),width=5,height=3)
op<-par(mar=c(0.1,0.1,0.1,0.1))
plot_psem(n,taxa,cc,layout="circle")
par(op)
dev.off()

cat("=========================== taxa = fish =============================== \n")
taxa<-"fish"
dat<-s%>%filter(TAXA==taxa)
n<-nrow(dat)

model_psem<-piecewiseSEM::psem(
  lmer(VR ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(A ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(R~t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(stability ~ (R + VR +A)+t_skw+t_median+
         R:t_skw+ A:t_skw+ VR:t_skw+
         R:t_median+ A:t_median+ VR:t_median+
         (1|STUDY_ID), data=dat))

#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(A ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(R~t_skw+(1|STUDY_ID),data=dat),
#  lmer(stability ~ (R + VR +A)+t_skw+
#         R:t_skw+A:t_skw+VR:t_skw+
#         (1|STUDY_ID), data=dat))

cc<-coefs(model_psem)
print(summary(model_psem))
saveRDS(model_psem,paste(resloc,"model_psem_tmed_tskw_",taxa,".RDS",sep=""))
pdf(paste(resloc,"plot_psem_tmed_tskw_",taxa,".pdf",sep=""),width=5,height=3)
#pdf(paste(resloc,"plot_psem_tskw_",taxa,".pdf",sep=""),width=5,height=3)
op<-par(mar=c(0.1,0.1,0.1,0.1))
plot_psem(n,taxa,cc,layout="circle")
par(op)
dev.off()

cat("=========================== taxa = terrestrial invertebrates =============================== \n")
taxa<-"terrestrial invertebrates"
dat<-s%>%filter(TAXA==taxa)
n<-nrow(dat)

model_psem<-piecewiseSEM::psem(
  lmer(VR ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(A ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(R~t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(stability ~ (R + VR +A)+t_skw+t_median+
         R:t_skw+ A:t_skw+ VR:t_skw+
         R:t_median+ A:t_median+ VR:t_median+
         (1|STUDY_ID), data=dat))

#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(A ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(R~t_skw+(1|STUDY_ID),data=dat),
#  lmer(stability ~ (R + VR +A)+t_skw+
#         R:t_skw+A:t_skw+VR:t_skw+
#         (1|STUDY_ID), data=dat))

cc<-coefs(model_psem)
print(summary(model_psem))
saveRDS(model_psem,paste(resloc,"model_psem_tmed_tskw_",taxa,".RDS",sep=""))
pdf(paste(resloc,"plot_psem_tmed_tskw_",taxa,".pdf",sep=""),width=5,height=3)
#pdf(paste(resloc,"plot_psem_tskw_",taxa,".pdf",sep=""),width=5,height=3)
op<-par(mar=c(0.1,0.1,0.1,0.1))
plot_psem(n,taxa,cc,layout="circle",red_arr = 15,red_edge = 10) # only t_skw 
par(op)
dev.off()

cat("=========================== taxa = freshwater invertebrates =============================== \n")
taxa<-"freshwater invertebrates"
dat<-s%>%filter(TAXA==taxa)
n<-nrow(dat)

model_psem<-piecewiseSEM::psem(
  lmer(VR ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(A ~ R+t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(R~t_skw+t_median+(1|STUDY_ID),data=dat),
  lmer(stability ~ (R + VR +A)+t_skw+t_median+
         R:t_skw+ A:t_skw+ VR:t_skw+
         R:t_median+ A:t_median+ VR:t_median+
         (1|STUDY_ID), data=dat))

#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(A ~ R+t_skw+(1|STUDY_ID),data=dat),
#  lmer(R~t_skw+(1|STUDY_ID),data=dat),
#  lmer(stability ~ (R + VR +A)+t_skw+
#         R:t_skw+A:t_skw+VR:t_skw+
#         (1|STUDY_ID), data=dat))

cc<-coefs(model_psem)
print(summary(model_psem))
saveRDS(model_psem,paste(resloc,"model_psem_tmed_tskw_",taxa,".RDS",sep=""))
#saveRDS(model_psem,paste(resloc,"model_psem_tskw_",taxa,".RDS",sep=""))
pdf(paste(resloc,"plot_psem_tmed_tskw_",taxa,".pdf",sep=""),width=5,height=3)
#pdf(paste(resloc,"plot_psem_tskw_",taxa,".pdf",sep=""),width=5,height=3)
op<-par(mar=c(0.1,0.1,0.1,0.1))
plot_psem(n,taxa,cc,layout="circle")
par(op)
dev.off()
sink()

#car::vif(lmer(stability ~ (R + VR +A + t_var+t_skw)+
#                (1|STUDY_ID), data=dat))

# standardized path coeff. can be >1










