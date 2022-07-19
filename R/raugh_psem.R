#######################################################################
# For each taxa, separately make mixed effect model in piecewiseSEM
# NOTE >5 levels required for practical purpose to add random effects
#######################################################################
rm(list=ls())
library(tidyverse)
library(lme4)
library(piecewiseSEM)
source("plot_psem.R")

s<-read.csv("../Results/stability_metric_and_env_all.csv")
s$UID<-paste(s$source,s$STUDY_ID,sep="_")
s$A<-s$L+abs(s$U) # total asymmetry
s<-s%>%dplyr::rename(
  stability=iCValt,
  VR=phi_LdM,
  R=nsp)

as.data.frame(table(s$TAXA))

s$TAXA<-as.factor(s$TAXA)

############ want rescaling? #################
mydat_scaled<-s
mydat_scaled$stability<-as.numeric(scale(mydat_scaled$stability))
mydat_scaled$R<-as.numeric(scale(mydat_scaled$R))
mydat_scaled$VR<- as.numeric(scale(mydat_scaled$VR))
mydat_scaled$A<- as.numeric(scale(mydat_scaled$A))

mydat_scaled$t_med<-as.numeric(scale(mydat_scaled$t_med))
mydat_scaled$tmax_med<-as.numeric(scale(mydat_scaled$tmax_med))
mydat_scaled$tmin_med<-as.numeric(scale(mydat_scaled$tmin_med))

mydat_scaled$t_skw<-as.numeric(scale(mydat_scaled$t_skw))
mydat_scaled$tmax_skw<-as.numeric(scale(mydat_scaled$tmax_skw))
mydat_scaled$tmin_skw<-as.numeric(scale(mydat_scaled$tmin_skw))

##################################

resloc<-"./../Results/raugh_psem_res/"

# =========== for birds ==================
sink(paste(resloc,"/console_psem_tmed_tskw_.txt",sep=""),
     append=TRUE, split=TRUE)

#sink(paste(resloc,"/console_psem_tskw_.txt",sep=""),
#     append=TRUE, split=TRUE)
cat("=========================== taxa = birds =============================== \n")
taxa<-"freshwater invertebrates"
dat<-mydat_scaled%>%filter(TAXA==taxa)
n<-nrow(dat)

model_psem<-piecewiseSEM::psem(
  lmer(VR ~ R+t_med+(1|UID),data=dat),
  lmer(A ~ R+t_med+(1|UID),data=dat),
  lmer(R~t_med+(1|UID),data=dat),
  lmer(stability ~ (R + VR +A)+t_med+
         R:t_med+ A:t_med+ VR:t_med+
         (1|UID), data=dat))


cc<-coefs(model_psem)

print(summary(model_psem))
saveRDS(model_psem,paste(resloc,"model_psem_tmed_",taxa,".RDS",sep=""))
pdf(paste(resloc,"plot_psem_tmed_",taxa,".pdf",sep=""),width=5,height=3)
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
  lmer(VR ~ R+t_med+(1|UID),data=dat),
  lmer(A ~ R++t_med+(1|UID),data=dat),
  lmer(R~+t_med+(1|UID),data=dat),
  lmer(stability ~ (R + VR +A)++t_med+
         R:+t_med+ A:+t_med+ VR:+t_med+
         (1|UID), data=dat))

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










