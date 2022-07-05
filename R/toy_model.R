rm(list=ls())
library(tidyverse)
library(brms)

call_toy_model<-function(resloc, data, taxa){
  
  sink(paste(resloc,"/console_bayesian_model_tskw_",taxa,".txt",sep=""),
       append=TRUE, split=TRUE)
  
 
  cat(paste("------- brms model with R, A, VR_LdM, env variable starting at time: ", Sys.time()," -------------- \n "))
  
  # all taxa model
  #bf_0<-bf(stability_skw ~ (R+A+VR_LdM+t_median)*TAXA+(1|UID))
  #bf_1<-bf(R~t_median*TAXA +(1|UID))
  #bf_2<-bf(VR_LdM~t_median*TAXA +(1|UID))
  #bf_3<-bf(A~t_median*TAXA +(1|UID))
  
  # each taxa model
  bf_0<-bf(stability ~ (R + VR +A)+t_skw+
             R:t_skw+A:t_skw+VR:t_skw+
             (1|STUDY_ID))
  bf_1<-bf(R~t_skw+(1|STUDY_ID))
  bf_2<-bf(VR~R+t_skw+(1|STUDY_ID))
  bf_3<-bf(A~R+t_skw+(1|STUDY_ID))
  
  bf_stability<-bf_0+bf_1+bf_2+bf_3
  
  full_model<-brm(bf_stability+set_rescor(F),
                  data=data,
                  family = gaussian(),
                  chains=4,cores=4,iter=12000,
                  warmup=6000,inits="random",thin=4,
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  save_pars = save_pars(all = TRUE),seed=123)
  
  print(summary(full_model),digits = 3)
  saveRDS(full_model,paste(resloc,"/full_model_tskw_",taxa,".RDS",sep=""))
  
  sink()
  
}
##########################################

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
##################################
# raw data plot 
s<-s%>%filter(TAXA%in%c("birds","fish","terrestrial invertebrates","freshwater invertebrates"))
ggplot(data=s, aes(x=t_median))+geom_density()+facet_wrap(~TAXA, ncol=2)+theme_bw()
ggplot(data=s, aes(x=t_skw))+geom_density()+facet_wrap(~TAXA, ncol=2)+theme_bw()
ggplot(data=s, aes(x=t_var))+geom_density()+facet_wrap(~TAXA, ncol=2)+theme_bw()

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

resloc<-"./../Results/toy_model_brms/"
taxa<-"freshwater invertebrates"
data<-s%>%filter(TAXA==taxa)
call_toy_model(resloc,data=data,taxa)


