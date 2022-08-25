# first for birds
library(tidyverse)
library(dplyr)
sm_all<-read.csv("./../Results/stability_metric_and_env_all.csv")
taxa<-"birds"
sb<-sm_all%>%filter(TAXA==taxa)

# all cor with temperature
df<-read.csv("../Results/birds_splist_with_temp_sensitivity.csv")

# consistency table
dfc<-read.csv("../Results/birds_splist_consistency_table.csv")

# birdtraits
dft<-read.csv("../DATA/traitsdata/bird_traits_from_AVONET.csv")
length(unique(dft$Avibase.ID))#536
t1<-dft%>%group_by(Avibase.ID)%>%summarise(nRecords=n(),# number of records for that sp
                                           nSexM=sum(Sex=="M"),# number of records for male sp
                                           nSexF=sum(Sex=="F"),# number of records for female sp
                                           nSexU=sum(Sex=="U"),# number of records for unisex sp
                                           nWL=sum(!is.na(Wing.Length)), # number of recorded wing length
                                           nHWI=sum(!is.na(Hand.wing.Index)# number of recorded HWI
                                                    ))%>%ungroup()
t1<-t1[order(t1$nHWI),]
which(t1$nSexF>t1$nSexM & t1$nSexF>t1$nSexU)

# group by traits for each species (M/F/U all together)
dft_summarised<-dft%>%group_by(Avibase.ID,spname)%>%summarise(HWI=mean(Hand.wing.Index,na.rm=T))%>%
  ungroup()%>%filter(!is.na(HWI))

length(unique(dft_summarised$spname))#536 unique spname from BBS,
length(unique(dft_summarised$Avibase.ID))# but actually 521 sp based on unique Avibase.ID

# check if with increasing temp, there any change in birds dispersal ability?
df_allspHWI<-left_join(df,dft_summarised,by="spname")
df_allspHWI<-na.omit(df_allspHWI)
df_allspHWI_persite<-df_allspHWI%>%group_by(STUDY_ID,newsite)%>%summarise(meanHWI=mean(HWI))%>%ungroup()
sb<-left_join(sb,df_allspHWI_persite,by=c("newsite","STUDY_ID"))

##############################################################################################

#hist(sb$trend_t_tau)
#plot(sb$trend_t_tau,sb$meanHWI,col=rgb(0,0,0,0.2),pch=19)

############################################################################################
# temperature: low vs high in community and how traits like dispersal ability vary?
q_T_taxa<-sb%>%summarise(q=quantile(t_med,c(0.25,0.75)))

lowT_taxa<-sb%>%filter(t_med<=unname(q_T_taxa[1,1]))
highT_taxa<-sb%>%filter(t_med>=unname(q_T_taxa[2,1]))
lowT_taxa$type<-"Low T,<50%CI"
highT_taxa$type<-"High T,>50%CI"

compare_data<-rbind(lowT_taxa,highT_taxa)
compare_data$type<-as.factor(compare_data$type)

g1<-ggplot(compare_data, aes(x = type, y = meanHWI, color = type)) + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black")+
  geom_jitter(width = .02,alpha=0.3)+
  theme_bw()+theme(legend.position="none")+ 
  ylab("Birds' mean traits, HWI")+xlab("Community in environment")
print(g1)

g2<-ggplot(compare_data, aes(x = type, y = nsp, color = type)) + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black")+
  geom_jitter(width = .02,alpha=0.3)+
  theme_bw()+theme(legend.position="none")+ 
  ylab("Birds' richness, body length in cm")+xlab("Community in environment")
print(g2)




# mean traits
#library(PupillometryR)
#gs<-ggplot(data = compare_data, aes(y = meanHWI, x = type, fill = type)) +
#  scale_fill_manual(values=alpha(c("red","blue"), 1))+
#  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
#  geom_point(aes(y = meanHWI, color = type), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
#  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
#  ylab("Birds' mean traits, HWI")+xlab("Community in environment")+
#  theme_bw()+
#  theme(
#    panel.background=element_rect(fill="white", colour="white"), 
#    legend.position="none",text=element_text(size=20))+
#  scale_color_manual(values=alpha(c("red","blue"), 1))

#library(tidyverse)
#library(lme4)
#library(piecewiseSEM)
#source("plot_psem.R")

#sb$UID<-paste(sb$source,sb$STUDY_ID,sep="_")
#sb$A<-sb$L+abs(sb$U) # total asymmetry
#sb<-sb%>%dplyr::rename(
#  stability=iCValt,
#  VR=phi_LdM,
#  R=nsp)

############ want rescaling? #################
#mydat_scaled<-sb
#mydat_scaled$stability<-as.numeric(scale(mydat_scaled$stability))
#mydat_scaled$R<-as.numeric(scale(mydat_scaled$R))
#mydat_scaled$VR<- as.numeric(scale(mydat_scaled$VR))
#mydat_scaled$A<- as.numeric(scale(mydat_scaled$A))

#mydat_scaled$t_med<-as.numeric(scale(mydat_scaled$t_med))
#mydat_scaled$tmax_med<-as.numeric(scale(mydat_scaled$tmax_med))
#mydat_scaled$tmin_med<-as.numeric(scale(mydat_scaled$tmin_med))

#mydat_scaled$t_skw<-as.numeric(scale(mydat_scaled$t_skw))
#mydat_scaled$tmax_skw<-as.numeric(scale(mydat_scaled$tmax_skw))
#mydat_scaled$tmin_skw<-as.numeric(scale(mydat_scaled$tmin_skw))
#mydat_scaled$t_var<-as.numeric(scale(mydat_scaled$t_var))
#mydat_scaled$meanHWI<-as.numeric(scale(mydat_scaled$meanHWI))
#---  

#cat("=========================== taxa = birds =============================== \n")
#dat<-mydat_scaled
#n<-nrow(dat)
#model_psem<-piecewiseSEM::psem(
#  lmer(VR ~ R+t_med+trend_t_tau+(1|UID),data=dat),
#  lmer(A ~ R+t_skw+(1|UID),data=dat),
#  lmer(R~t_med+trend_t_tau+(1|UID),data=dat),
#  lmer(meanHWI~t_med+trend_t_tau+(1|UID),data=dat),
#  lmer(stability ~ (R + VR +A)+meanHWI+t_med+trend_t_tau+t_skw+
#         (1|UID), data=dat))
#cc<-coefs(model_psem)
#plot_psem(n,taxa,cc,layout="circle")






