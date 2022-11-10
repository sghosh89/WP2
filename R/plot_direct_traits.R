#########################################
# First for birds
#########################################
# first for birds
library(tidyverse)
library(dplyr)
library(here)
library(gridExtra)

sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
taxa<-"birds"
sb<-sm_all%>%filter(TAXA==taxa)

# all cor with temperature
df<-read.csv(here("Results/birds_splist_with_temp_sensitivity.csv"))

# consistency table
dfc<-read.csv(here("Results/birds_splist_consistency_table.csv"))

# birdtraits
dft<-read.csv(here("DATA/traitsdata/bird_traits_from_AVONET.csv"))
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
gtb1<-sb%>%ggplot(aes(x=t_med_celcius,y=meanHWI)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T,color="green",fill="greenyellow") + annotate("text",  x=Inf, y = Inf, label = "A", vjust=1.5, hjust=1.5,size=7)+
  ylab("Community-level mean HWI") +
  xlab("") +
  theme_bw()+ggtitle("Birds")
#print(gtb1)
ms<-summary(lm(formula = meanHWI ~ t_med_celcius, data = sb))
sl<-ms$coefficients[2,1] #slope
pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gtb1<-gtb1+
#  annotate("text",  x=8, y = 50, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=0, size=4)

gtb2<-sb%>%ggplot(aes(x=t.sens.slope.celcius,y=meanHWI)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T,color="green",fill="greenyellow") + annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5,size=7)+
  ylab("") +
  xlab("") +
  theme_bw()+ggtitle("Birds")
ms<-summary(lm(formula = meanHWI ~ t.sens.slope.celcius, data = sb))
sl<-ms$coefficients[2,1] #slope
pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gtb2<-gtb2+
#  annotate("text",  x=0, y = 50, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=0, size=4)


gtb3<-sb%>%ggplot(aes(x=t_skw_celcius,y=meanHWI)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="loess", se=T,color="green",fill="greenyellow") + annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5,size=7)+
  ylab("") +
  xlab("Temperature Skewness") +
  theme_bw()+ggtitle("Birds")
#print(gtb3)

###########################################
# Now for fish
###########################################

sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
taxa<-"fish"
sb<-sm_all%>%filter(TAXA==taxa)

# all cor with temperature
df<-read.csv(here("Results/fish_splist_with_temp_sensitivity.csv"))

# consistency table
dfc<-read.csv(here("Results/fish_splist_consistency_table.csv"))

# fishtraits
dft<-read.csv(here("DATA/traitsdata/fish_traits_from_FishBase.csv"))

# check if with increasing temp trend is there any change in fish length?
df_allsplen<-left_join(df,dft,by=c("spname"="name_in_mydb"))
df_allsplen<-rename(df_allsplen,name_in_mydb=spname)
df_allsplen_persite<-df_allsplen%>%group_by(STUDY_ID,newsite)%>%summarise(meanLen=mean(Length,na.rm=T))%>%ungroup()
sb<-left_join(sb,df_allsplen_persite,by=c("newsite","STUDY_ID"))

gtf1<-sb%>%ggplot(aes(x=t_med_celcius,y=meanLen)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T,color="dodgerblue",fill="steelblue1") + annotate("text",  x=Inf, y = Inf, label = "C", vjust=1.5, hjust=1.5,size=7)+
  ylab("Community-level mean body length (cm)") +
  xlab("Temperature, MedianT (\u00B0C)") +
  theme_bw()+ggtitle("Fish")
ms<-summary(lm(formula = meanLen ~ t_med_celcius, data = sb))
sl<-ms$coefficients[2,1] #slope
pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gtf1<-gtf1+
#  annotate("text",  x=5, y = 10, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=0, size=4)



gtf2<-sb%>%ggplot(aes(x=t.sens.slope.celcius,y=meanLen)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T,color="dodgerblue",fill="steelblue1") + annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5,size=7)+
  ylab("") +
  xlab("Temperature trend") +
  theme_bw()+ggtitle("Fish")
ms<-summary(lm(formula = meanLen ~ t.sens.slope.celcius, data = sb))
sl<-ms$coefficients[2,1] #slope
pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gtf2<-gtf2+
#  annotate("text",  x=0.05, y = 10, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=0, size=4)


pdf(here("Results/res_Prelim_Report/plot_direct_trais.pdf"), width = 6, height = 6)
grid.arrange(gtb1,gtb2,gtf1,gtf2,nrow=2)
dev.off()

gtf3<-sb%>%ggplot(aes(x=t_skw_celcius,y=meanLen)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T,color="dodgerblue",fill="steelblue1") + annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5,size=7)+
  ylab("") +
  xlab("Temperature Skewness") +
  theme_bw()+ggtitle("Fish")

#print(gtf3)


