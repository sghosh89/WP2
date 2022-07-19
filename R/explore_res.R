#############################################################
# I am showing the result without BioTIME and BioTIMEx data
#############################################################

library(dplyr)
library(tidyverse)

# starting metadata summary
md_BioDyn<-read.csv("../DATA/metadata_summary.csv") #2759 communities with min 20 years data, 7 taxa
md<-md_BioDyn%>%filter(TAXA%in%c("birds","fish","terrestrial invertebrates","freshwater invertebrates")) #1985 obs
md<-md%>%filter(startyr>=1979 & endyr<=2019) # 1985 communities
## But this is not a good idea to filter data from metadata of BioDyn based on startyr as sometimes 
# the timeseries starts e.g. on 1966, then after years gap it again have data from 1980 onwards,
# so sample each raw data from the databases and filter community if it has data for the 
# range [1979 to 2019].
md%>%group_by(TAXA,source)%>%summarise(n=n())%>%ungroup()

# so, we examined each community and data after filtering we got
sm_all<-read.csv("../Results/stability_metric_and_env_all.csv")
str(sm_all) # total communities within 1979-2019 timespan, we selected only 4 taxa
sm_all%>%group_by(TAXA,source)%>%summarise(n=n())%>%ungroup()

# explore some plots 
sum(sm_all$t_skw<0)
#ggplot(sm_all,aes(x=t_skw))+geom_histogram(binwidth=0.2)+ggtitle("all taxa")+theme_classic()

br <- c(min(sm_all$t_skw),0,max(sm_all$t_skw))

p1<-sm_all%>%
  ggplot( aes(x=t_skw, stat(density))) +
  ggtitle("all taxa")+
  geom_text(
    aes(label = round(stat(count) / sum((count)), 3)), 
    stat = 'bin', vjust = -0.5, breaks = br
  )+ ylim(c(0, 1))+geom_density()+
  annotate("rect", xmin=min(sm_all$t_skw), xmax=0, ymin=0, ymax=1, alpha=0.15, fill="red")+
  annotate("rect", xmin=0, xmax=max(sm_all$t_skw), ymin=0, ymax=1, alpha=0.15, fill="blue")+
  theme_classic()
p1 # this plot shows overall what percent of communities facing 
    # increasing (red) or decreasing (blue) annual temperature during the study period

# Now repeat the above plot by taxa
p2<-sm_all%>%
  ggplot( aes(x=t_skw,y=..scaled..))+geom_density()+ylim(c(0, 1))+
  annotate("rect", xmin=min(sm_all$t_skw), xmax=0, ymin=0, ymax=1, alpha=0.15, fill="red")+
  annotate("rect", xmin=0, xmax=max(sm_all$t_skw), ymin=0, ymax=1, alpha=0.15, fill="blue")+
  theme_classic()+facet_wrap(~TAXA)
p2 

p3<-sm_all%>%
  ggplot( aes(x=t_skw, stat(density))) +
  ggtitle("all taxa")+
  geom_text(
    aes(label = round(stat(count) / sum((count)), 3)), 
    stat = 'bin', vjust = -0.5, breaks = br
  )+ ylim(c(0, 1))+geom_density()+
  annotate("rect", xmin=min(sm_all$t_skw), xmax=0, ymin=0, ymax=1, alpha=0.15, fill="red")+
  annotate("rect", xmin=0, xmax=max(sm_all$t_skw), ymin=0, ymax=1, alpha=0.15, fill="blue")+
  theme_classic()+facet_wrap(~TAXA)
#p3 # density is not scaled, that's why >1?

# In any way, I made the above plot to see the temperatute trend is increasing or decreasing at 
# those communities for the study period






