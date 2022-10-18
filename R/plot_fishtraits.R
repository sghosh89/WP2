# first for fish
library(here)
library(tidyverse)
library(dplyr)

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


# temperature: low vs high in community and how traits like dispersal ability vary?
q_T_taxa<-sb%>%summarise(q=quantile(t_med_celcius,c(0.25,0.75)))

lowT_taxa<-sb%>%filter(t_med_celcius<=unname(q_T_taxa[1,1]))
highT_taxa<-sb%>%filter(t_med_celcius>=unname(q_T_taxa[2,1]))
lowT_taxa$type<-"Low T,<50%CI"
highT_taxa$type<-"High T,>50%CI"

compare_data<-rbind(lowT_taxa,highT_taxa)
compare_data$type<-as.factor(compare_data$type)

g1<-ggplot(compare_data, aes(x = type, y = meanLen, color = type)) + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black")+
  geom_jitter(width = .02,alpha=0.3)+
  theme_bw()+theme(legend.position="none")+ 
  ylab("Fish mean traits, body length in cm")+xlab("Community in environment")
print(g1)

g2<-ggplot(compare_data, aes(x = type, y = nsp, color = type)) + 
  geom_boxplot(width = .1, outlier.shape = NA, color="black")+
  geom_jitter(width = .02,alpha=0.3)+
  theme_bw()+theme(legend.position="none")+ 
  ylab("Fish richness")+xlab("Community in environment")
print(g2)
