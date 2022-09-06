# we want to test the idea 
# does that fish trait has more variation sensitivity to temperature than birds
# so fish should show less similar response or synchrony-temperature effect than birds
# follow these steps:
# [1] choose a bird species, compute the correlation between sp. timeseries and temp. time series for a given site
# [2] repeat it for all sites where that species occur (maybe the study period could differ)
# [3] we want to know how consistent that species response to change in temp. across sites?
# [4] get the consistency ranking (somehow quantify it across all sites) - say 
#     atleast 70% of sites show consistent signal (positive or negative correlation)
# [5] repeat for all available bird species (species occurrence rate may not be same in all sites)
# [6] repeat the above steps for fish species
# [7] compare the consistency histogram between birds and fish
#rm(list=ls())
library(tidyverse)
library(dplyr)
sm_all<-read.csv("./../Results/stability_metric_and_env_all.csv")
taxa<-"birds"
sb<-sm_all%>%filter(TAXA==taxa)
table(sb$source) #1227 BBS data, 19 BioTIME data
#Make an all species list and the newsite name
sb<-sb%>%dplyr::select(source,STUDY_ID, newsite)
# for each new site get the splist

tab_spearcor_with_t<-c()
for(i in 1:nrow(sb)){
  if(sb$source[i]=="BioTIME"){
    if(sb$STUDY_ID[i]%in%c("67","215")){
      readpath<-paste("../Results/for_",sb$source[i],"/Terrestrial_plotlevel/",sb$STUDY_ID[i],"/",sb$newsite[i],"/",sep="")
    }else{
      readpath<-paste("../Results/for_",sb$source[i],"/Terrestrial_plotlevel/",sb$STUDY_ID[i],"/",sep="")
    }
  }else{
    readpath<-paste("../Results/for_",sb$source[i],"/",sb$newsite[i],"/",sep="")
  }
  #print(i)
  tempo<-readRDS(paste(readpath,"NonParamStat.RDS",sep=""))
  spearcor<-tempo$spear
  id<-which(rownames(spearcor)=="t")
  tempo2<-spearcor[id,]
  tempo2<-na.omit(tempo2)
  tempo2<-as.data.frame(tempo2)
  colnames(tempo2)<-"spearcor_with_t"
  
  # now get the significance of correlation
  tempo3<-tempo$posnI[id,]
  tempo3<-as.data.frame(tempo3)
  tempo3<-tempo3[1:nrow(tempo2),] # 1 means indep., non significant, change this code
  tempo3<-as.data.frame(tempo3)
  tempo3<-ifelse(tempo3$tempo3==1,0,1)
  tempo3[is.na(tempo3)]<-1
  tempo2$is_sig<-tempo3 # now 1 means sig cor, 0 means indep.
  
  tempo2$source<-sb$source[i]
  tempo2$STUDY_ID<-sb$STUDY_ID[i]
  tempo2$newsite<-sb$newsite[i]
  tempo2$spname<-as.character(rownames(tempo2))
  rownames(tempo2)<-NULL
  
  tab_spearcor_with_t<-rbind(tab_spearcor_with_t,tempo2)
}

#old=tab_spearcor_with_t
# below you can see a table of unique species occurence frequency 
# (i.e. number of communities they are found)
occur<-as.data.frame(table(tab_spearcor_with_t$spname))%>%arrange(desc(Freq))
#hist(occur$Freq,200)
sum(occur$Freq==1) #155 sp occur only in one community
df<-tab_spearcor_with_t %>% distinct(spname, newsite,.keep_all = TRUE)

splist<-read.csv("../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/SpeciesList_edited.csv")
splist<-splist%>%dplyr::select(AOU,ScientificName)
splist$AOU<-as.character(splist$AOU)
df<-left_join(df,splist,by=c("spname"="AOU"))
df$spname<-coalesce(df$ScientificName,df$spname)
df<-df%>%dplyr::select(-ScientificName)

write.csv(df,"../Results/birds_splist_with_temp_sensitivity.csv",row.names = F)

occur2<-as.data.frame(table(df$spname))%>%arrange(desc(Freq)) #537 unique sp across community
#hist(occur2$Freq,200)
sum(occur2$Freq==1) #101 sp occur only in one community

length(unique(df$spname)) # 537 unique sp in total

# now for sig correlation
df_sig<-df%>%filter(is_sig==1)
length(unique(df_sig$spname)) #357 unique sp with sig cor with temperature
# now get the percentage of how consistent a positive or negative correlation is
cons_tab_sig<-df_sig%>%group_by(spname)%>%
  summarise(poscor=sum(spearcor_with_t>0),
            negcor=sum(spearcor_with_t<0),
            zerocor=sum(spearcor_with_t==0))%>%ungroup()
# so all sig correlation between bird species and temp are positive: 
# with increasing temp, bird abundance increases

# but if you consider the species with non-sig correlation, then?
df_nonsig<-df%>%filter(is_sig==0)
length(unique(df_nonsig$spname)) #523 unique sp with non-sig correlation
# this means some common sp present which shows both sig and nonsig relationship with temp based on the sites
cons_tab_nonsig<-df_nonsig%>%group_by(spname)%>%
  summarise(poscor=sum(spearcor_with_t>0),
            negcor=sum(spearcor_with_t<0),
            zerocor=sum(spearcor_with_t==0))%>%ungroup()
which(df$spearcor_with_t==0)

# so, we will do for all data
cons_tab_all<-df%>%group_by(spname)%>%
  summarise(poscorsite=sum(spearcor_with_t>0),
            negcorsite=sum(spearcor_with_t<0),
            zerocorsite=sum(spearcor_with_t==0),
            poscorval=sum(spearcor_with_t[which(spearcor_with_t>0)]),
            negcorval=sum(spearcor_with_t[which(spearcor_with_t<0)]),
            zerocorval=sum(spearcor_with_t[which(spearcor_with_t==0)]))%>%ungroup()%>%
  group_by(spname)%>%mutate(occur_sites=sum(poscorsite,negcorsite,zerocorsite))%>%
  mutate(avgcorval=sum(poscorval,negcorval,zerocorval)/occur_sites)%>%ungroup()

write.csv(cons_tab_all,"../Results/birds_splist_consistency_table.csv",row.names = F)

