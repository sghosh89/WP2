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
sb<-sm_all%>%filter(TAXA=="birds")
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
sum(occur$Freq==1) #139 sp occur only in one community
df<-tab_spearcor_with_t %>% distinct(spname, newsite,.keep_all = TRUE)

splist<-read.csv("../DATA/for_BBS/raw_data/BBSdata_accessed_03dec2020/SpeciesList_edited.csv")
splist<-splist%>%dplyr::select(AOU,ScientificName)
splist$AOU<-as.character(splist$AOU)
df<-left_join(df,splist,by=c("spname"="AOU"))
df$spname<-coalesce(df$ScientificName,df$spname)
df<-df%>%dplyr::select(-ScientificName)

write.csv(df,"../Results/res_Prelim_Report/birds_splist_with_temp_sensitivity.csv",row.names = F)

occur2<-as.data.frame(table(df$spname))%>%arrange(desc(Freq)) #537 unique sp across community
#hist(occur2$Freq,200)
sum(occur2$Freq==1) #95 sp occur only in one community

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
  summarise(poscor=sum(spearcor_with_t>0),
            negcor=sum(spearcor_with_t<0),
            zerocor=sum(spearcor_with_t==0))%>%ungroup()%>%
group_by(spname)%>%mutate(occur_sites=sum(poscor,negcor,zerocor))%>%
  mutate(percent_poscor=(poscor/occur_sites)*100,
         percent_negcor=(negcor/occur_sites)*100,
         percent_zerocor=(zerocor/occur_sites)*100)%>%ungroup()
cons_tab_all$diffcor<-cons_tab_all$percent_poscor - cons_tab_all$percent_negcor

hist(cons_tab_all$diffcor,100,col="green",border=F,
     xlab="sites(pos.cor - neg.cor)",
     main=paste("birds, n =",nrow(cons_tab_all)," unique species", sep=""))
abline(v=0,col="black",lty=2)
sum(cons_tab_all$diffcor>0)/nrow(cons_tab_all)# 64% with positive correlation
sum(cons_tab_all$diffcor<0)/nrow(cons_tab_all)# 30% sites with negative correlation
sum(cons_tab_all$diffcor==0)/nrow(cons_tab_all)# 6% sites with no correlation
text(x=20, y=40, round(100*sum(cons_tab_all$diffcor>0)/nrow(cons_tab_all),2),
     col = "red", srt = 0)
text(x=-20, y=40, round(100*sum(cons_tab_all$diffcor<0)/nrow(cons_tab_all),2),
     col = "blue", srt = 0)

#hist(cons_tab_all$percent_poscor,100,border=F,col=rgb(1,0,0,0.25),
#     xlab="consistent % of positive and negative cor",
#     main=paste("birds, n =",nrow(cons_tab_all)))
#hist(cons_tab_all$percent_negcor,100,border=F,col=rgb(0,0,1,0.25),add=T)



# which(cons_tab_all$diffcor==0) # 31 bird sp shows equal sites of pos and neg correlation
# one sec! this histogram says overall response
# but I want to see how consistent response each bird species shows to temperature
dfb<-read.csv("../Results/res_Prelim_Report/birds_splist_with_temp_sensitivity.csv")
#dfb<-dfb%>%filter(is_sig==1)
tab_cib<-dfb%>%group_by(spname)%>%summarise(qlow95=quantile(spearcor_with_t,c(0.025)),
                                          qhi95=quantile(spearcor_with_t,c(0.975)),
                                          qlow75=quantile(spearcor_with_t,c(0.125)),
                                          qhi75=quantile(spearcor_with_t,c(0.875)),
                                          qlow50=quantile(spearcor_with_t,c(0.25)),
                                          qhi50=quantile(spearcor_with_t,c(0.75)))%>%
  mutate(sig95=ifelse(qlow95/qhi95>0,1,0),
         sig75=ifelse(qlow75/qhi75>0,1,0),
         sig50=ifelse(qlow50/qhi50>0,1,0))%>%ungroup()

dff<-read.csv("../Results/res_Prelim_Report/fish_splist_with_temp_sensitivity.csv")
#dff<-dff%>%filter(is_sig==1)
tab_cif<-dff%>%group_by(spname)%>%summarise(qlow95=quantile(spearcor_with_t,c(0.025)),
                                            qhi95=quantile(spearcor_with_t,c(0.975)),
                                            qlow75=quantile(spearcor_with_t,c(0.125)),
                                            qhi75=quantile(spearcor_with_t,c(0.875)),
                                            qlow50=quantile(spearcor_with_t,c(0.25)),
                                            qhi50=quantile(spearcor_with_t,c(0.75)))%>%
  mutate(sig95=ifelse(qlow95/qhi95>0,1,0),
         sig75=ifelse(qlow75/qhi75>0,1,0),
         sig50=ifelse(qlow50/qhi50>0,1,0))%>%ungroup()

#based on 95% CI how much % of species per taxa show consistent response to temp change
tb<-as.data.frame(table(tab_cib$sig95)/nrow(tab_cib))
tf<-as.data.frame(table(tab_cif$sig95)/nrow(tab_cif))
consistency_tab<-data.frame(is_consistent=tb$Var1,
                            birds=tb$Freq,
                            fish=tf$Freq)
consistency_tab

#based on 75% CI how much % of species per taxa show consistent response to temp change
tb<-as.data.frame(table(tab_cib$sig50)/nrow(tab_cib))
tf<-as.data.frame(table(tab_cif$sig50)/nrow(tab_cif))
consistency_tab<-data.frame(is_consistent=tb$Var1,
                            birds=tb$Freq,
                            fish=tf$Freq)
consistency_tab
