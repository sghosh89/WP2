#rm(list=ls())
library(tidyverse)
library(dplyr)
sm_all<-read.csv("./../Results/stability_metric_and_env_all.csv")
sb<-sm_all%>%filter(TAXA=="fish")
table(sb$source) #544 RivFishTIME data, 11 BioTIME data, 25 BioTIMEx data
#Make an all species list and the newsite name
sb<-sb%>%dplyr::select(source,STUDY_ID, newsite)
# for each new site get the splist
tab_spearcor_with_t<-c()
for(i in 1:nrow(sb)){
  if(sb$source[i]=="BioTIME"){
    if(sb$STUDY_ID[i]%in%c("57")){
      readpath<-paste("../Results/for_",sb$source[i],"/Freshwater_plotlevel/",sb$STUDY_ID[i],"/",sep="")
    }else{
      readpath<-paste("../Results/for_",sb$source[i],"/Freshwater_plotlevel/",sb$STUDY_ID[i],"/",sb$newsite[i],"/",sep="")
    }
  }else if(sb$source[i]=="BioTIMEx"){
    readpath<-paste("../Results/for_",sb$source[i],"/",sb$STUDY_ID[i],"/",sb$newsite[i],"/",sep="")
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
#---------------------------

occur<-as.data.frame(table(tab_spearcor_with_t$spname))%>%arrange(desc(Freq))
#hist(occur$Freq,200)
sum(occur$Freq==1) #23 sp occur only in one community
df<-tab_spearcor_with_t %>% distinct(spname, newsite,.keep_all = TRUE)
write.csv(df,"../Results/res_Prelim_Report/fish_splist_with_temp_sensitivity.csv",row.names = F)

length(unique(df$spname)) # 157 unique fish sp in total

# now for sig correlation
df_sig<-df%>%filter(is_sig==1)
length(unique(df_sig$spname)) #75 unique sp with sig cor with temperature
# now get the percentage of how consistent a positive or negative correlation is
cons_tab_sig<-df_sig%>%group_by(spname)%>%
  summarise(poscor=sum(spearcor_with_t>0),
            negcor=sum(spearcor_with_t<0),
            zerocor=sum(spearcor_with_t==0))%>%ungroup()
# so all sig correlation between fish species and temp are positive: 
# with increasing temp, fish abundance increases

# but if you consider the species with non-sig correlation, then?
df_nonsig<-df%>%filter(is_sig==0)
length(unique(df_nonsig$spname)) #154 unique sp with non-sig correlation
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

hist(cons_tab_all$diffcor,100,col="skyblue",border=F,
     xlab="sites(pos.cor - neg.cor)",
     main=paste("fish, n =",nrow(cons_tab_all)," unique species", sep=""))
abline(v=0,col="black",lty=2)
sum(cons_tab_all$diffcor>0)/nrow(cons_tab_all)# 49.7% with positive correlation
sum(cons_tab_all$diffcor<0)/nrow(cons_tab_all)# 42% sites with negative correlation
sum(cons_tab_all$diffcor==0)/nrow(cons_tab_all)# 8.3% sites with no correlation
text(x=20, y=15, round(100*sum(cons_tab_all$diffcor>0)/nrow(cons_tab_all),2),
     col = "red", srt = 0)
text(x=-20, y=15, round(100*sum(cons_tab_all$diffcor<0)/nrow(cons_tab_all),2),
     col = "blue", srt = 0)

# which(cons_tab_all$diffcor==0) # 13 fish sp shows equal sites of pos and neg correlation
#hist(cons_tab_all$percent_poscor,100,border=F,col=rgb(1,0,0,0.25),
#     xlab="consistent % of positive and negative cor",
#     main=paste("fish, n =",nrow(cons_tab_all)))
#hist(cons_tab_all$percent_negcor,100,border=F,col=rgb(0,0,1,0.25),add=T)








