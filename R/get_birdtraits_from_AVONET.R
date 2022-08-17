#rm(list=ls())
`%notin%` <- Negate(`%in%`)
library(tidyverse)
library(dplyr)
library(readxl)
# first for all fish species
df<-read.csv("../Results/res_Prelim_Report/birds_splist_with_temp_sensitivity.csv")
dff<-read.csv("../Results/res_Prelim_Report/birds_splist_consistency_table.csv")
avodat_meta<-read_excel("../DATA/traitsdata/AVONET/AVONET Supplementary dataset 1.xlsx",sheet=1)
avodat_raw<-read_excel("../DATA/traitsdata/AVONET/AVONET Supplementary dataset 1.xlsx",sheet=6)
# ok, see the problem
# species names are not same throughout the three sources of avodat_raw
# Species1_BirdLife, Species2_eBird, Species3_BirdTree
#
mydat<-data.frame(spname=dff$spname)
# first match with source2
get_dat_ebird<-left_join(mydat,avodat_raw,by=c("spname"="Species2_eBird"))
length(unique(get_dat_ebird$Avibase.ID))

id<-which(is.na(get_dat_ebird$Avibase.ID)==T)
avina<-get_dat_ebird[id,]# unknown 48 sp
avina<-data.frame(spname=avina$spname)

get_dat_ebird_good<-get_dat_ebird%>%filter(!is.na(get_dat_ebird$Avibase.ID))
length(unique(get_dat_ebird_good$spname)) #489 sp found

# now match with ebird,group (if any)
get_dat_ebird_gr<-left_join(avina,avodat_raw,by=c("spname"="eBird.species.group"))
get_dat_ebird_gr_good<-get_dat_ebird_gr%>%filter(!is.na(get_dat_ebird_gr$Avibase.ID))
length(unique(get_dat_ebird_gr_good$spname)) #1 sp found more

id<-which(is.na(get_dat_ebird_gr$Avibase.ID)==T)
avina<-get_dat_ebird_gr[id,]# unknown 47 sp
avina<-data.frame(spname=avina$spname)


# now match with another source
get_dat_BirdTree<-left_join(avina,avodat_raw,by=c("spname"="Species3_BirdTree"))

get_dat_BirdTree_good<-get_dat_BirdTree%>%filter(!is.na(get_dat_BirdTree$Avibase.ID))
length(unique(get_dat_BirdTree_good$spname)) #20 sp found more

id<-which(is.na(get_dat_BirdTree$Avibase.ID)==T)
avina<-get_dat_BirdTree[id,]# unknown 27 sp still
avina<-data.frame(spname=avina$spname)

# now match with another source again
get_dat_BirdLife<-left_join(avina,avodat_raw,by=c("spname"="Species1_BirdLife"))
# no match found!!!!

# so you have to deal with this manually
avina$possible_sp<-NA
avina$comments<-NA
avina2<-data.frame(Avibase.ID=NA)
avina<-cbind(avina2,avina)
write.csv(avina,"../DATA/traitsdata/bird_traits_tobe_filledin.csv",row.names=F)

# =============== how to get info for Empidonax sp.? =================
# This entry Empidonax sp. occurred 3 times in BBS data for newsite = 840_14_419, 840_17_211, 840_53_27
# one way to deal with this
# get name of particular species occurred in the routes containing those 3 sites and assign the majorly found species
# I can see from below tables Empidonax oberholseri species was found in all those routes, so I am choosing it.

library(stringr)
df1<-df%>%filter(str_detect(spname, 'Empidonax '))%>%filter(str_detect(newsite, '840_14_'))
tab1<-as.data.frame(table(df1$spname))
tab1<-tab1[order(tab1$Freq,decreasing = T),]
#Var1 Freq
#1  Empidonax difficilis   15
#3 Empidonax oberholseri   13
#2   Empidonax hammondii   10
#6    Empidonax wrightii    4
#5    Empidonax traillii    2
#4         Empidonax sp.    1

df1<-df%>%filter(str_detect(spname, 'Empidonax '))%>%filter(str_detect(newsite, '840_17_'))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
#Var1 Freq
#2  Empidonax oberholseri   14
#3 Empidonax occidentalis   13
#1    Empidonax hammondii    5
#5     Empidonax traillii    5
#6     Empidonax wrightii    2
#4          Empidonax sp.    1
df1<-df%>%filter(str_detect(spname, 'Empidonax '))%>%filter(str_detect(newsite, '840_53_'))
tab3<-as.data.frame(table(df1$spname))
tab3<-tab3[order(tab3$Freq,decreasing = T),]
#Var1 Freq
#5    Empidonax traillii   13
#1   Empidonax hammondii   12
#3 Empidonax oberholseri   11
#2     Empidonax minimus    2
#4         Empidonax sp.    1

# =============== how to get info for Gull sp.? =================
# In BBS these are the gull sp. recorded
#Xema sabini
#Chroicocephalus philadelphia
#Chroicocephalus ridibundus
#Hydrocoloeus minutus
#Rhodostethia rosea
#Leucophaeus atricilla
#Leucophaeus pipixcan
#Larus heermanni
#Larus canus
#Larus delawarensis
#Larus occidentalis
#Larus californicus
#Larus argentatus
#Larus glaucoides
#Larus fuscus
#Larus schistisagus
#Larus glaucescens
#Larus occidentalis x glaucescens
#Larus hyperboreus
#Larus marinus
#Gull sp.

df%>%filter(spname=="Gull sp.")
df1<-df%>%filter(str_detect(newsite, '124_4_'))%>%
  filter(str_detect(spname, "Larus")|str_detect(spname, "Leucophaeus")|str_detect(spname, "Chroicocephalus")|str_detect(spname, "Xema")|str_detect(spname, "Hydrocoloeus")|str_detect(spname, "Rhodostethia"))
tab1<-as.data.frame(table(df1$spname))
tab1<-tab1[order(tab1$Freq,decreasing = T),]
tab1
#Var1 Freq
#2   Larus delawarensis   10
#1   Larus californicus    2
#3 Leucophaeus pipixcan    2

df1<-df%>%filter(str_detect(newsite, '840_49_'))%>%
  filter(str_detect(spname, "Larus")|str_detect(spname, "Leucophaeus")|str_detect(spname, "Chroicocephalus")|str_detect(spname, "Xema")|str_detect(spname, "Hydrocoloeus")|str_detect(spname, "Rhodostethia"))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
tab2
#Var1 Freq
#2 Larus delawarensis    3
#1   Larus argentatus    1

# =============== how to get info for Phalacrocorax sp.? =================
# I can see from above tables which sp has higher chance
df1<-df%>%filter(str_detect(newsite, '840_14_'))%>%
  filter(str_detect(spname, "Phalacrocorax"))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
tab2
# based on above table choose Phalacrocorax auritus

# =============== how to get info for Poecile sp.? =================
# I can see from above tables which sp has higher chance
df1<-df%>%filter(str_detect(newsite, '840_33_'))%>%
  filter(str_detect(spname, "Poecile"))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
tab2
# based on above table choose Poecile atricapillus

# =============== how to get info for Sphyrapicus sp.? =================
# I can see from above tables which sp has higher chance
df1<-df%>%filter(str_detect(newsite, '124_11_'))%>%
  filter(str_detect(spname, "Sphyrapicus"))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
tab2
# based on above table choose Sphyrapicus nuchalis

# =============== how to get info for Trochilid sp.? =================
# I can see from above tables which sp has higher chance
df1<-df%>%filter(str_detect(newsite, '840_83_'))%>%
  filter(str_detect(spname, "Trochilid"))

# no info found

# =============== how to get info for Woodpecker sp.? =================
# I can see from above tables which sp has higher chance
df1<-df%>%filter(str_detect(newsite, '840_69_'))%>%
  filter(str_detect(spname, "Melanerpes"))
tab2<-as.data.frame(table(df1$spname))
tab2<-tab2[order(tab2$Freq,decreasing = T),]
tab2
# based on above table choose Melanerpes formicivorus

#====================================================================
# first read the manually filled in file
df_manual<-read.csv("../DATA/traitsdata/bird_traits_manually_filled.csv")
df_manual_2<-left_join(df_manual,avodat_raw,by=c("Avibase.ID"="Avibase.ID"))
write.csv(df_manual_2,"../DATA/traitsdata/AVONET_data_for_bird_traits_manually_filled.csv",row.names = F)


# ok, now combined all traits
get_dat_ebird_good<-get_dat_ebird_good%>%dplyr::select(-Species1_BirdLife,-eBird.species.group,-Species3_BirdTree)
get_dat_ebird_gr_good<-get_dat_ebird_gr_good%>%dplyr::select(-Species1_BirdLife,-Species2_eBird,-Species3_BirdTree)
get_dat_BirdTree_good<-get_dat_BirdTree_good%>%dplyr::select(-Species1_BirdLife,-Species2_eBird,-eBird.species.group)

birdstraits<-rbind(get_dat_ebird_good,get_dat_ebird_gr_good,get_dat_BirdTree_good)
birdstraits$possible_sp<-birdstraits$spname
birdstraits$comments<-"spname and possible sp are identical, matched sp from AVONET and BBS"
colnames(birdstraits)

df_manual_2<-df_manual_2%>%dplyr::select(colnames(birdstraits))
birdstraits<-rbind(birdstraits,df_manual_2)
birdstraits<-birdstraits[order(birdstraits$spname),]
write.csv(birdstraits,"../DATA/traitsdata/bird_traits_from_AVONET.csv",row.names=F)

