# get traits data for birds and fish used for WP2 
#rm(list=ls())
`%notin%` <- Negate(`%in%`)
library(tidyverse)
library(dplyr)
# first for all fish species
df<-read.csv("../Results/res_Prelim_Report/fish_splist_with_temp_sensitivity.csv")
dff<-read.csv("../Results/res_Prelim_Report/fish_splist_consistency_table.csv")


# so for 157 unique fish species used in WP2, we want to find the bodysize/bodymass

library(rfishbase)

########################################################
# get the common length mainly, but keep all
z2<-rfishbase::species(dff$spname)
z2_simple<-z2%>%dplyr::select(SpecCode,Species,Author,Length,LTypeMaxM,LengthFemale,LTypeMaxF,MaxLengthRef,
                              CommonLength,LTypeComM,CommonLengthF,LTypeComF,CommonLengthRef,Weight,
                              WeightFemale,MaxWeightRef,Comments)
# ok, note by searching any species name in the online database you can get more info,
# for example, search any species name here: https://www.fishbase.se/search.php
# then you can see, the Length column means max length (male/unsexed)

z2_known<-z2_simple%>%filter(!is.na(SpecCode))
# ok, now get the speccode = NA
z2_unknown<-z2_simple%>%filter(is.na(SpecCode))
z2_unknown$possible_sp<-NA
write.csv(z2_unknown,"../DATA/traitsdata/fish_traits_tobe_filledin.csv",row.names=F)

# now I manually filled in some info, read this
z2_unknown<-read.csv("../DATA/traitsdata/fish_traits_manually_filled.csv")
z2_unknown<-z2_unknown%>%dplyr::select(Species,possible_sp)
z2_get_unknown<-rfishbase::species(z2_unknown$possible_sp)
z2_get_unknown<-z2_get_unknown%>%dplyr::select(SpecCode,Species,Author,Length,LTypeMaxM,LengthFemale,LTypeMaxF,MaxLengthRef,
                                               CommonLength,LTypeComM,CommonLengthF,LTypeComF,CommonLengthRef,Weight,
                                               WeightFemale,MaxWeightRef,Comments)
#NOTE: 	
# walleye and Stizostedion vitreum both are same species called: Sander vitreus in FishBase
# Now join the whole data
z2_known$name_in_mydb<-z2_known$Species
z2_get_unknown$name_in_mydb<-z2_unknown$Species

fish_traits<-rbind(z2_known,z2_get_unknown)
table(fish_traits$LTypeMaxM)
#FL  SL  TL 
#6  15 135 

fish_traits_TL<-fish_traits%>%filter(LTypeMaxM=="TL" | is.na(LTypeMaxM)) #136 obs
fish_traits_others<-fish_traits%>%filter(LTypeMaxM!="TL") #21 obs

# Now we want to convert SL/FL to TL for given Length column
# use Length_Length function
get_TL<-length_length(fish_traits_others$Species)
get_TL<-get_TL%>%filter(Length1=="TL" | Length2=="TL") # atleast 64 obs showing any one length is TL
get_TL<-get_TL%>%dplyr::select(SpecCode,Species,autoctr,StockCode,Length1,Length2,a,b)

keep_sp<-c()
for(i in 1:nrow(fish_traits_others)){
  tempo<-get_TL%>%filter(Species%in%fish_traits_others$Species[i])
  id<-tempo$Length1%in%fish_traits_others$LTypeMaxM[i] | tempo$Length2%in%fish_traits_others$LTypeMaxM[i]
  keep_sp<-rbind(keep_sp,tempo[id,])
}
#fish_traits_others1<-fish_traits_others%>%dplyr::select(Species,Length,LTypeMaxM)
keep_sp<-left_join(keep_sp,fish_traits_others,by=c("Species"="Species","SpecCode"="SpecCode"))
keep_sp$estimated_TL<-NA
for(i in 1:nrow(keep_sp)){
  if(keep_sp$LTypeMaxM[i]==keep_sp$Length1[i]){ # then use TL as L2 formula
    keep_sp$estimated_TL[i]<-keep_sp$a[i]+(keep_sp$b[i]*keep_sp$Length[i])
  }
  if(keep_sp$LTypeMaxM[i]==keep_sp$Length2[i]){ # then use TL as L1 formula
    keep_sp$estimated_TL[i]<-(keep_sp$Length[i] - keep_sp$a[i])/keep_sp$b[i]
  }
}
write.csv(keep_sp,"../DATA/traitsdata/fish_traits_records_estimatedTL.csv",row.names=F)

tempo<-keep_sp%>%group_by(Species)%>%summarise(LengthTL=mean(estimated_TL))%>%ungroup()
tempo$Type<-"TL"
# ok, now overwrite
res <- fish_traits_others %>% left_join(tempo, by = c("Species")) %>%
  mutate(Length=coalesce(LengthTL,Length),LTypeMaxM=coalesce(Type,LTypeMaxM)) %>%
  dplyr::select(-Type,-LengthTL)
# now combine
fish_traits<-rbind(fish_traits_TL,res)
fish_traits<-fish_traits[order(fish_traits$Species),]
table(fish_traits$LTypeMaxM) # 153 TL male, 3 SL, 1 Female TL (Scardinius erythrophthalmus)
write.csv(fish_traits,"../DATA/traitsdata/fish_traits_from_FishBase.csv",row.names=F)

