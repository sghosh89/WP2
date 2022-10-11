# I want to get an idea of thermal preference niche and tolerance limit of a given species: STI

# WON'T RUN ON WINDOWS

#
#rm(list=ls())
library(here)
library(rgbif)
library(tidyverse)
library(dplyr)
library(parallel)
`%notin%` <- Negate(`%in%`)

#freecores<-1
busycores<-20 # for parallell computing with mclapply your computer will use these cores
`%notin%` <- Negate(`%in%`)


# birdtraits
dft<-read.csv(here("DATA/traitsdata/bird_traits_from_AVONET.csv"))
dft<-dft[(!is.na(dft$Avibase.ID)),]#exclude Trochilid sp.

ddft0<-dft%>%distinct(Avibase.ID,.keep_all = T)# unique species
ddft<-ddft0%>%dplyr::select(Avibase.ID,
                           spname, # occur in community data
                           Species=possible_sp # then we identified, see comments in ddft0
                           )

splist<-ddft$Species

ddft$gbif_dwnld_date<-"2022-09-24"
ddft$gbif_TaxonKey<-NA # this is the usage key or taxon key or species key
ddft$maxrecords_in_gbif<-NA
ddft$maxrecords_PRESENT_only<-NA
name_backbone_list<-c()

for(i in c(1:length(splist))){
  sp<-splist[i]
  x<-name_backbone(sp)
  ddft$gbif_TaxonKey[i]<-x$usageKey
  y<-occ_count(taxonKey=x$usageKey, georeferenced=TRUE)
  ddft$maxrecords_in_gbif[i]<-y
  
  z<-occ_search(taxonKey = x$usageKey,
                hasGeospatialIssue =F,
                hasCoordinate = T,
                occurrenceStatus = "PRESENT",
                limit=0)$meta$count
  ddft$maxrecords_PRESENT_only[i]<-z
  
  if("acceptedUsageKey" %in% colnames(x)){
    x1<-x$acceptedUsageKey
    x<-x%>%dplyr::select(-acceptedUsageKey)
    x$acceptedUsageKey<-x1
  }else{
    x$acceptedUsageKey<-x$usageKey
  }
  name_backbone_list<-rbind(name_backbone_list,x)
  print(i)
}
write.csv(name_backbone_list, here("DATA/STI_related/birds_gbif_data/birds_data_name_backbone_table.csv"),row.names = F)

ddft$comments<-NA # fill this column later manually
ddft$gbif_dwnld_type<-ifelse(ddft$maxrecords_PRESENT_only>99999,"manual","code") # manual or coded
write.csv(ddft, here("DATA/STI_related/birds_gbif_data/birds_occurrence_data_needed.csv"),row.names = F)

# read the manually filled in table
ddft<-read.csv(here("DATA/STI_related/birds_gbif_data/birds_occurrence_data_needed_edited.csv"))
tab_manual<-ddft%>%filter(gbif_dwnld_type=="manual") # 31 species data 
# are downloaded from GBIF website

tab_code<-ddft%>%filter(gbif_dwnld_type=="code")
# for the rest 115 species we will get data by running below code

# Now we need to get the occurrence data through code 
# for the 115 species appeared in the above table
splist <- tab_code$Species

totsp<-length(splist)
limit<-99999
# running a for loop may take longer time, so you can run the code with 
# mclapply using multiple cores, but mclapply does not work from windows
res_single_sp<-function(isp,splist,limit=99999){
  s<-splist[isp]
  sp_data <- rgbif::occ_data(scientificName=s,
                             limit=limit,# change this limit later, default=500, max=100000
                             hasCoordinate=TRUE, 
                             hasGeospatialIssue = FALSE)
  
  #coords <- sp_data$data[ , c("name",
  #                            "decimalLongitude", "decimalLatitude", 
  #                            "occurrenceStatus", 
  #                            "basisOfRecord",
  #                            "issues",
  #                            "year","month","day","country")]
  coords<-sp_data$data
  coords<-coords%>%dplyr::select(c(species,scientificName,decimalLongitude, decimalLatitude, 
                                   occurrenceStatus, 
                                   basisOfRecord,
                                   issues,
                                   year,month,day,countryCode))
  
  coords<-coords%>%filter(occurrenceStatus=="PRESENT")# just to ensure
  
  write.csv(coords, paste(here("DATA/STI_related/birds_gbif_data/birdrecords_from_GBIF_for_"),s,".csv",sep=""),row.names = F)
  cat(paste("----------- isp = ",isp," , species = ",s," -----    \n"))
}
totsp<-length(splist)
res_all_sp<-mclapply(X=error_sp,FUN=res_single_sp,
                     splist=splist,
                     limit=limit,mc.cores=busycores)




