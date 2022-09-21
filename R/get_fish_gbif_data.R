# I want to get an idea of thermal preference niche and tolerance limit of a given species: STI

#rm(list=ls())
library(here)
library(rgbif)
library(tidyverse)
library(dplyr)
library(parallel)
library(countrycode)
library(CoordinateCleaner)
`%notin%` <- Negate(`%in%`)

# fishtraits
dft<-read.csv(here("DATA/traitsdata/fish_traits_from_FishBase.csv"))
any(is.na(dft$SpecCode))#FALSE

ddft<-dft%>%distinct(SpecCode,.keep_all = T)# unique species
ddft<-ddft%>%dplyr::select(SpecCode,Species,Author,name_in_mydb)

splist<-ddft$Species

ddft$gbif_dwnld_date<-"2022-09-16"
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
  name_backbone_list<-rbind(name_backbone_list,x)
  
}
write.csv(name_backbone_list, here("DATA/STI_related/fish_gbif_data/fish_data_name_backbone_table.csv"),row.names = F)

ddft$comments<-NA # fill this column later manually
ddft$gbif_dwnld_type<-ifelse(ddft$maxrecords_PRESENT_only>99999,"manual","code") # manual or coded
write.csv(ddft, here("DATA/STI_related/fish_gbif_data/fish_occurrence_data_needed.csv"),row.names = F)

#x<-ddft%>%filter(maxrecords_in_gbif>99999)

# now how to search?
# example: https://www.gbif.org/occurrence/charts?has_coordinate=true&has_geospatial_issue=false&taxon_key=2394610&occurrence_status=present

# read the manually filled in table
ddft<-read.csv(here("DATA/STI_related/fish_gbif_data/fish_occurrence_data_needed_edited.csv"))
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
for(isp in 1:totsp){
  s<-splist[isp]
  print(s)
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
  
  write.csv(coords, here(paste("DATA/STI_related/fish_gbif_data/fishrecords_from_GBIF_for_",s,".csv",sep="")),row.names = F)
  cat(paste("----------- isp = ",isp," , species = ",s," -----    \n"))
}












