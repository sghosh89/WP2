library(here)
library(rgbif)
library(tidyverse)
library(dplyr)
library(countrycode)
library(CoordinateCleaner)
`%notin%` <- Negate(`%in%`)

ddft<-read.csv(here("DATA/STI_related/birds_gbif_data/birds_occurrence_data_needed_edited.csv"))
length(unique(ddft$Species))
ddft<-ddft%>%distinct(Species,.keep_all = T)
ddft$cleaned_records<-NA

tab_manual<-ddft%>%filter(gbif_dwnld_type=="manual")
length(unique(tab_manual$Species))
tab_coded<-ddft%>%filter(gbif_dwnld_type=="code")
#------------------------------------------------------
sink(here("DATA/STI_related/birds_gbif_data/cleaned/issues_found_during_cleaning_codedsp.txt"),append=TRUE, split=TRUE)
# now do the same for coded species
sp_coded<-tab_coded$Species # 64 species
tot_sp_coded<-length(sp_coded)
for(i in 1:tot_sp_coded){
  s<-sp_coded[i]
  
  filename<-here(paste("DATA/STI_related/birds_gbif_data/birdrecords_from_GBIF_for_",s,".csv",sep=""))
  x<-read.csv(filename,row.names = NULL)
  y <- x%>%dplyr::select( c(species,scientificName,decimalLongitude, decimalLatitude,
                            basisOfRecord,issues,year,month,day,countryCode))
  
  y<-y%>%filter(basisOfRecord%notin%c("FOSSIL_SPECIMEN","PRESERVED_SPECIMEN"))%>%rename(countryCode2=countryCode)
  # get 3 letter countrycode
  y<-y%>%mutate(countryCode = countrycode(y$countryCode2, origin =  'iso2c', destination = 'iso3c'))
  
  #y1<-y%>%distinct(countryCode,.keep_all = T)# ok, so for some 2 letter country code no 3 letter code
  # is known, for example: Kosovo country (XK) has no 3 letter code
  # filter out also those ambiguous country records
  y<-y%>%filter(!is.na(countryCode))
  y<-y%>%filter(!is.na(decimalLongitude) & !is.na(decimalLongitude))
  
  # automated fixing the issues
  flags <- clean_coordinates(x = y, lon = "decimalLongitude", lat = "decimalLatitude", 
                             countries = "countryCode",  species = NULL,
                             tests = c("capitals", "centroids", 
                                       "equal", "gbif", "zeros", 
                                       "countries", "seas"), seas_ref = buffland)
  cat(paste("i = ",i,", s = ",s,"\n"))
  print(summary(flags))
  y<-y[flags$.summary,]# cleaned data
  write.csv(y,here(paste("DATA/STI_related/birds_gbif_data/cleaned/",s,".csv",sep="")))
}
sink()

tab_manual<-ddft%>%filter(gbif_dwnld_type=="manual")
#----- Now clean and save the manually downloaded species in the same format as of automatically downloaded data -----
sp_manual<-tab_manual$Species # 438 species
tot_sp_manual<-length(sp_manual)
sink(here("DATA/STI_related/birds_gbif_data/cleaned/issues_found_during_cleaning.txt"),append=TRUE, split=TRUE)
for(i in 1:tot_sp_manual){
  s<-sp_manual[i]
  filename<-here(paste("DATA/STI_related/birds_gbif_data/",s,".csv",sep=""))
  x<-read.csv(filename,sep="\t",row.names = NULL)
  y <- x%>%mutate(species=s)%>%dplyr::select( c(species,
                                                scientificName,decimalLongitude, decimalLatitude,
                                                basisOfRecord,
                                                issue,
                                                year,month,day,countryCode))%>%
    mutate(occurrenceStatus="PRESENT")%>%rename(issues=issue)
  
  y<-y%>%filter(basisOfRecord%notin%c("FOSSIL_SPECIMEN","PRESERVED_SPECIMEN"))%>%rename(countryCode2=countryCode)
  # get 3 letter countrycode
  y<-y%>%mutate(countryCode = countrycode(y$countryCode2, origin =  'iso2c', destination = 'iso3c'))
  
  #y1<-y%>%distinct(countryCode,.keep_all = T)# ok, so for some 2 letter country code no 3 letter code
  # is known, for example: Kosovo country (XK) has no 3 letter code
  # filter out also those ambiguous country records
  y<-y%>%filter(!is.na(countryCode))
  # just to ensure
  y$decimalLatitude<-as.numeric(y$decimalLatitude)
  y$decimalLongitude<-as.numeric(y$decimalLongitude)
  y<-y%>%filter(!is.na(decimalLongitude))%>%filter(!is.na(decimalLongitude))
  
  # automated fixing the issues
  flags <- clean_coordinates(x = y, lon = "decimalLongitude", lat = "decimalLatitude", 
                             countries = "countryCode",  species = NULL,
                             tests = c("capitals", "centroids", 
                                       "equal", "gbif", "zeros", 
                                       "countries", "seas"), seas_ref = buffland)
  cat(paste("i = ",i,", s = ",s,"\n"))
  print(summary(flags))
  y<-y[flags$.summary,]# cleaned data
  write.csv(y,here(paste("DATA/STI_related/birds_gbif_data/cleaned/",s,".csv",sep="")),row.names = F)
}
sink()

for (i in 1:nrow(ddft)){
  s<-ddft$Species[i]
  filename<-here(paste("DATA/STI_related/birds_gbif_data/cleaned/",s,".csv",sep=""))
  x<-read.csv(filename,row.names = NULL)
  ddft$cleaned_records[i]<-nrow(x)
  print(i)
}
write.csv(ddft,here("DATA/STI_related/birds_gbif_data/cleaned/birds_occurrence_metadata_filledin.csv"),row.names = F)



