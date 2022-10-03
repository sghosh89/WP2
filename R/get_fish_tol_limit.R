# compute thermal preference niche: tolerance limit for fish species
library(here)
library(dplyr)
library(tidyverse)
library(raster)
library(sp)

#op<-par(mfrow=c(2,2))
bio5_path<- here("DATA/CHELSA_v2/climatologies/1981_2010/CHELSA_bio5_1981-2010_V.2.1.TIF")
rst_bio5 <- raster::raster(bio5_path)
#rst_bio5
#plot(rst_bio5)

bio6_path<- here("DATA/CHELSA_v2/climatologies/1981_2010/CHELSA_bio6_1981-2010_V.2.1.TIF")
rst_bio6 <- raster::raster(bio6_path)
#rst_bio6
#plot(rst_bio6)

bio7_path<- here("DATA/CHELSA_v2/climatologies/1981_2010/CHELSA_bio7_1981-2010_V.2.1.TIF")
rst_bio7 <- raster::raster(bio7_path)
#rst_bio7
#plot(rst_bio7)
#par(op)

# ok, that means data are in K/10 scale

fish_meta<-read.csv(here("DATA/STI_related/fish_gbif_data/cleaned/fish_occurrence_metadata.csv"))
# 2 out of 146 species has <10 cleaned records

fish_meta$maxTtol<-NA # max temp tolerance limit
fish_meta$minTtol<-NA # min temp tolerance limit

for(i in 1:nrow(fish_meta)){
  sp<-fish_meta$Species[i]
  occ_dat<-read.csv(here(paste("DATA/STI_related/fish_gbif_data/cleaned/",sp,".csv",sep="")))
  #range(occ_dat$year,na.rm=T)
  
  df_lonlat_table<-occ_dat%>%dplyr::select(decimalLongitude,decimalLatitude)
  
  #plot(df_lonlat_table$decimalLatitude~df_lonlat_table$decimalLongitude)
  
  # now make it as sp object
  coordinates(df_lonlat_table) <- ~decimalLongitude + decimalLatitude
  proj4string(df_lonlat_table) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  #class(df_lonlat_table)
  
  get_values_bio5<- raster::extract(rst_bio5, df_lonlat_table)
  get_values_bio6<- raster::extract(rst_bio6, df_lonlat_table)
  get_values_bio7<- raster::extract(rst_bio7, df_lonlat_table)
  
  occ_dat$bio5<-get_values_bio5
  occ_dat$bio6<-get_values_bio6
  occ_dat$bio7<-get_values_bio7
  
  #write.csv(occ_dat,here(paste("DATA/STI_related/fish_gbif_data/cleaned/",sp,"_with_bio567.csv",sep="")),row.names = F)
  #print(i)
  
  #--------- now get the max temp and minimum temp for each fish species ------------------
  # we want to take average of top 5 max temp records and bottom 5 min temp records
  # if there are less than 5 distinct records, then take avg of whatever records found
  
  xx<-occ_dat
  xxbio5<-xx%>%dplyr::select(species,year,bio5)%>%distinct(bio5,.keep_all = T)
  #xxbio5<-xxbio5%>%filter(year<1980)
  xxbio5<-xxbio5%>%arrange(desc(bio5))
  
  if(nrow(xxbio5)>=5){
    top5avg<-mean(xxbio5$bio5[1:5])
  }else{
    top5avg<-mean(xxbio5$bio5)
  }
  fish_meta$maxTtol[i]<-top5avg
  
  xxbio6<-xx%>%dplyr::select(species,year,bio6)%>%distinct(bio6,.keep_all = T)
  #xxbio6<-xxbio6%>%filter(year<1980)
  xxbio6<-xxbio6%>%arrange(bio6)
  if(nrow(xxbio6)>=5){
    bottom5avg<-mean(xxbio6$bio6[1:5])
  }else{
    bottom5avg<-mean(xxbio6$bio6)
  }
  fish_meta$minTtol[i]<-bottom5avg
}
write.csv(fish_meta,here("DATA/STI_related/fish_gbif_data/cleaned/fish_occurrence_metadata_with_tolerance.csv"),row.names = F)

