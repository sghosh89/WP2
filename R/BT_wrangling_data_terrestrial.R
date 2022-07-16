library(dplyr)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
#grid_BT<-readRDS("../DATA/for_BioTIME/BioTIME_public_private_data_metadata_minyr_20.RDS")

grid_terres<-grid_BT%>%filter(REALM=="Terrestrial")
#===================== generate results folder for terrestrial ===============
resloc<-"../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
saveRDS(grid_terres,paste(resloc,"bt_terres_min20yr_rawdata.RDS",sep=""))
#============================================================================
unique(grid_terres$STUDY_ID) #10 study_ID
df<-grid_terres%>%group_by(TAXA)%>%distinct(STUDY_ID)%>%ungroup()

#  STUDY_ID = 195 # BBS data, 300 # Landis from BioTIMEx
df<-df%>%filter(STUDY_ID%notin%c(195,300))

write.csv(df,paste(resloc,"tab_terres_BT.csv",sep=""),row.names = F)
#============================================================================
# now watch each STUDY_ID
df$nyr<-NA
df$nPLOT<-NA
df$NUMBER_LAT_LONG<-NA
df$LATmin<-NA
df$LATmax<-NA
df$LONmin<-NA
df$LONmax<-NA
df$monthlyfreqsamp<-NA
df$n_methods<-NA

library(htmltools) 
library(htmlwidgets)
library(leaflet) 

for(i in 1:nrow(df)){
  dat<-grid_terres%>%filter(STUDY_ID==df$STUDY_ID[i])
  df$nyr[i]<-length(unique(dat$YEAR))
  df$nPLOT[i]<-list(unique(dat$PLOT))
  df$NUMBER_LAT_LONG[i]<-unique(dat$NUMBER_LAT_LONG)
  df$LATmin[i]<-min(unique(dat$LATITUDE))
  df$LATmax[i]<-max(unique(dat$LATITUDE))
  df$LONmin[i]<-min(unique(dat$LONGITUDE))
  df$LONmax[i]<-max(unique(dat$LONGITUDE))
  t1<-dat%>%group_by(YEAR)%>%summarise(n_distinct(MONTH))%>%ungroup()
  df$monthlyfreqsamp[i]<-list(range(t1$`n_distinct(MONTH)`))
  df$n_methods[i]<-list(unique(dat$SUMMARY_METHODS))
  #---------- save sampling sites on map ----------
  dat<-dat%>%dplyr::select(STUDY_ID,LATITUDE,LONGITUDE)%>%distinct()
  
  sitemap<-leaflet(dat) %>% addTiles() %>%
    addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(STUDY_ID))
  f<-paste("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/samplingsite_",
           dat$STUDY_ID[1],".html",sep="")
  htmlwidgets::saveWidget(sitemap, 
                          file.path(normalizePath(dirname(f)),basename(f)))
  
}
saveRDS(df,"../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")










