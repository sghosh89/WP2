`%notin%` <- Negate(`%in%`)
library(dplyr)
data_pt_thrs<-20

# read the meta data
xxm<-read.csv("../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEMetadata_24_06_2021.csv") # a dataframe
#xxm<-xxm%>%filter(DATA_POINTS>=data_pt_thrs)

# NOTES: BioTIME database is not at all perfect, Study_ID=249 has 1992-2009 = 18 years of data, 
# and we need annual data, but in DATA_POINTS they put the entry 24. So, it is better to check 
# the data first, not always going by the metadata

# read the meta data from private version
xxm_private<-read.csv("../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/bioTIMEmetadataScienceStudies.csv") # a dataframe
xxm_extra<-anti_join(x=xxm_private,y=xxm,by="STUDY_ID") # 303 extra data
#xxm_extra<-xxm_extra%>%filter(DATA_POINTS>=data_pt_thrs) # 10 extra data with >= 20 years

# but we will exclude relative biomass type: park grass exp. as it is not frequently sampled
xxm_extra<-xxm_extra%>%filter(BIOMASS_TYPE%notin%"Relative biomass") # 9 extra data

# now combine the metadata
xxm<-xxm%>%dplyr::select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
                         AB_BIO,ABUNDANCE_TYPE, BIOMASS_TYPE,
                         TITLE, AREA_SQ_KM, GRAIN_SQ_KM, PROTECTED_AREA, 
                         DATA_POINTS,START_YEAR,END_YEAR,CENT_LAT,CENT_LONG,NUMBER_LAT_LONG,       
                         NUMBER_OF_SPECIES,NUMBER_OF_SAMPLES,SAMPLE_DESC_NAME, DATE_STUDY_ADDED,
                         # below info available only in public version 
                         METHODS,SUMMARY_METHODS, COMMENTS,WEB_LINK,LICENSE,DATA_SOURCE)

xxm_extra<-xxm_extra%>%dplyr::select(STUDY_ID,REALM,ORGANISMS,CLIMATE, HABITAT,TAXA,BIOME_MAP,
                                     AB_BIO,ABUNDANCE_TYPE, BIOMASS_TYPE,
                                     TITLE, AREA_SQ_KM, GRAIN_SQ_KM, PROTECTED_AREA, 
                                     DATA_POINTS,START_YEAR,END_YEAR,CENT_LAT,CENT_LONG,NUMBER_LAT_LONG,       
                                     NUMBER_OF_SPECIES,NUMBER_OF_SAMPLES,SAMPLE_DESC_NAME, DATE_STUDY_ADDED)%>%
  mutate( # below info available only in public version 
    METHODS=NA,SUMMARY_METHODS=NA, COMMENTS=NA,WEB_LINK=NA,LICENSE=NA,DATA_SOURCE=NA)

xxm_all<-rbind(xxm,xxm_extra)
# check
#any(xxm_all$DATA_POINTS<data_pt_thrs) # should be FALSE

saveRDS(xxm_all,"../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
#===============================================

# now get those data
xx<-read.csv("../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEQuery_24_06_2021.csv") # a dataframe
saveRDS(xx,"../DATA/for_BioTIME/raw_data/accessed31Aug2021/BioTIMEQuery24_06_2021.RDS")
# read the data from private version
xx_private<-read.csv("../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019.csv")
xx_extra<-xx_private%>%filter(STUDY_ID%in%xxm_extra$STUDY_ID)
saveRDS(xx_extra,"../DATA/for_BioTIME/raw_data/BioTIMEData_Blowes-Supp-etal-2019/BioTIMEQueryScienceStudies2019_extra.RDS")

# now combine
all(colnames(xx)==colnames(xx_extra))==T
xx_all<-rbind(xx,xx_extra)

bt<-dplyr::inner_join(xxm_all, xx_all, by='STUDY_ID')
# check again
#any(bt$DATA_POINTS<data_pt_thrs) # should be FALSE

saveRDS(bt,"../DATA/for_BioTIME/BioTIME_public_private_data_metadata.RDS")

bt<-bt%>%filter(REALM%in%c('Freshwater','Terrestrial'))%>%
  filter(TAXA%in%c('Birds','Terrestrial invertebrates','Fish','Freshwater invertebrates'))

grid <- bt %>% 
  dplyr::select(STUDY_ID, PLOT, DAY, MONTH, YEAR, 
                GENUS_SPECIES, sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS,
                CLIMATE, REALM, TAXA, ABUNDANCE_TYPE, BIOMASS_TYPE, 
                LATITUDE, LONGITUDE, CENT_LAT, CENT_LONG, NUMBER_LAT_LONG, SUMMARY_METHODS)
colnames(grid)[6:8] <- c('Species', 'Abundance', 'Biomass')

tbl<-grid%>%group_by(STUDY_ID,REALM)%>%summarise(n_distinct(YEAR))%>% ungroup()
tbl<-tbl%>%filter(`n_distinct(YEAR)`>=data_pt_thrs) # so, these are the 40 study_IDs we will start with

grid<-grid%>%filter(STUDY_ID%in%tbl$STUDY_ID)
saveRDS(grid,paste("../DATA/for_BioTIME/BioTIME_public_private_data_metadata_minyr_",data_pt_thrs,".RDS",sep=""))



