# After you gather all data, then you can see for multiple newsites nested within a STUDY_ID 
# (especially for BioTIME database on manual check), all the metric (L,U,iCValt, etc.) are same. That means 
# they are duplicated data (something is wrong there in the rawdata)

# for example, see for STUDY_ID 229

library(here)
library(dplyr)
library(tidyverse)
`%notin%` <- Negate(`%in%`)
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))

data1<-readRDS(here("DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/229/STUDY_ID_229_LAT35.2087_LON-83.3515/allspecies_timeseries_and_metadata.RDS"))
data1<-data1$spmat

data2<-readRDS(here("DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/229/STUDY_ID_229_LAT35.04016_LON-83.36127/allspecies_timeseries_and_metadata.RDS"))
data2<-data2$spmat

all.equal(data1,data2)#TRUE
# so you can see there are multiple nested sites for which we have exactly same data recorded for a given STUDY_ID

# Now we need to automate the code to find such duplicated sites
# the easiest way is to go through the sm_all table and find same L, U, iCValt for a given STUDY_ID

checktab1<-sm_all%>%group_by(source,STUDY_ID)%>%summarise(nsites=n_distinct(newsite),
                                                          nuq_stab=n_distinct(iCValt))%>%ungroup()
# In the above table, whenever you find nsites!=nuq_stab, there are some duplicated sites

duplicated_tab<-checktab1%>%filter(nsites!=nuq_stab)

# So, we need to choose only any one site for a given STUDY_ID
sm_all_good<-sm_all%>%filter(STUDY_ID%notin%duplicated_tab$STUDY_ID)
sm_all_bad<-sm_all%>%filter(STUDY_ID%in%duplicated_tab$STUDY_ID)
sm_all_good2<-sm_all_bad%>%distinct(STUDY_ID,.keep_all = T)# distinct function always choose the first row

sm_all_good<-rbind(sm_all_good,sm_all_good2)
sm_all_good<-sm_all_good%>%arrange(source)#1925 observations in total
write.csv(sm_all_good,here("Results/stability_metric_and_env_all.csv"),row.names=F)



