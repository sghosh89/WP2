# CHELSA database provides monthly data of res 30 Arc Sec ~ 1 Km for whole 
# world, we extracted for the unique lon-lat we needed and 
# those tas, tasmax, tasmin, pr data 
# are saved in the DATA/wrangled_env_data/ folder
# Now, we will wrangle those file to get avg data across 12 months 
# so to get annual data
#============================================
library(dplyr)
library(tidyverse)
######################
#   tas (1979-2019)
######################
d<-read.csv("../DATA/wrangled_env_data/tas_monthlyvalues_extracted_lonlat.csv")
nd<-ncol(d)
# convert to long format
dl<-gather(d, month_yr, values, 3:nd, factor_key=F)
class(dl$month_yr)
dl$month_yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$month<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[1], collapse = "_"))
dl<-dl%>%dplyr::select(-month_yr)
#tb<-dl%>%filter(month%in%c("06","07","08"))
tb<-dl%>%group_by(CENT_LONG,CENT_LAT,yr)%>%
  summarise(meanval=mean(values))%>%ungroup()
write.csv(tb,"../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv",row.names = F)

######################
#   tasmax (1979-2019)
######################
d<-read.csv("../DATA/wrangled_env_data/tasmax_monthlyvalues_extracted_lonlat.csv")
nd<-ncol(d)
# convert to long format
dl<-gather(d, month_yr, values, 3:nd, factor_key=F)
class(dl$month_yr)
dl$month_yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$month<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[1], collapse = "_"))
dl<-dl%>%dplyr::select(-month_yr)
tb<-dl%>%group_by(CENT_LONG,CENT_LAT,yr)%>%
  summarise(meanval=mean(values))%>%ungroup()
write.csv(tb,"../DATA/wrangled_env_data/tasmax_annualval_extracted_lonlat.csv",row.names = F)

######################
#   tasmin (1979-2019)
######################
d2<-read.csv("../DATA/wrangled_env_data/tasmin_monthlyvalues_extracted_lonlat.csv")
nd<-ncol(d2)
# convert to long format
dl2<-gather(d2, month_yr, values, 3:nd, factor_key=F)
class(dl2$month_yr)
dl2$month_yr<-sapply(strsplit(dl2$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl2$yr<-sapply(strsplit(dl2$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl2$month<-sapply(strsplit(dl2$month_yr, "_"), function(x) paste(x[1], collapse = "_"))
dl2<-dl2%>%dplyr::select(-month_yr)
tb2<-dl2%>%group_by(CENT_LONG,CENT_LAT,yr)%>%
  summarise(meanval=mean(values))%>%ungroup()
write.csv(tb2,"../DATA/wrangled_env_data/tasmin_annualval_extracted_lonlat.csv",row.names = F)

######################
# pr (1979-2018)
######################
d<-read.csv("../DATA/wrangled_env_data/pr_monthlyvalues_extracted_lonlat.csv")
nd<-ncol(d)
# convert to long format
dl<-gather(d, month_yr, values, 3:nd, factor_key=F)
class(dl$month_yr)
dl$month_yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$month<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[1], collapse = "_"))
dl<-dl%>%dplyr::select(-month_yr)
#tb<-dl%>%filter(month%in%c("06","07","08"))
tb<-dl%>%group_by(CENT_LONG,CENT_LAT,yr)%>%
  summarise(meanval=mean(values))%>%ungroup()
write.csv(tb,"../DATA/wrangled_env_data/pr_annualval_extracted_lonlat.csv",row.names = F)

###################################################
