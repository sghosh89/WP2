#============================================
rm(list=ls())
library(dplyr)
library(tidyverse)
#====================
# tmax (1901-2016)
#####################
d<-read.csv("../DATA/wrangled_env_data/CHELSAcruts_tmax_monthlyvalues_extracted_lonlat.csv")
nd<-ncol(d)
# convert to long format
dl<-gather(d, month_yr, values, 3:nd, factor_key=F)
class(dl$month_yr)
dl$month_yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$yr<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[-1], collapse = "_"))
dl$month<-sapply(strsplit(dl$month_yr, "_"), function(x) paste(x[1], collapse = "_"))
dl<-dl%>%dplyr::select(-month_yr)
#tb<-dl%>%filter(month%in%c("6","7","8"))
tb<-dl%>%group_by(CENT_LONG,CENT_LAT,yr)%>%
  summarise(meanval=mean(values))%>%ungroup()
write.csv(tb,"../DATA/wrangled_env_data/CHELSAcruts_tmax_annualval_extracted_lonlat.csv",row.names = F)
#====================
# tmin (1901-2016)
#####################
d<-read.csv("../DATA/wrangled_env_data/CHELSAcruts_tmin_monthlyvalues_extracted_lonlat.csv")
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
write.csv(tb,"../DATA/wrangled_env_data/CHELSAcruts_tmin_annualval_extracted_lonlat.csv",row.names = F)
#====================
# prec (1901-2016)
#####################
d<-read.csv("../DATA/wrangled_env_data/CHELSAcruts_prec_monthlyvalues_extracted_lonlat.csv")
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
write.csv(tb,"../DATA/wrangled_env_data/CHELSAcruts_prec_annualval_extracted_lonlat.csv",row.names = F)

###################################################
rm(list=ls())
library(dplyr)
library(tidyverse)
library(e1071)

# first with tmax file
tmax<-read.csv("../DATA/wrangled_env_data/CHELSAcruts_tmax_annualval_extracted_lonlat.csv")
s<-read.csv("../Results/data_summary.csv") #2759 obs. with 1891 unique lonlat







