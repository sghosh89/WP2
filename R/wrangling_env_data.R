# CHELSA database provides monthly data of res 30 Arc Sec ~ 1 Km for whole 
# world, we extracted for the unique lon-lat we needed and 
# those tas, tasmax, tasmin, pr data 
# are saved in the DATA/wrangled_env_data/ folder
# Now, we will wrangle those file to get avg data across 12 months 
# so to get annual data
#============================================
rm(list=ls())
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
###################################################
rm(list=ls())
library(dplyr)
library(tidyverse)
library(e1071)

# first with tasmax file
t<-read.csv("../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv")
tmax<-read.csv("../DATA/wrangled_env_data/tasmax_annualval_extracted_lonlat.csv")
tmin<-read.csv("../DATA/wrangled_env_data/tasmin_annualval_extracted_lonlat.csv")
pr<-read.csv("../DATA/wrangled_env_data/pr_annualval_extracted_lonlat.csv")

s<-read.csv("../Results/data_summary.csv") #2759 obs. with 1891 unique lonlat

s2<-s%>%filter(startyr>=1979 & endyr<=2018) #1463 obs.
s2$dummy<-paste(s2$CENT_LONG,"_",
                       s2$CENT_LAT,sep="")

s2$dummy2<-paste(s2$CENT_LONG,"_",
                        s2$CENT_LAT,"_",
                        s2$startyr,"_",
                        s2$endyr,sep="")

s2_lonlat_yrs<-s2[!duplicated(s2[ , c("dummy2")]), ]
nrow(s2_lonlat_yrs)# with 690 unique lonlat and year span

s2_lonlat <- s2[!duplicated(s2[ , c("dummy")]), ] 
nrow(s2_lonlat) # with 685 unique lonlat (1979-2018)

xx<-anti_join(s2_lonlat_yrs, s2_lonlat, by='dummy2')

as.data.frame(table(s$TAXA)) #initial data
as.data.frame(table(s2$TAXA)) # data with 1979-2018 constraint

#so i have decided to go with the subsetted data (1979-2018) 
udf<-s2_lonlat_yrs # this is the dataframe you start with
nrow(udf) #690

# for each row first calculate the skewness, variability, ...
# of tas, tasmax, tasmin, pr, for a given lon-lat and yearspan
udf$t_var<-NA # variability of temperature, median/IQR
udf$t_skw<-NA # skewness of temperature of the yearspan
udf$t_median<-NA # median of temperature of the yearspan
  
udf$tmax_var<-NA # variability of max temperature, median/IQR
udf$tmax_skw<-NA # skewness of max temperature of the yearspan
udf$tmax_median<-NA # median of max temperature of the yearspan

udf$tmin_var<-NA # variability of minimum temperature, median/IQR
udf$tmin_skw<-NA # skewness of minimum temperature of the yearspan
udf$tmin_median<-NA # median of minimum temperature of the yearspan

udf$pr_var<-NA # variability of precipitation, median/IQR
udf$pr_skw<-NA # skewness of precipitation of the yearspan
udf$pr_median<-NA # median of precipitation of the yearspan

for(i in 1:nrow(udf)){
  
  tg_lon<-udf$CENT_LONG[i]
  tg_lat<-udf$CENT_LAT[i]
  tg_syr<-udf$startyr[i]
  tg_eyr<-udf$endyr[i]
  
  tempo<-t%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$t_median[i]<-median(tempo$meanval)
  udf$t_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$t_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T)
  
  tempo<-tmax%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$tmax_median[i]<-median(tempo$meanval)
  udf$tmax_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$tmax_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T)
  
  tempo<-tmin%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$tmin_median[i]<-median(tempo$meanval)
  udf$tmin_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$tmin_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T) 
  
  tempo<-pr%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$pr_median[i]<-median(tempo$meanval)
  udf$pr_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$pr_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T) 
}

# check hypo
# temp would be negatively skewed
sum(udf$t_skw<0)
sum(udf$t_skw>0)

sum(udf$tmax_skw<0)
sum(udf$tmax_skw>0)

sum(udf$tmin_skw<0)
sum(udf$tmin_skw>0)

# pr would be positively skewed
sum(udf$pr_skw<0)
sum(udf$pr_skw>0)


udf1<-udf%>%dplyr::select(dummy2,
                          t_var,t_skw,t_median,
                          tmax_var,tmax_skw,tmax_median,
                          tmin_var,tmin_skw,tmin_median,
                          pr_var,pr_skw,pr_median)
s2_env<-left_join(s2,udf1,by="dummy2")
write.csv(s2_env,"../Results/data_summary_with_env_yrspan_1979_2018.csv",row.names = F)

#============= repeat the analysis for temp data 1979 to 2019 ================

rm(list=ls())
library(dplyr)
library(tidyverse)
library(e1071)

# temperature file
t<-read.csv("../DATA/wrangled_env_data/tas_annualval_extracted_lonlat.csv")
tmax<-read.csv("../DATA/wrangled_env_data/tasmax_annualval_extracted_lonlat.csv")
tmin<-read.csv("../DATA/wrangled_env_data/tasmin_annualval_extracted_lonlat.csv")

s<-read.csv("../Results/data_summary.csv") #2759 obs. with 1891 unique lonlat

s2<-s%>%filter(startyr>=1979 & endyr<=2019) #1463 obs.
s2$dummy<-paste(s2$CENT_LONG,"_",
                s2$CENT_LAT,sep="")

s2$dummy2<-paste(s2$CENT_LONG,"_",
                 s2$CENT_LAT,"_",
                 s2$startyr,"_",
                 s2$endyr,sep="")
s2_lonlat_yrs<-s2[!duplicated(s2[ , c("dummy2")]), ]
nrow(s2_lonlat_yrs)# with 1802 unique lonlat and year span

s2_lonlat <- s2[!duplicated(s2[ , c("dummy")]), ] 
nrow(s2_lonlat) # with 1797 unique lonlat (1979-2018)

xx<-anti_join(s2_lonlat_yrs, s2_lonlat, by='dummy2')

as.data.frame(table(s$TAXA)) #initial data
as.data.frame(table(s2$TAXA)) # data with 1979-2018 constraint

#so i have decided to go with the subsetted data (1979-2018) 
udf<-s2_lonlat_yrs # this is the dataframe you start with
nrow(udf) #1802

# for each row first calculate the skewness, variability, ...
# of tas, tasmax, tasmin, pr, for a given lon-lat and yearspan
udf$t_var<-NA # variability of temperature, median/IQR
udf$t_skw<-NA # skewness of temperature of the yearspan
udf$t_median<-NA # median of temperature of the yearspan

udf$tmax_var<-NA # variability of max temperature, median/IQR
udf$tmax_skw<-NA # skewness of max temperature of the yearspan
udf$tmax_median<-NA # median of max temperature of the yearspan

udf$tmin_var<-NA # variability of minimum temperature, median/IQR
udf$tmin_skw<-NA # skewness of minimum temperature of the yearspan
udf$tmin_median<-NA # median of minimum temperature of the yearspan

for(i in 1:nrow(udf)){
  
  tg_lon<-udf$CENT_LONG[i]
  tg_lat<-udf$CENT_LAT[i]
  tg_syr<-udf$startyr[i]
  tg_eyr<-udf$endyr[i]
  
  tempo<-t%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$t_median[i]<-median(tempo$meanval)
  udf$t_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$t_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T)
  
  tempo<-tmax%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$tmax_median[i]<-median(tempo$meanval)
  udf$tmax_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$tmax_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T)
  
  tempo<-tmin%>%filter(CENT_LAT==tg_lat & CENT_LONG==tg_lon)
  tempo<-tempo%>%filter(yr%in%c(tg_syr:tg_eyr))
  udf$tmin_median[i]<-median(tempo$meanval)
  udf$tmin_var[i]<-IQR(tempo$meanval)/median(tempo$meanval)
  udf$tmin_skw[i]<-skewness(x=tempo$meanval,type=2,na.rm = T) 
  
}

udf1<-udf%>%dplyr::select(dummy2,
                          t_var,t_skw,t_median,
                          tmax_var,tmax_skw,tmax_median,
                          tmin_var,tmin_skw,tmin_median)
s2_env<-left_join(s2,udf1,by="dummy2")
write.csv(s2_env,"../Results/data_summary_with_temperature_yrspan_1979_2019.csv",row.names = F)
