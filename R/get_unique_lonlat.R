rm(list=ls())
library(tidyverse)
##########################
# first read all the data/metadata collected for proj BioDyn
s<-read.csv("../Results/data_summary.csv")

# get first unique set of long+lat combination
s_lonlat<-s%>%select(CENT_LONG,CENT_LAT)
s_lonlat <- s_lonlat[!duplicated(s_lonlat[ , c("CENT_LONG", "CENT_LAT")]), ] 
rownames(s_lonlat)<-NULL
nrow(s_lonlat) # 1891 unique lon+lat combo

write.csv(s_lonlat,"../Results/unique_lonlat.csv",row.names = F)
#######################################

