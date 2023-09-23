library(dplyr)
library(tidyverse)
##########################
# first read all the metadata collected for proj BioDyn
md<-read.csv("../DATA/metadata_summary_with_citation.csv")
#========== get first unique set of long+lat combination for all metadata ===========
md_lonlat<-md%>%dplyr::select(CENT_LONG,CENT_LAT)
md_lonlat <- md_lonlat[!duplicated(md_lonlat[ , c("CENT_LONG", "CENT_LAT")]), ] 
rownames(md_lonlat)<-NULL
nrow(md_lonlat) 
write.csv(md_lonlat,"../Results/unique_lonlat.csv",row.names = F)
#===============================================================================