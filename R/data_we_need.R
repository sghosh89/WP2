# This script will tell which communities you need to extract from BioDyn data collection
# We need communities which is sampled from 1979 - to match with env data
rm(list=ls())
library(dplyr)
library(tidyverse)
######################
md<-read.csv("../Results/metadata_summary_from_BioDyn.csv")
md<-md%>%filter(startyr>=1979 & endyr<=2019) # 2575 obs.

as.data.frame(table(md$TAXA)) 
# we decided to extract data only for 4 taxa:
# birds, fish
# terrestrial invertebrates, freshwater invertebrates
md<-md%>%filter(TAXA%in%c("birds","fish","terrestrial invertebrates","freshwater invertebrates")) #1985 obs

as.data.frame(table(md$source))
# so, we will only run our analysis for those communities
# all BioDyn data is saved in data folder (BioDyn data will be made public in Dryad later)
md%>%group_by(TAXA,source)%>%summarise(n=n())%>%ungroup()



