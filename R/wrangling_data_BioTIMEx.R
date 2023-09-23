library(tidyverse)
library(dplyr)
md<-read.csv("../DATA/metadata_summary_with_citation.csv") # metadata from BioDyn project, >=20 datapoints
md<-md%>%filter(source=="BioTIMEx")%>%filter(TAXA%in%c("birds","fish","freshwater invertebrates","terrestrial invertebrates"))
# We need to analyse these studies
unique(md$STUDY_ID) # 6 STUDY_IDs 
#--------------------------------------------
# carpenter_2016
source("carpenter_2016.R")

# cumbrian_zoo
source("cumbrian_zoo.R")

# landis_2018
source("landis_2018.R")

# lightfoot_2015
source("lightfoot_2015.R")

# oneida_fish_gillnets
source("oneida_fish_gillnets.R")

# oneida_fish_trawl
source("oneida_fish_trawl.R")

#===================================

