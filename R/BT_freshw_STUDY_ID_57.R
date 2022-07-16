source("./tail_analysis.R")
library(dplyr)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

# don't overwrite these four variables
#metadata_BT<-readRDS("../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
#grid_freshw<-readRDS("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/bt_freshw_min20yr_rawdata.RDS")
#freshw_tbl_for_map<-readRDS("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")
#env_BT<-read.csv("../DATA/for_BioTIME/wrangled_data/annual_tas_CHELSA_1979_2019_BT_lonlat.csv")

df<-freshw_tbl_for_map%>%filter(STUDY_ID==57)
df$newsite<-df$STUDY_ID # this is the same as there is single site
#----------- create result folder for wrangle ddata -------------------------
resloc<-"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
saveRDS(df,"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/57/wrangledtable.RDS")
#================= filter data only for this site  =========================
site<-df$STUDY_ID
x<-grid_freshw%>%filter(STUDY_ID==site)
# df ensures there is only one single site with each month sampling once in a given year
unique(x$MONTH) # No month info available
#==================== saving input spmat  ====================
sort(unique(x$Species))

# do not consider these unknown sp into analysis
x<-x%>%filter(Species%notin%c("unspecifiable ","Unknown","Unknown rotifer", "Unknown rotifer2", "unknown ","Unknown "))

x<-x%>%dplyr::select(YEAR,Species,Value=Abundance)
x<-x%>%group_by(Species,YEAR)%>%
  dplyr::summarise(Value=median(Value))%>%ungroup()

c1<-x%>%group_by(Species)%>%summarise(n_distinct(YEAR))%>%ungroup() 
# As all species are not found each year, we need to fill in the missing values with 0.
x_c<-x %>% 
  complete(Species, 
           nesting(YEAR), 
           fill = list(Value = 0))
xmat<-x_c%>%spread(Species, Value)
year<-xmat$YEAR
xmat<-as.matrix(xmat[,-1])
rownames(xmat)<-year

xmeta<-metadata_BT%>%filter(STUDY_ID==site)

input_sp<-list(spmat=xmat,meta=xmeta)
saveRDS(input_sp,paste(resloc,"allspecies_timeseries_and_metadata.RDS",sep=""))

#==================== saving input spmat for tailanal for 57 ====================
m<-readRDS(paste(resloc,"allspecies_timeseries_and_metadata.RDS",sep=""))
presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
presentyr<-unname(presentyr)
commonspid<-which(presentyr>=0.7*nrow(m$spmat)) # common sp = present minimum 70% of sampled year

if(length(commonspid)>0){
  m1<-m$spmat[,commonspid]
  m1<-as.data.frame(m1)
  input_tailanal<-m1
}else{
  cat("no common species are found \n")
}
saveRDS(input_tailanal,paste(resloc,"commonspecies_timeseries.RDS",sep=""))
tot_target_sp<-ncol(input_tailanal)
saveRDS(tot_target_sp,paste(resloc,"tot_target_sp.RDS",sep="")) # nsp

#----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
input_tailanal$yr<-as.integer(rownames(input_tailanal))
input_tailanal<-input_tailanal%>%filter(yr%in%c(1979:2019))
rownames(input_tailanal)<-input_tailanal$yr

#-----------------------adding environmental variable in the matrix-----------------------------
tempdat<-env_BT%>%filter(STUDY_ID%in%site)%>%filter(yr%in%rownames(input_tailanal))%>%dplyr::select(yr,t,tmax,tmin)
tempdat$tmax_n<- -tempdat$tmax

# check if all TRUE
all(rownames(input_tailanal)==tempdat$yr)==T

input_tailanal$t<-tempdat$t
input_tailanal$tmax<-tempdat$tmax
input_tailanal$tmin<-tempdat$tmin
input_tailanal$tmax_n<-tempdat$tmax_n
input_tailanal<-input_tailanal%>%dplyr::select(-yr)

saveRDS(input_tailanal,paste(resloc,"input_mat_for_tailanal_with_env.RDS",sep="")) # dataframe with species timeseries along column

#----------- tail analysis ----------------
resloc_output<-"../Results/for_BioTIME/Freshwater_plotlevel/57/"
if(!dir.exists(resloc_output)){
  dir.create(resloc_output)
}
res<-tail_analysis(mat = input_tailanal, tot_target_sp=tot_target_sp, resloc = resloc_output, nbin = 2)










