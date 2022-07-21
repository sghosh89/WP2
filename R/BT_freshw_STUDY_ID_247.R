source("./tail_analysis.R")
source("./monthly_rarefy_BT.R")
library(tidyverse)
library(dplyr)
`%notin%` <- Negate(`%in%`)

# don't overwrite these four variables
#metadata_BT<-readRDS("../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
#grid_freshw<-readRDS("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/bt_freshw_min20yr_rawdata.RDS")
#freshw_tbl_for_map<-readRDS("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/table_for_map.RDS")
#env_BT_t<-read.csv("../DATA/for_BioTIME/wrangled_data/annual_tas_CHELSA_1979_2019_BioTIME_lonlat.csv")

df<-freshw_tbl_for_map%>%filter(STUDY_ID==247)# multiple sites
#----------- create result folder for wrangle ddata -------------------------
resloc<-"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------------------------------------------
site<-df$STUDY_ID
x<-grid_freshw%>%filter(STUDY_ID==site)
#x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_LON",LONGITUDE,"_LAT",LATITUDE,sep=""))
x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
newsite<-sort(unique(x$newsite))

# check if each newsite visited for >20 years?
tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()

# include sites which are sampled > 20 years
tt<-tt%>%filter(n>=20)

#update
x_allsite<- x %>% filter(newsite %in% tt$newsite)
newsite<-tt$newsite

mylonlat<-data.frame(LONGITUDE=x_allsite$LONGITUDE,
                     LATITUDE=x_allsite$LATITUDE,
                     lonlat=paste(x_allsite$LONGITUDE,x_allsite$LATITUDE,sep="_"))
mylonlat<-mylonlat%>%distinct(lonlat,.keep_all = T)
write.csv(mylonlat,"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/mylonlat.csv",row.names = F)
#-------------------------------------------------------------------------------------------------------------
# sometimes months have different multiple sampling dates within a year
# so, take the average
x_allsite$Abundance<-as.numeric(x_allsite$Abundance)
x_allsite$Biomass<-as.numeric(x_allsite$Biomass)
x_allsite<-x_allsite%>%group_by(newsite,YEAR,MONTH,Species)%>%
  summarise(Abundance=mean(Abundance,na.rm=T),
            Biomass=mean(Biomass,na.rm=T),
            ABUNDANCE_TYPE=unique(ABUNDANCE_TYPE),
            LATITUDE=LATITUDE,
            LONGITUDE=LONGITUDE)%>%ungroup()
# NOTE: ABUNDANCE TYPE should be kept as it is - if NA then keep NA
#------------------------------------------------------------------------------------------------------------
# Now, create folder for all these newsite
for(k in 1:length(newsite)){
  resloc2<-paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/",newsite[k],"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
}
#------------------------------------------------------------
for(k in 1:length(newsite)){
  
  x<-x_allsite%>%filter(newsite==newsite[k])
  
  # do not consider these unknown sp into analysis
  x<-x%>%filter(Species%notin%c("unspecifiable ","Unknown","Unknown rotifer", "Unknown rotifer2", "unknown ","Unknown "))
  
  t0<-x%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH))%>%ungroup()
  #t1<-x%>%group_by(YEAR,MONTH)%>%summarise(nd=n_distinct(DAY))%>%ungroup()
  
  #---------- ok, after seeing t0, we need to rarefy --------------
  min_samp<-min(t0$nm) # min months sampled each year
  cat("---------- min_samp = ",min_samp," , newsite = ",newsite[k]," ------------------- \n")
  need_rarefy<-length(unique(t0$nm))>1
  
  AB<-is.na(x$ABUNDANCE_TYPE)[1]
  if(AB==F){
    field<-"Abundance"
  }else{
    field<-"Biomass"
  }
  
  id<-which(colnames(x)==field)
  
  if(need_rarefy==T){
    study<-x%>%dplyr::select(MONTH,YEAR,Species,Value=id)
    x_c<-monthly_rarefy(study = study,resamples = 100,field = field)
  }else{
    x<-x%>%dplyr::select(YEAR,Species,Value=id)
    x<-x%>%group_by(Species,YEAR)%>%
      dplyr::summarise(Value=mean(Value))%>%ungroup()
    c1<-x%>%group_by(Species)%>%summarise(n_distinct(YEAR))%>%ungroup() 
    # As all species are not found each year, we need to fill in the missing values with 0.
    x_c<-x %>% 
      complete(Species, 
               nesting(YEAR), 
               fill = list(Value = 0))
  }
  
  #-----------------------------------------------------------------
  xmat<-x_c%>%spread(Species, Value)
  year<-xmat$YEAR
  xmat<-as.matrix(xmat[,-1])
  rownames(xmat)<-year
  
  xmeta<-metadata_BT%>%filter(STUDY_ID==site)
  xmeta$lonlat<-mylonlat$lonlat[k]
  
  input_sp<-list(spmat=xmat,meta=xmeta)
  resloc<-paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/",newsite[k],"/",sep="")
  saveRDS(input_sp,paste(resloc,"allspecies_timeseries_and_metadata.RDS",sep=""))
  
  #----------- saving input spmat for tailanal ---------------------
  m<-readRDS(paste(resloc,"allspecies_timeseries_and_metadata.RDS",sep=""))
  # first we aggregated the rare sp (present even less than 30% of sampled years) into a pseudo sp 
  presentyr<-apply(X=m$spmat,MARGIN=2,FUN=function(x){sum(x>0)})
  presentyr<-unname(presentyr)
  commonspid<-which(presentyr>=0.7*nrow(m$spmat)) # common sp = present minimum 70% of sampled year
  if(length(commonspid)>0){
    m1<-m$spmat[,commonspid]
    m1<-as.data.frame(m1)
    input_tailanal<-m1
  }else{
    input_tailanal<-NA
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
  tempdat<-env_BT_t%>%filter(lonlat%in%xmeta$lonlat)%>%filter(yr%in%rownames(input_tailanal))%>%dplyr::select(yr,t,tmax,tmin)
  tempdat$tmax_n<- -tempdat$tmax
  
  # check if all TRUE
  all(rownames(input_tailanal)==tempdat$yr)==T
  
  input_tailanal$t<-tempdat$t
  input_tailanal$tmax<-tempdat$tmax
  input_tailanal$tmin<-tempdat$tmin
  input_tailanal$tmax_n<-tempdat$tmax_n
  input_tailanal<-input_tailanal%>%dplyr::select(-yr)
  
  saveRDS(input_tailanal,paste(resloc,"input_mat_for_tailanal_with_env.RDS",sep="")) # dataframe with species timeseries along column
  
  #----------------- now do tail analysis ----------------------
  resloc2<-paste("../Results/for_BioTIME/Freshwater_plotlevel/",site,"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
  
  
  resloc<-paste(resloc2,newsite[k],"/",sep="")
  if(!dir.exists(resloc)){
    dir.create(resloc)
  }
  res<-tail_analysis(mat = input_tailanal, tot_target_sp=tot_target_sp,resloc = resloc, nbin = 2)
}

#--------------------------------------------------------
saveRDS(newsite,"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/247/newsite.RDS")

#----------------------- map selected sites------------------
library(htmltools) 
library(htmlwidgets)
library(leaflet) 

dat<-x_allsite%>%dplyr::select(newsite,LATITUDE,LONGITUDE)%>%distinct()
sitemap<-leaflet(dat) %>% addTiles() %>%
  addMarkers(~LONGITUDE, ~LATITUDE, label = ~htmlEscape(newsite))
f<-paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/samplingsite_247_selected.html",sep="")
htmlwidgets::saveWidget(sitemap, 
                        file.path(normalizePath(dirname(f)),basename(f)))





