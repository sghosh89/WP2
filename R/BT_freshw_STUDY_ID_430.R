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

df<-freshw_tbl_for_map%>%filter(STUDY_ID==430)# multiple sites
site<-df$STUDY_ID
x<-grid_freshw%>%filter(STUDY_ID==site)
x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
newsite<-sort(unique(x$newsite))
# which newsite has atleast 20 yrs of sampling?
tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()
as.data.frame(table(tt$n))

x$Abundance<-as.numeric(x$Abundance)
x$Biomass<-as.numeric(x$Biomass)

# we need to group the newsite based on hydrobasins
latlon_xx<-distinct(x,newsite,.keep_all=T)
latlon_xx<-latlon_xx%>%dplyr::select(LATITUDE,LONGITUDE)

library(sf)
library(raster)
library(fasterize)

sp<-st_read('../DATA/for_BioTIME/raw_data/Hydrosheds/au_bas_30s_beta/au_bas_30s_beta.shp')
ras<-raster(extent(sp),resolution=0.01) #creat an empty raster
ras2<-fasterize(sp,ras,field='BASIN_ID',fun="first") #convert the polygons to raster, because polygons are overlapping, creating a problem to extract values then

latlon_xx2<-st_as_sf(latlon_xx,coords=c('LONGITUDE','LATITUDE'),crs=st_crs(sp)) #converting data to spatial data

latlon_xx2$basin<-extract(ras2,latlon_xx2) #extrcating infromation
latlon_xx<-cbind(as.data.frame(latlon_xx2),st_coordinates(latlon_xx2))
latlon_xx<-rename(latlon_xx,LATITUDE=Y,LONGITUDE=X)

ggplot()+
  geom_point(data=latlon_xx,aes(x=LONGITUDE,y=LATITUDE,color=as.factor(basin)))+
  geom_point(data=subset(latlon_xx,basin%in%c(80355,98961,100981,101416)),aes(x=LONGITUDE,y=LATITUDE),col="black")+
  theme(legend.position="none")

latlon_xx<-latlon_xx%>%mutate(newsite=paste("STUDY_ID_",site,"_LAT",LATITUDE,"_LON",LONGITUDE,sep=""))
latlon_xx<-latlon_xx%>%dplyr::select(newsite,basin)

x<-left_join(x,latlon_xx,by="newsite")
length(unique(x$basin))
#################################################################################################
mylonlat<-x%>%distinct(basin,.keep_all = T)%>%dplyr::select(basin,LONGITUDE,LATITUDE)%>%
  mutate(lonlat=paste(LONGITUDE,LATITUDE,sep="_"))
#################################################################################################
# now we are going to aggregate by basin
x_agg<-x%>%group_by(basin,YEAR,MONTH,Species)%>%summarise(Abundance=mean(Abundance),
                                                          Biomass=mean(Biomass))%>%ungroup()

tt<-x%>%group_by(basin)%>%summarise(n=n_distinct(YEAR))%>%ungroup()

# include sites which are sampled > 20 years
tt<-tt%>%filter(n>=20)

# update
mylonlat<-mylonlat%>%filter(basin%in%tt$basin)

# only four basins sampled for 24 years minimum
x_agg<-x_agg%>%filter(basin%in%tt$basin)
x_agg$CLIMATE<-unique(x$CLIMATE)
x_agg$REALM<-unique(x$REALM)
x_agg$TAXA<-unique(x$TAXA)
x_agg$ABUNDANCE_TYPE<-unique(x$ABUNDANCE_TYPE)
x_agg$BIOMASS_TYPE<-unique(x$BIOMASS_TYPE)
x_agg$site<-unique(x$STUDY_ID)
x_agg<-rename(x_agg,newsite=basin)
x_agg$newsite<-paste("STUDY_ID_430_basin_",x_agg$newsite,sep="")

site<-unique(x_agg$site)
#----------- create result folder for wrangle data -------------------------
resloc<-"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/430/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
newsite<-unique(x_agg$newsite)
saveRDS(newsite,paste(resloc,"newsite.RDS",sep=""))

# Now, create folder for all these newsite
for(k in 1:length(newsite)){
  resloc2<-paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/430/",newsite[k],"/",sep="")
  if(!dir.exists(resloc2)){
    dir.create(resloc2)
  }
}
#--------------------------------------------------------------

#------------ now format the data as per input format for tail analysis ------------

for(k in 1:length(newsite)){
  x<-x_agg%>%filter(newsite==newsite[k])
  
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
  resloc<-paste("../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/430/",newsite[k],"/",sep="")
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
saveRDS(newsite,"../DATA/for_BioTIME/wrangled_data/Freshwater_plotlevel/430/newsite.RDS")
