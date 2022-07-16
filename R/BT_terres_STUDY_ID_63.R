source("./tail_analysis.R")
source("./monthly_rarefy_BT.R")
library(dplyr)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

# don't overwrite these four variables
#metadata_BT<-readRDS("../DATA/for_BioTIME/BioTIME_public_private_metadata.RDS")
#grid_terres<-readRDS("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/bt_terres_min20yr_rawdata.RDS")
#terres_tbl_for_map<-readRDS("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/table_for_map.RDS")
#env_BT<-read.csv("../DATA/for_BioTIME/wrangled_data/annual_tas_CHELSA_1979_2019_BT_lonlat.csv")

df<-terres_tbl_for_map%>%filter(STUDY_ID==63)
df$newsite<-df$STUDY_ID # this is the same as there is single site
#----------- create result folder for wrangle ddata -------------------------
resloc<-"../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/63/"
if(!dir.exists(resloc)){
  dir.create(resloc)
}
#--------------------------------------------------------------------------------
site<-df$STUDY_ID
x<-grid_terres%>%filter(STUDY_ID==site)
newsite<-site
unique(x$MONTH) # no monthly info

# Now, create folder for all these newsite
if(length(newsite)>1){
  for(k in 1:length(newsite)){
    resloc2<-paste("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/63/",newsite[k],"/",sep="")
    if(!dir.exists(resloc2)){
      dir.create(resloc2)
    }
  }
}
#------------------------------------------------------------
newsite_bad<-c()


if(length(newsite)>1){
  x<-x%>%mutate(newsite=paste("STUDY_ID_",site,"_PLOT_",PLOT,sep=""))
  newsite<-sort(unique(x$newsite))
  
  # check if each newsite visited for >20 years?
  tt<-x%>%group_by(newsite)%>%summarise(n=n_distinct(YEAR))%>%ungroup()
  
  # include sites which are sampled > 20 years
  tt<-tt%>%filter(n>=20)
  x_allsite<- x %>% filter(newsite %in% tt$newsite)
  newsite<-tt$newsite
}else{
  x<-grid_terres%>%filter(STUDY_ID==site)
  newsite<-site
  x_allsite<-x
}
#----------------------------
for(k in 1:length(newsite)){
  
  x<-x_allsite%>%filter(newsite==newsite[k])
  
  # do not consider these unknown sp into analysis
  x<-x%>%filter(Species%notin%c("Unknown","Unknown "))
  
  t0<-x%>%group_by(YEAR)%>%summarise(nm=n_distinct(MONTH))%>%ungroup()
  t1<-x%>%group_by(YEAR,MONTH)%>%summarise(nd=n_distinct(DAY))%>%ungroup()
  
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
    study<-x%>%dplyr::select(DAY,MONTH,YEAR,Species,Value=id)
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
  
  input_sp<-list(spmat=xmat,meta=xmeta)
  
  if(length(newsite)>1){
    resloc<-paste("../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/63/",newsite[k],"/",sep="")
  }else{
    resloc<-"../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/63/"
  }
  
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
    tot_target_sp<-ncol(input_tailanal)
  }else{
    input_tailanal<-NA
    tot_target_sp<-NA
    cat("no common species are found \n")
  }
  saveRDS(input_tailanal,paste(resloc,"commonspecies_timeseries.RDS",sep=""))
  saveRDS(tot_target_sp,paste(resloc,"tot_target_sp.RDS",sep="")) # nsp
  
  #----------------- consider the matrix data only in between 1979 to 2019 period to match with env data------------------
  input_tailanal$yr<-as.integer(rownames(input_tailanal))
  input_tailanal<-input_tailanal%>%filter(yr%in%c(1979:2019))
  rownames(input_tailanal)<-input_tailanal$yr
  
  #check again
  suffyr<-(nrow(input_tailanal)>=20)
  if(suffyr==T){
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
    resloc2<-paste("../Results/for_BioTIME/Terrestrial_plotlevel/",site,"/",sep="")
    if(!dir.exists(resloc2)){
      dir.create(resloc2)
    }
    if(length(newsite)>1){
      resloc<-paste(resloc2,newsite[k],"/",sep="")
    }else{
      resloc<-resloc2
    }
    
    res<-tail_analysis(mat = input_tailanal, tot_target_sp=tot_target_sp, resloc = resloc, nbin = 2)
    
    cat("---------- k = ",k,"-----------\n")
  }else{
    cat("---------- number of years consistent with env stats is not sufficient -----------\n")
    newsite_bad<-c(newsite_bad,newsite)
  }
}

newsite<-setdiff(newsite,newsite_bad)
saveRDS(newsite,"../DATA/for_BioTIME/wrangled_data/Terrestrial_plotlevel/63/newsite.RDS")