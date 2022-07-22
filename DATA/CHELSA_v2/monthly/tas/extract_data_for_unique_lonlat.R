###############################################################################################################
# This file has to be in the same folder where you save the environmental data 
# that you will extract
###############################################################################################################
path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
library(raster)
library(sp)
rastlist <- list.files(path = "./", pattern='.tif$', all.files=TRUE, full.names=FALSE)
allrasters <- raster::stack(rastlist)
df_lonlat<-read.csv("../../../../Results/unique_lonlat.csv")
#rownames(df_lonlat)<-paste("lon_",df_lonlat$CENT_LONG,"_lat_",df_lonlat$CENT_LAT,sep="")

#df_lonlat<-df_lonlat[7,]
df_lonlat_table<-df_lonlat

plot(df_lonlat$CENT_LAT~df_lonlat$CENT_LONG)

# now make it as sp object
coordinates(df_lonlat) <- ~CENT_LONG + CENT_LAT
proj4string(df_lonlat) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
class(df_lonlat)

get_values<- raster::extract(allrasters, df_lonlat)
df_env<- as.data.frame(get_values)
cnm<-colnames(df_env)
cnm<-substr(cnm,12,18)
colnames(df_env)<-paste("tas_",cnm,sep="") # don't start column names with number, otherwise will add X in reading csv files
df_env <- cbind(df_lonlat_table,df_env)
write.csv(df_env,"../../../../DATA/wrangled_env_data/tas_monthlyvalues_extracted_lonlat.csv",row.names = F)


