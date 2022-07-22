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

#df_lonlat<-df_lonlat[1:3,]
df_lonlat_table<-df_lonlat

plot(df_lonlat$CENT_LAT ~ df_lonlat$CENT_LONG)

# now make it as sp object
coordinates(df_lonlat) <- ~CENT_LONG + CENT_LAT
proj4string(df_lonlat) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(df_lonlat)

get_values<- raster::extract(allrasters, df_lonlat)
df_env<- as.data.frame(get_values)
cnm<-colnames(df_env)
cnm<-substr(cnm,8,21)
colnames(df_env)<-cnm # month_year
df_env <- cbind(df_lonlat_table,df_env)
write.csv(df_env,"../../../../DATA/wrangled_env_data/tasmin_monthlyvalues_extracted_lonlat.csv",row.names = F)

