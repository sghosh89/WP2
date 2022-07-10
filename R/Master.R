path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
#==========================================================
rm(list=ls())
source("get_unique_lonlat.R") # extract community lon-lat
source("data_we_need.R")
source("wrangling_env_data.R") # get annual time series for env data from CHELSA (1979 onwards)
#source("raugh_psem.R") # path analysis with piecewiseSEM