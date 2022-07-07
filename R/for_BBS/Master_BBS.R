path<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

# wrangling data, find good routes, get community matrix minimum for 20 years, 
# species included sampled for at least 70% of sampling period, sp_threshold=4
source("data_wrangling.R") 
source("BBS_ta.R")         # tail analysis for each route