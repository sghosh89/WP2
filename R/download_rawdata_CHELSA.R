# this script is showing an example for how to download data from CHELSA 
library(utils)

#=================== download monthly DATA: observed precipitation DATA ======================================
# 1979-2018, 12 months in a year
tb<-read.delim("../DATA/CHELSA_v2/monthly/pr/envidatS3paths.txt",header = F)
for(i in 1:nrow(tb)){
  readpath<-tb[i,1]
  destfilename<-trimws(basename(readpath))
  download.file(url=readpath,
                destfile= paste("../DATA/CHELSA_v2/monthly/pr/",destfilename,sep=""),mode="wb")
}

#=================== download monthly DATA: observed mean temperature DATA ======================================
# 1979-2019, 12 months in a year
tb<-read.delim("../DATA/CHELSA_v2/monthly/tas/envidatS3paths.txt",header = F)
for(i in 1:nrow(tb)){
  readpath<-tb[i,1]
  destfilename<-trimws(basename(readpath))
  download.file(url=readpath,
                destfile= paste("../DATA/CHELSA_v2/monthly/tas/",destfilename,sep=""),mode="wb")
}

#=================== download monthly DATA: observed max temperature DATA ======================================
# 1979-2019, 12 months in a year
tb<-read.delim("../DATA/CHELSA_v2/monthly/tasmax/envidatS3paths.txt",header = F)
for(i in 1:nrow(tb)){
  readpath<-tb[i,1]
  destfilename<-trimws(basename(readpath))
  download.file(url=readpath,
                destfile= paste("../DATA/CHELSA_v2/monthly/tasmax/",destfilename,sep=""),mode="wb")
}
#=================== download monthly DATA: observed min temperature DATA ======================================
# 1980-2019, 12 months in a year
tb<-read.delim("../DATA/CHELSA_v2/monthly/tasmin/envidatS3paths.txt",header = F)
for(i in 1:nrow(tb)){
  readpath<-tb[i,1]
  destfilename<-trimws(basename(readpath))
  download.file(url=readpath,
                destfile= paste("../DATA/CHELSA_v2/monthly/tasmin/",destfilename,sep=""),mode="wb")
}
#====================================================================================================


