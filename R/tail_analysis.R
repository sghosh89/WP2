# This script will do the copula analysis, will generate necessary plots
# for a given community and also with a given environmental variable

source("./NonParamStat.R")
source("./NonParamStat_matrixplot.R")

# Input:
# mat: a matrix or dataframe where each target species time series along each column, 
#         and also env timeseries as columns, rows have name for sampling years
# tot_target_sp: number of living species in the "mat" matrix (i.e., without env variable)
#resloc: path to save results
#nbin: 2 (default) to measure tail-dep.

# Output:
# a list and several plots to be saved in resloc path

tail_analysis<-function(mat, tot_target_sp, resloc, nbin=2){
  
  id<-which(colnames(mat)=="raresp")
  if(length(id)>0){
    mat<-mat[,-id]
  }
  
  splist<-vector(mode="list",length=ncol(mat))
  names(splist)<-colnames(mat)
  for(i in 1:length(splist)){
    splist[[i]]<-data.frame(Year=rownames(mat),Dat=mat[,i])
  }
  
  z<-multcall(d_allsp = splist, resloc = resloc, nbin=nbin)
  
  saveRDS(z,paste(resloc,"NonParamStat.RDS",sep=""))
  
  NonParamStat_matrixplot(data=z,
                          resloc=resloc,
                          tot_target_sp=tot_target_sp,
                          tl.cex=1.2,cl.cex=2,line=1)
}