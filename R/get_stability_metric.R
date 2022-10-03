# This function will give you different kind of stability metric measures for a community

# Args:
# m: community matrix, species time series along each column

# Output:
# a data frame with:    
#    1. "cvsq_real", # square of CV for the real community data
#    2. "cvsq_indep", # square of CV of the community if all the sp. behave independently
#    3. "phi",  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
#    4. "phi_LdM", # Loreau's variance ratio
#    5. "skw_real",  # skewness for the real community data
#    6. "skw_indep", # skewness of the community if all the sp. behave independently
#    7. "phi_skw"
#    8. "iCV" : inverse of CV: stability index
#    9. "iCValt" : inverse of varialibity for skewed distribution = median/IQR


get_stability_metric<-function(m){
  
  id<-which(colnames(m)%in%c("raresp"))
  if(length(id)>0){
    m<-m[,-id]
  }
  
  tot_quantity<-apply(m,MARGIN = 1,FUN = sum)
  var_each_sp<-apply(m,MARGIN = 2,FUN = var)
  mean_each_sp<-apply(m,MARGIN = 2,FUN = mean)
  cm3_each_sp<-apply(m,MARGIN = 2,FUN = my3cm)
  
  df_stability<-as.data.frame(matrix(NA,nrow=1,ncol=9))
  
  colnames(df_stability)<-c("cvsq_real", # square of CV for the real community data
                            "cvsq_indep", # square of CV of the community if all the sp. behave independently
                            "phi",  # this is the ratio of cvsq_real/cvsq_indep and compared to 1
                            "phi_LdM", # Loreau's variance ratio
                            "skw_real",  # skewness for the real community data
                            "skw_indep", # skewness of the community if all the sp. behave independently
                            "phi_skw", # this is the ratio of skw_real/skw_indep and compared to ??
                            "iCV", # inverse of cv = mean/std
                            "iCValt" # inverse of cv (non-biased) = median/IQR
  )
  
  # classic variance ratio
  df_stability$cvsq_real<-mycvsq(tot_quantity)
  df_stability$cvsq_indep<-sum(var_each_sp)/((sum(mean_each_sp))^2)
  df_stability$phi<-df_stability$cvsq_real/df_stability$cvsq_indep
  
  # Loreau's variance ratio 
  df_stability$phi_LdM<-var(tot_quantity)/((sum(sqrt(var_each_sp)))^2) 
  
  # skewness ratio
  df_stability$skw_real<-myskns(tot_quantity)
  df_stability$skw_indep<-sum(cm3_each_sp)/((sum(var_each_sp))^(3/2))
  df_stability$phi_skw<-df_stability$skw_real/df_stability$skw_indep
  
  # inverse of CV
  df_stability$iCV<-abs(mean(tot_quantity))/sd(tot_quantity)
  df_stability$iCValt<-abs(median(tot_quantity))/IQR(tot_quantity,type=7)
  
  return(df_stability)
  
}



###########################
# Function to calculate coefficient of variation^2
# Arg : x is a vector : here used for total timeseries 
mycvsq<-function(x){
  cvsq<-var(x)/(mean(x))^2
  return(cvsq)
}

#Functions for good estimators for 3rd central
#moment and skewness.

#Computes 3rd central moment of the data using
#the unbiased estimator. See
#http://mathworld.wolfram.com/SampleCentralMoment.html
#http://mathworld.wolfram.com/h-Statistic.html
#
#Args
#x      A numeric vector
#
#Output
#The estimated third central moment
my3cm<-function(x,na.rm=F){
  if (na.rm==T){
    x<-x[is.finite(x)]  
  }
  
  n<-length(x)
  m3<-(1/n)*sum((x-mean(x))^3)
  h3<-n^2*m3/((n-2)*(n-1))
  return(h3)
}

#Computes skewness of the data, making use
#of the function my3cm. See
#https://en.wikipedia.org/wiki/Skewness
#
#Args
#x      A numeric vector
#
#Output
#The estimated skewness
#
myskns<-function(x,na.rm=F){
  if (na.rm==T){
    x<-x[is.finite(x)]  
  }
  
  return(my3cm(x)/(sd(x)^3))
}









