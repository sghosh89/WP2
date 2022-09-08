# This script tests parametric lm fit slope and non-parametric Sen's slope
# create random timeseries data with some trends
rm(list=ls())
library(here)
library(trend)
set.seed(123)
# create Y_i=a+b*x_i+eps
npts<-20 # number of data points in each timeseries
nts<-5 # number of timeseries you want to genarate
a<-matrix(rep(sample.int(n=30,size=5),npts),nrow=npts,ncol=nts,byrow=T)

mu<-(1:nts) # specified mean
sigma<-0.1*(1:nts) # specified sd
b<-mapply(function(x,y){rnorm(x,y,n=npts)},x=mu,y=sigma)
hist(b[,5])# normal distribution along each column

x<-1:npts

eps<-matrix(runif(npts*nts,-1,1),nrow=npts,ncol=nts)

hist(eps[,nts]) #white noise

y_pos_slope<- a+ (b*x) +eps # this is the timeseries values along each column you generated

#plot(x,y[,5],type="l")
matplot(x, y_pos_slope, type='l', xlab='Years', ylab='t_med', 
        col=rainbow(ncol(y_pos_slope)),lty=1)

# ok, now genarate 5 timeseries with negative slope
a_neg<-matrix(rep(sample.int(n=30,size=5),npts),nrow=npts,ncol=nts,byrow=T)
y_neg_slope<- a_neg - (b*x) +eps # this is the timeseries values along each column you generated
matplot(x, y_neg_slope, type='l', xlab='Years', ylab='t_med', 
        col=rainbow(ncol(y_neg_slope)),lty=1)

# ok, now genarate 5 timeseries with no slope
intercept<-sample.int(n=30,size=5)
eps0<-matrix(NA,nrow=npts,ncol=nts)
for(i in 1:ncol(eps0)){
  eps0[,i]<-intercept[i]+runif(npts,min=0,max=mu[i])
}

y_no_slope<-eps0 # this is the timeseries values along each column you generated
matplot(x, y_no_slope, type='l', xlab='Years', ylab='t_med', 
        col=rainbow(ncol(y_no_slope)),lty=1)

#------------------------------------------------------------
mat<-cbind(y_pos_slope,y_no_slope,y_neg_slope)
mat<-as.data.frame(mat)
trendmat<-matrix(NA,nrow=ncol(mat),ncol=7)
colnames(trendmat)<-c("lm.slope","lm.slope.sig","mk.trend.z","mk.trend.tau","mk.trend.sig","sens.slope","sens.slope.sig.95%CI")
trendmat<-as.data.frame(trendmat)# each column is related to each timeseries

x<-1:npts
for(i in 1:ncol(mat)){
  # first do linear regression (parametric)
  model <- lm(mat[,i] ~ x, data = mat)
  trendmat$lm.slope[i]<-unname(model$coefficients[2])
  
  tempo<-summary(model)
  tempo<-tempo$coefficients[,4]
  tempo<-unname(tempo[2])
  trendmat$lm.slope.sig[i]<-ifelse(tempo<0.05,1,0)
    
  # now do the mann-kendall test
  tempo<-mk.test(mat[,i])
  trendmat$mk.trend.z[i]<-unname(tempo$statistic["z"])
  trendmat$mk.trend.tau[i]<-unname(tempo$estimates["tau"]) # tau of Mann-Kendall trend test, for shorter time series it is difficult to see a trend
  trendmat$mk.trend.sig[i]<-ifelse(tempo$p.value<0.05,1,0) # 1 means significant trend
  
  tempo_sens<-sens.slope(mat[,i], conf.level = 0.95)
  trendmat$sens.slope[i]<-unname(tempo_sens$estimates["Sen's slope"])
  trendmat$`sens.slope.sig.95%CI`[i]<-ifelse(tempo_sens$p.value<0.05,1,0) # 1 means significant trend
}

# ok, so after observing the parametric lm.slope and non-parametric sens.slope
# I think we need to use sen's slope rather than the tau of mk.test
# this is because the tau cannot detect the strength of the trend it just says either positive or negative

# also "For linear trend, the slope is usually estimated by computing the 
# least squares estimate using linear regression. However it is only valid 
# when there is no serial correlation, and the method is very sensitive to 
# outliers. A more robust method was developed by Sen (1968), 
# see this: https://www.researchgate.net/post/Sens_slope_vs_least_squares_regression"
plot(trendmat$lm.slope,trendmat$sens.slope)

# Now you can plot them from real data
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
plot(sm_all$t.lm.slope,sm_all$t.sens.slope,col=rgb(0,0,0,0.2),pch=19)
abline(h=0,col="red")
abline(v=0,col="red")

#################################################################




