set.seed(101)
d=rnorm(25,mean=0,sd=0.4)
z<-matrix(d,5,5)
z[upper.tri(z,diag=T)]<-NA
tempo<-z

indI<-which(z< -0.1,arr.ind = T)
tempo[indI]<-NA
diag(tempo)<-NA

minval<-min(tempo,na.rm=T)
maxval<-max(tempo,na.rm=T)

cr<-max(abs(minval),abs(maxval))
colrange=c(-cr,cr)
z=tempo
posnI_ind=indI
posnN_ind=which(tempo< 0,arr.ind = T)
