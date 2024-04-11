# Input:
# taxa = a character: "birds" or "fish" 
# data 
# rescaling = T (if you want mean centering before psem) or F
# resloc = folder path to save results
# Textreme = a character: "skw" or "kurt"
# Tvar = a character: "cv" or "IQR"
# Tscale = a character: "Kelvin" or "Celsius"

call_model_psem_stationary<-function(taxa,data=sm_all,
                          rescaling=T,
                          mean_centering=T,resloc){
  
  sm_all_eachtaxa<-data%>%filter(TAXA==taxa)
  mydat<-sm_all_eachtaxa
  
  #------ want rescaling? 
  if(rescaling==T){
    
    mydat_scaled<-mydat
    mydat_scaled$stability<-as.numeric(scale(mydat_scaled$stability,
                                             center=mean_centering,scale=T))
    mydat_scaled$R<-as.numeric(scale(mydat_scaled$R,
                                     center=mean_centering,scale=T))
    mydat_scaled$E<-as.numeric(scale(mydat_scaled$E,
                                     center=mean_centering,scale=T))
    mydat_scaled$VR<- as.numeric(scale(mydat_scaled$VR,
                                       center=mean_centering,scale=T))
    mydat_scaled$A<- as.numeric(scale(mydat_scaled$A,
                                      center=mean_centering,scale=T))
    
    # celsius scale
    mydat_scaled$t_med_celsius<-as.numeric(scale(mydat_scaled$t_med_celsius,
                                                 center=mean_centering,scale=T))
    mydat_scaled$t_skw_celsius<-as.numeric(scale(mydat_scaled$t_skw_celsius,
                                                 center=mean_centering,scale=T))
    mydat_scaled$t_var_celsius<-as.numeric(scale(mydat_scaled$t_var_celsius,
                                                 center=mean_centering,scale=T))
    mydat_scaled$t.sens.slope.celsius<-as.numeric(scale(mydat_scaled$t.sens.slope.celsius,
                                                        center=mean_centering,scale=T))
    
    mydat_scaled$t_kurt_celsius<-as.numeric(scale(mydat_scaled$t_kurt_celsius,
                                                  center=mean_centering,scale=T))
    mydat_scaled$t_varIQR_celsius<-as.numeric(scale(mydat_scaled$t_varIQR_celsius,
                                                    center=mean_centering,scale=T))
  }
  dat<-mydat_scaled
  n<-nrow(dat)
  
  
  # 
 
    dat<-rename(dat, MedianT = t_med_celsius, 
                SkewT = t_skw_celsius,
                VarT = t_varIQR_celsius)
    
    pf_collinearity<-performance::check_collinearity(
      lme4::lmer(stability ~ R+E+VR+A+
                   t_med_celsius+
                   t_skw_celsius+t_varIQR_celsius+(1|UID), 
                 data=mydat))# with original data
    print(pf_collinearity)
    
    model_psem<-piecewiseSEM::psem(
      lmer(A ~ R+E+VR+MedianT+VarT+SkewT+(1|UID),data=dat),
      #lmer(VR ~ R+E+MedianT+VarT+SkewT+(1|UID),data=dat),
      lmer(VR ~ R+E+MedianT+VarT+(1|UID),data=dat),
      lmer(R~MedianT+VarT+(1|UID),data=dat),
      lmer(E~R+MedianT+VarT+(1|UID),data=dat),
      lmer(stability ~ R+E+VR+A+(R:MedianT)+
             MedianT+VarT+SkewT+(1|UID), data=dat))
 
  
  #=============================================================
  print(summary(model_psem))
  mcoef<-coefs(model_psem,standardize = "scale")
  
  plot_psem(n,taxa,mcoef,layout="circle")
  res<-list(model_psem=model_psem,
            pf_collinearity=pf_collinearity)
  return(res)
  #saveRDS(model_psem,here(paste(resloc,"model_psem_",taxa,".RDS",sep="")))
  #saveRDS(pf_collinearity,here(paste(resloc,"pf_collinearity_",taxa,".RDS",sep="")))
}