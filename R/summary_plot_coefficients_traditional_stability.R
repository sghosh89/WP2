library(here)
library(tidyverse)
library(dplyr)
library(piecewiseSEM)
library(lme4)
resloc<-"Results/res_Prelim_Report/traditional_stability_res/Tscale_Celsius_Tvar_IQR_Textreme_skw/"
taxa<-"birds"
psem_birds<-readRDS(here(paste(resloc,"model_psem_",taxa,".RDS",sep="")))
coef_birds<-coefs(psem_birds$model_psem)

taxa<-"fish"
psem_fish<-readRDS(here(paste(resloc,"model_psem_",taxa,".RDS",sep="")))
coef_fish<-coefs(psem_fish$model_psem)

mydatb1<-coef_birds%>%dplyr::select(Response, Predictor, Std.Estimate,  P.Value)%>%
  filter(Response!="stability")%>%
  mutate(Effect="Indirect")
mydatb1_1<-mydatb1[c(1:3,8:9,16),] # among biotic drivers
mydatb1_2<-mydatb1[c(4:7,10:15,17:19),] # effect of abiotic drivers on biotic drivers

mydatb2<-coef_birds%>%dplyr::select(Response, Predictor, Std.Estimate,  P.Value)%>%
  filter(Response=="stability")%>%
  mutate(Effect="Direct")
mydatb<-rbind(mydatb2,mydatb1_1,mydatb1_2)%>%
  mutate(Taxa="Birds",seq=1:28)

mydatf1<-coef_fish%>%dplyr::select(Response, Predictor, Std.Estimate,  P.Value)%>%
  filter(Response!="stability")%>%
  mutate(Effect="Indirect")
mydatf1_1<-mydatf1[c(1:3,8:9,16),] # among biotic drivers
mydatf1_2<-mydatf1[c(4:7,10:15,17:19),] # effect of abiotic drivers on biotic drivers


mydatf2<-coef_fish%>%dplyr::select(Response, Predictor, Std.Estimate,  P.Value)%>%
  filter(Response=="stability")%>%
  mutate(Effect="Direct")
mydatf<-rbind(mydatf2,mydatf1_1,mydatf1_2)%>%
  mutate(Taxa="Fish",seq=1:28)

mydat<-rbind(mydatb,mydatf)

gps<-ggplot(mydat, aes(x=seq, y=Std.Estimate, fill=Taxa)) + 
  geom_bar(stat="identity", position="identity")+theme_bw()+
  ylab("Standardized estimate")+xlab("")+xlim(0.05,30)+
  theme(
    panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="top",text=element_text(size=15), axis.ticks.x = element_blank(),
    axis.text.x = element_blank())+
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 0.5))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 0.5))

pdf(here("Results/res_Prelim_Report/traditional_stability_res/Tscale_Celsius_Tvar_IQR_Textreme_skw/summary_plot_coefficients.pdf"), width = 6, height = 4)
print(gps)
dev.off()


