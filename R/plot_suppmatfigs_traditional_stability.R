library(here)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(PupillometryR)
library(ggpubr)

#--------- first map communities for birds and fish on the global map --------------
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sbf<-sm_all%>%filter(TAXA%in%c("birds","fish"))
sbirds<-sm_all%>%filter(TAXA=="birds")
sfish<-sm_all%>%filter(TAXA=="fish")

library(maps)
wd<-map_data("world")
gmap_base<-ggplot()+coord_fixed()+xlab("Longitude")+ylab("Latitude")+
  xlim(-180,180)+
  ylim(-80,80)

gmap_base<-gmap_base+geom_polygon(data=wd, aes(x=long, y=lat, group=group), colour="gray90", fill="gray90")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -67, ymax = -23,
           alpha = .2,fill = "yellow")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 23, ymax = 67,
           alpha = .2,fill = "yellow")

gmap_base<-gmap_base+theme(panel.grid.major=element_line(colour="gray90"), 
                           panel.grid.minor=element_blank(),legend.position="none",
                           panel.background=element_rect(fill="white",colour="gray90"))

gmap_birds<-gmap_base+geom_point(data=sbirds,aes(y=LATITUDE,x=LONGITUDE,
                                                 col="green3"),size=1)+
  scale_color_manual(values=alpha("green3", 0.5))+
  #theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("n =", nrow(sbirds)," sampling sites",sep=""))+
  theme(plot.title = element_text(size = 10))
gmap_birds

gmap_fish<-gmap_base+geom_point(data=sfish,aes(y=LATITUDE,x=LONGITUDE,
                                               col="dodgerblue"),size=1)+
  scale_color_manual(values=alpha("dodgerblue", 0.5))+
  #theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("n =", nrow(sfish)," sampling sites",sep=""))+
  theme(plot.title = element_text(size = 10))
gmap_fish

pdf(here("Results/res_Prelim_Report/traditional_stability_res/maps_samplingsite.pdf"), width = 5, height = 5)
grid.arrange(gmap_birds,gmap_fish,ncol=2)
dev.off()

#----------------------- draw raincloud plot for raw data --------------------
library(PupillometryR)

#stability
gs<-ggplot(data = sbf, aes(y = iCV, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = iCValt, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Stability")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "A", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# for biotic drivers
#richness
gR<-ggplot(data = sbf, aes(y = nsp, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = nsp, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Richness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Evenness
gE<-ggplot(data = sbf, aes(y = SmithWilson, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = SmithWilson, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Evenness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "C", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Overall synchrony
gVR<-ggplot(data = sbf, aes(y = phi_LdM, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = phi_LdM, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Overall synchrony")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Tail-dep synchrony
sbf$TA<-sbf$L+abs(sbf$U)
gTA<-ggplot(data = sbf, aes(y = TA, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = TA, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Tail-dependent synchrony")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "E", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# for abiotic drivers
# median temp
gMedianT<-ggplot(data = sbf, aes(y = t_med_celsius, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = t_med_celsius, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Median Temperature (\u00B0C)")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "F", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# temp variability
gVarT<-ggplot(data = sbf, aes(y = t_varIQR_celsius, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = t_varIQR_celsius, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature variability")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "G", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# temp trend
gTrendT<-ggplot(data = sbf, aes(y = t.sens.slope.celsius, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = t.sens.slope.celsius, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature Trend")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "H", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))


# temp skewness
gSkewT<-ggplot(data = sbf, aes(y = t_skw_celsius, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = t_skw_celsius, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature skewness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "I", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

pdf(here("Results/res_Prelim_Report/traditional_stability_res/rawdata_modelvariables.pdf"), width = 12, height = 9)
grid.arrange(gs, gR, gE, gVR, gTA, gMedianT, gVarT, gTrendT, gSkewT,ncol = 2, 
             layout_matrix= rbind(c(NA,1,1,NA),c(2,3,4,5),c(6,7,8,9)))
dev.off()


#------------- plot interactive effects between richness & temp ---------------

## Add the categotical temperature variable for birds and fish
## Note that the cutoffs are calculated separate for birds and for fish
## Also do the same for number of species

#========== temperature category ========
q_T_birds <- sbirds %>%
  summarise(q=quantile(t_med_celsius,c(0.5))) %>%
  pull(q)

q_T_fish <- sfish %>%
  summarise(q=quantile(t_med_celsius,c(0.5))) %>%
  pull(q)

sbf <- sbf %>%
  mutate(MedianT_level = case_when(TAXA == "birds" & t_med_celsius < q_T_birds[1] ~ "low T, <50%CI",
                                   TAXA == "birds" & t_med_celsius >= q_T_birds[1] ~ "high T, >50%CI",
                                   TAXA == "fish" & t_med_celsius < q_T_fish[1] ~ "low T, <50%CI",
                                   TAXA == "fish" & t_med_celsius >= q_T_fish[1] ~ "high T, >50%CI")) %>%
  mutate(MedianT_level = fct_relevel(MedianT_level, "low T, <50%CI", "high T, >50%CI"))


#========== richness category ========
q_nsp_birds <- sbirds%>%
  summarise(q=quantile(nsp,c(0.5))) %>%
  pull(q)
q_nsp_fish <- sfish%>%
  summarise(q=quantile(nsp,c(0.5))) %>% # note the change, it is now categorized around median
  pull(q)

sbf <- sbf %>%
  mutate(Richness_level = case_when(TAXA == "birds" & nsp < q_nsp_birds[1] ~ "low richness, <50%CI",
                                    TAXA == "birds" & nsp >= q_nsp_birds[1] ~ "high richness, >50%CI",
                                    TAXA == "fish" & nsp < q_nsp_fish[1] ~ "low richness, <50%CI",
                                    TAXA == "fish" & nsp >= q_nsp_fish[1] ~ "high richness, >50%CI")) %>%
  mutate(Richness_level = fct_relevel(Richness_level,
                                      "low richness, <50%CI",
                                      "high richness, >50%CI"))

#------ plot how stability-diversity relationship changes with temperature -------

gb_interact1<-sbf%>%filter(TAXA=="birds")%>%ggplot(aes(x=nsp,y=iCV, col = MedianT_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  ylab("Stability, log2 scale") +
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "A", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Birds")
#print(gb_interact1)

gb_interact2<-sbf%>%filter(TAXA=="birds")%>%ggplot(aes(x=t_med_celsius,y=iCV, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5,size=7)+
  ylab("Stability") +
  xlab("Temperature, MedianT (\u00B0C)") +
  scale_color_manual(values=c("orange","magenta"))+
  scale_fill_manual(values=c("orange","magenta"))+
  theme_bw()+ggtitle("Birds")
#print(gb_interact2)

gf_interact1<-sbf%>%filter(TAXA=="fish")%>%ggplot(aes(x=nsp,y=iCV, col = MedianT_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  ylab("Stability, log2 scale") +
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "C", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Fish")
#print(gf_interact1)

gf_interact2<-sbf%>%filter(TAXA=="fish")%>%ggplot(aes(x=t_med_celsius,y=iCV, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5,size=7)+
  ylab("Stability") +
  xlab("Temperature, MedianT (\u00B0C)") +
  scale_color_manual(values=c("orange","magenta"))+
  scale_fill_manual(values=c("orange","magenta"))+
  theme_bw()+ggtitle("Fish")
#print(gf_interact2)

pdf(here("Results/res_Prelim_Report/traditional_stability_res/stability_diversity_temperature_plot.pdf"), width = 12, height = 8)
grid.arrange(gb_interact1, gb_interact2,gf_interact1, gf_interact2, nrow=2)
dev.off()

#-------------- plot sensitivity test for tail threshold ----------------

source(here("R/plot_sensitivity_tails.R"))

#-------------- plot all sp vs dominant sp comparison ----------------
library(ggpubr)
tab_rad_summary<-readRDS(here("Results/tab_rad_summary.RDS"))

gp1<-ggplot(tab_rad_summary,aes(x=iCV_allsp,y=iCV))+geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F, fill="blue") +
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.3, label.y = 20,col="blue")+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 21)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 21))+
  xlab("Community stability considering all species")+
  ylab("Community stability considering common species")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())+
  geom_abline(intercept=0, slope=1, linetype="dashed", col="green")+ coord_fixed()

gp1e<-ggplot(data=tab_rad_summary, aes(x=max_accumfreq_commonsp, group=TAXA, fill=TAXA)) +
  geom_density(adjust=1.5) +
  theme_bw() +
  facet_wrap(~TAXA) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

gp2<-ggplot(data = tab_rad_summary, aes(y = max_accumfreq_commonsp, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = max_accumfreq_commonsp, color = TAXA), 
             position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Max. accumulated frequency for common species \n(i.e., contribution by the common species \n to the total community-abundance)")+
  xlab("TAXA")+ 
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=11))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

resloc<-here("Results/res_Prelim_Report/traditional_stability_res/")
pdf((paste(resloc,"/plot_allsp_vs_dominantsp.pdf",sep="")),height=4,width=8)
gridExtra::grid.arrange(gp1, gp2, nrow=1)
dev.off()

#------------------ plot method fig: tail-dep synchrony -----------------------------

library(here)
library(tidyverse)
library(VineCopula)
set.seed(seed=101)
x<-BiCopSim(N=40, family=3, par=5)
xx<-as.data.frame(x)
gp<-ggplot(data=xx, aes(x=V1,y=V2))+geom_point(alpha=0.5)+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  #geom_abline(intercept=0, slope=1, col="gray80")+
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())+
  geom_abline(intercept=1, slope=-1, linetype="solid")+
  geom_abline(intercept=2/3, slope=-1, col="blue", linetype="dashed")+
  geom_abline(intercept=4/3, slope=-1, col="blue", linetype="dashed")+
  geom_abline(intercept=0.5, slope=-1, col="red", linetype="dotted")+
  geom_abline(intercept=1.5, slope=-1, col="red", linetype="dotted")+
  ggtitle("Copula plot")+
  xlab("Species i, normalized ranked abundance")+ 
  ylab("Species j, normalized ranked abundance")+ coord_fixed()

resloc<-here("Results/res_Prelim_Report/traditional_stability_res/")

pdf((paste(resloc,"/method_taildepsyn_plot.pdf",sep="")),height=5,width=5)
gp
dev.off()




