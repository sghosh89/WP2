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

# length of timeseries used
br<-c(0,20,30,40,50)
gts1<-sbf %>%
  ggplot( aes(nyr_used, stat(density))) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 breaks=br) +
  geom_text(aes(label = round(stat(count) / sum((count)), 3)), 
    stat = 'bin', vjust = -13, breaks = br)+ 
  #annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5,size=7)+
  ylim(c(0, 1))+
  xlab("")+ylab("")+ggtitle(label="Density plot across taxa")+
  #scale_y_continuous(labels = scales::percent)+
  theme_bw()+theme(plot.title = element_text(size = 12),
                   panel.background=element_rect(fill="white", colour="white"),
                   #axis.text = element_text(size = 15),
                   text = element_text(size = 15))

gts2<-ggplot(data=sbf, aes(x=nyr_used, fill=TAXA))+geom_histogram(bins=10)+
  facet_grid(~TAXA)+xlab("Time series length (in years)")+
  annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=1.5,size=7)+
  theme_classic()+
  theme(strip.background = element_blank(),
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 0.6))

#stability
gs<-ggplot(data = sbf, aes(y = iCV, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = iCValt, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Stability")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Richness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Evenness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Overall synchrony")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "e", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Tail-dependent synchrony")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "f", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Median Temperature (\u00B0C)")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "g", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Temperature variability")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "h", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Temperature Trend")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "i", vjust=1.5, hjust=1.5,size=7)+
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
  ylab("Temperature skewness")+xlab("")+ 
  annotate("text",  x=Inf, y = Inf, label = "j", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

pdf(here("Results/res_Prelim_Report/traditional_stability_res/rawdata_modelvariables.pdf"), width = 12, height = 9)
grid.arrange(gts2, gts1, gs, gR, gE, gVR, gTA, gMedianT, gVarT, gTrendT, gSkewT,ncol = 2, 
             layout_matrix= rbind(c(1,2,3,3),c(4,5,6,7),c(8,9,10,11)))
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
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Birds")
#print(gb_interact1)

gb_interact2<-sbf%>%filter(TAXA=="birds")%>%ggplot(aes(x=t_med_celsius,y=iCV, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=1.5,size=7)+
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
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Fish")
#print(gf_interact1)

gf_interact2<-sbf%>%filter(TAXA=="fish")%>%ggplot(aes(x=t_med_celsius,y=iCV, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5,size=7)+
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
  geom_abline(intercept=0, slope=1, linetype="dashed", col="pink")+ 
  coord_fixed()+annotate("text", x=19, y=3.2, label= "b", size=7)

gp1e<-ggplot(data=tab_rad_summary, aes(x=max_accumfreq_commonsp, group=TAXA, fill=TAXA)) +
  geom_density(adjust=1.5) +
  theme_bw() +
  facet_wrap(~TAXA) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  )

gp2<-ggplot(data = tab_rad_summary, 
            aes(y = max_accumfreq_commonsp, x = TAXA, fill = TAXA)) +
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
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))+
  annotate("text", x=2.4, y=62, label= "a", size=7)

resloc<-here("Results/res_Prelim_Report/traditional_stability_res/")
pdf((paste(resloc,"/plot_allsp_vs_dominantsp.pdf",sep="")),height=4,width=8)
gridExtra::grid.arrange(gp2, gp1, nrow=1)
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
                   panel.grid.minor = element_blank(),
                   text = element_text(size=12))+
  geom_abline(intercept=1, slope=-1, linetype="solid")+
  geom_abline(intercept=2/3, slope=-1, col="blue", linetype="dashed")+
  geom_abline(intercept=4/3, slope=-1, col="blue", linetype="dashed")+
  geom_abline(intercept=0.5, slope=-1, col="red", linetype="dotted")+
  geom_abline(intercept=1.5, slope=-1, col="red", linetype="dotted")+
  ggtitle("Copula plot")+
  xlab("Species i, normalized ranked abundance")+ 
  ylab("Species j, normalized ranked abundance")+ coord_fixed()

resloc<-here("Results/res_Prelim_Report/traditional_stability_res/")

pdf((paste(resloc,"/method_taildepsyn_plot.pdf",sep="")),height=5,width=5.2)
print(gp)
dev.off()

#=======================
tab_f<-readRDS(here("DATA/STI_related/fish_gbif_data/cleaned/tab_fish_country.RDS"))
tab_b<-readRDS(here("DATA/STI_related/birds_gbif_data/cleaned/tab_birds_country.RDS"))

tf<-tab_f%>%group_by(countryCode)%>%summarise(fishsp=n_distinct(Species))%>%ungroup()
tb<-tab_b%>%group_by(countryCode)%>%summarise(birdsp=n_distinct(Species))%>%ungroup()

tbf<-full_join(tb,tf,by="countryCode")
id<-which(is.na(tbf$fishsp))
tbf$fishsp[id]<-0

tbf%>%pivot_longer(cols = c(birdsp, fishsp)) %>% #get into long format
  rename(Taxa = name, #rename columns
         Number = value)-> dat_tbf

data<-dat_tbf#%>%filter(Taxa=="birdsp")
data$id<-c(1:nrow(data))
data$individual<-paste(data$countryCode,data$Number,sep=", ")

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #
gpr<-ggplot(data, aes(x=as.factor(id), y=Number, fill=Taxa)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity") +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 0.5))+
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-500,500) +
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    #panel.grid = element_blank(),
    panel.grid.major.x = element_line(color="gray95"),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, 
                                 y=500, label=individual, 
                                 hjust=hjust), color="black", 
            fontface="bold",alpha=0.5, size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE )+facet_wrap(~Taxa, ncol=1) 


resloc<-here("Results/res_Prelim_Report/traditional_stability_res/")
pdf((paste(resloc,"/gbif_summary_plot.pdf",sep="")),height=15,width=8)
print(gpr)
dev.off()


