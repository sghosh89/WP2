# This is a script that will pull source data and generate figures for the supplemental information
# You can choose size of PDF/ figure file to save these figures, 
# here I did not save any figure, rather you can visualize in R studio plot window

rm(list=ls())
library(here)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(piecewiseSEM)
library(maps)
library(PupillometryR)
source(here("R/plot_psem.R")) 

#================================ NOTE ======================
# For supplemental figures S1 to S5, we used the same source data that we used for Figure 3 in the main text

sbf<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig3.xls"), sheet= 1)
sbf<-sbf[-c(1:6),]
colnames(sbf)<-sbf[1,]
sbf<-sbf[-1,]

# make sure you are reading in correct format from excel file while importing in R
sbf$LATITUDE<-as.numeric(sbf$LATITUDE)
sbf$LONGITUDE<-as.numeric(sbf$LONGITUDE)
sbf$nyr_used<-as.integer(sbf$nyr_used)
sbf$stability<-as.numeric(sbf$stability)
sbf$richness<-as.numeric(sbf$richness)
sbf$evenness<-as.numeric(sbf$evenness)
sbf$overall_synchrony<-as.numeric(sbf$overall_synchrony)
sbf$taildep_synchrony<-as.numeric(sbf$taildep_synchrony)
sbf$MedianT<-as.numeric(sbf$MedianT)
sbf$TrendT<-as.numeric(sbf$TrendT)
sbf$SkewT<-as.numeric(sbf$SkewT)
sbf$VarT<-as.numeric(sbf$VarT)
sbf$is.significant.TrendT<-as.numeric(sbf$is.significant.TrendT)
sbf$is.stationary.adf<-as.numeric(sbf$is.stationary.adf)

#=================== code for Figure S1 ================================

sbirds<-sbf%>%filter(TAXA=="birds")
sfish<-sbf%>%filter(TAXA=="fish")

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
  ggtitle(paste("n =", nrow(sbirds)," sampling sites, birds",sep=""))+
  theme(plot.title = element_text(size = 10))

gmap_fish<-gmap_base+geom_point(data=sfish,aes(y=LATITUDE,x=LONGITUDE,
                                               col="dodgerblue"),size=1)+
  scale_color_manual(values=alpha("dodgerblue", 0.5))+
  #theme(legend.position = "bottom",legend.title = element_blank())+
  ggtitle(paste("n =", nrow(sfish)," sampling sites, fish",sep=""))+
  theme(plot.title = element_text(size = 10))

grid.arrange(gmap_birds,gmap_fish,ncol=2) # this is Fig. S1

#=================== code for Figure S2 ================================

# length of timeseries used
br<-c(0,20,30,40,50)
gts1<-sbf %>%
  ggplot( aes(nyr_used, stat(density))) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 breaks=br) +
  geom_text(aes(label = round(stat(count) / sum((count)), 3)), 
            stat = 'bin', vjust = -18, breaks = br)+ 
  annotate("text",  x=Inf, y = Inf, label = "a_inset", vjust=1.5, hjust=1.5,size=7)+
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
gs<-ggplot(data = sbf, aes(y = stability, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = stability, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
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
gR<-ggplot(data = sbf, aes(y = richness, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = richness, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Richness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Evenness
gE<-ggplot(data = sbf, aes(y = evenness, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = evenness, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Evenness")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Overall synchrony
gVR<-ggplot(data = sbf, aes(y = overall_synchrony, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = overall_synchrony, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Overall synchrony")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "e", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# Tail-dep synchrony
gTA<-ggplot(data = sbf, aes(y = taildep_synchrony, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = taildep_synchrony, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
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
gMedianT<-ggplot(data = sbf, aes(y = MedianT, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = MedianT, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Median Temperature (\u00B0C)")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "g", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# temp variability
gVarT<-ggplot(data = sbf, aes(y = VarT, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = VarT, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature variability")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "h", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

# temp trend
gTrendT<-ggplot(data = sbf, aes(y = TrendT, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = TrendT, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature Trend")+xlab("")+ annotate("text",  x=Inf, y = Inf, label = "i", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))


# temp skewness
gSkewT<-ggplot(data = sbf, aes(y = SkewT, x = TAXA, fill = TAXA)) +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 1))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4) + 
  #coord_flip()+
  geom_point(aes(y = SkewT, color = TAXA), position = position_jitter(width = .15), size = 0.9, alpha = 0.2) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.4)+ 
  ylab("Temperature skewness")+xlab("")+ 
  annotate("text",  x=Inf, y = Inf, label = "j", vjust=1.5, hjust=1.5,size=7)+
  theme_bw()+
  theme(
    #panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="none",text=element_text(size=15))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 1))

grid.arrange(gts2, gts1, gs, gR, gE, gVR, gTA, gMedianT, gVarT, gTrendT, gSkewT,ncol = 2, 
             layout_matrix= rbind(c(1,2,3,3),c(4,5,6,7),c(8,9,10,11))) # this is Figure S2


#=================== code for Figure S3 ================================

fishdat<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS3")
fishdat$P.Value<-as.numeric(fishdat$P.Value)
fishdat<-as.data.frame(fishdat)
plot_psem(n=356,taxa="fish",cc=fishdat,layout="circle") # this is Fig. S3

#=================== code for Figure S4 ================================

sbirds<-sbf%>%filter(TAXA=="birds")
sfish<-sbf%>%filter(TAXA=="fish")


q_T_birds <- sbirds %>%
  summarise(q=quantile(MedianT,c(0.5))) %>%
  pull(q)

q_T_fish <- sfish %>%
  summarise(q=quantile(MedianT,c(0.5))) %>%
  pull(q)

sbf <- sbf %>%
  mutate(MedianT_level = case_when(TAXA == "birds" & MedianT < q_T_birds[1] ~ "low T, <50%CI",
                                   TAXA == "birds" & MedianT >= q_T_birds[1] ~ "high T, >50%CI",
                                   TAXA == "fish" & MedianT < q_T_fish[1] ~ "low T, <50%CI",
                                   TAXA == "fish" & MedianT >= q_T_fish[1] ~ "high T, >50%CI")) %>%
  mutate(MedianT_level = fct_relevel(MedianT_level, "low T, <50%CI", "high T, >50%CI"))


q_nsp_birds <- sbirds%>%
  summarise(q=quantile(richness,c(0.5))) %>%
  pull(q)
q_nsp_fish <- sfish%>%
  summarise(q=quantile(richness,c(0.5))) %>% # note the change, it is now categorized around median
  pull(q)

sbf <- sbf %>%
  mutate(Richness_level = case_when(TAXA == "birds" & richness < q_nsp_birds[1] ~ "low richness, <50%CI",
                                    TAXA == "birds" & richness >= q_nsp_birds[1] ~ "high richness, >50%CI",
                                    TAXA == "fish" & richness < q_nsp_fish[1] ~ "low richness, <50%CI",
                                    TAXA == "fish" & richness >= q_nsp_fish[1] ~ "high richness, >50%CI")) %>%
  mutate(Richness_level = fct_relevel(Richness_level,
                                      "low richness, <50%CI",
                                      "high richness, >50%CI"))

gb_interact1<-sbf%>%filter(TAXA=="birds")%>%ggplot(aes(x=richness,y=stability, col = MedianT_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  ylab("Stability, log2 scale") +
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Birds")

gb_interact2<-sbf%>%filter(TAXA=="birds")%>%ggplot(aes(x=MedianT,y=stability, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=1.5,size=7)+
  ylab("Stability") +
  xlab("Temperature, MedianT (\u00B0C)") +
  scale_color_manual(values=c("orange","magenta"))+
  scale_fill_manual(values=c("orange","magenta"))+
  theme_bw()+ggtitle("Birds")

gf_interact1<-sbf%>%filter(TAXA=="fish")%>%ggplot(aes(x=richness,y=stability, col = MedianT_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) +
  scale_x_continuous(trans = 'log2') +
  scale_y_continuous(trans = 'log2') +
  ylab("Stability, log2 scale") +
  xlab("Richness, log2 scale") + annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5,size=7)+
  scale_color_manual(values=c("blue", "red"))+
  scale_fill_manual(values=c("blue", "red"))+
  theme_bw()+ggtitle("Fish")

gf_interact2<-sbf%>%filter(TAXA=="fish")%>%ggplot(aes(x=MedianT,y=stability, col = Richness_level)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=T) + annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5,size=7)+
  ylab("Stability") +
  xlab("Temperature, MedianT (\u00B0C)") +
  scale_color_manual(values=c("orange","magenta"))+
  scale_fill_manual(values=c("orange","magenta"))+
  theme_bw()+ggtitle("Fish")

grid.arrange(gb_interact1, gb_interact2,gf_interact1, gf_interact2, nrow=2) # this is Fig. S4


#=================== code for Figure S5 ================================

figS5_dat<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS5")
figS5_dat<-as.data.frame(figS5_dat)

figS5_dat$P.Value<-as.numeric(figS5_dat$P.Value)
figS5_dat$Std.Estimate<-as.numeric(figS5_dat$Std.Estimate)
figS5_dat$Estimate<-as.numeric(figS5_dat$Estimate)
figS5_dat$Std.Error<-as.numeric(figS5_dat$Std.Error)
figS5_dat$DF<-as.numeric(figS5_dat$DF)
figS5_dat$Crit.Value<-as.numeric(figS5_dat$Crit.Value)

figS5a_dat<-figS5_dat%>%filter(data_for_fig.panel=="S5a")
figS5b_dat<-figS5_dat%>%filter(data_for_fig.panel=="S5b")
figS5c_dat<-figS5_dat%>%filter(data_for_fig.panel=="S5c")
figS5d_dat<-figS5_dat%>%filter(data_for_fig.panel=="S5d")

plot_psem(n=938,taxa="birds",cc=figS5a_dat,layout="circle") # this is Fig. S5a
plot_psem(n=386,taxa="fish",cc=figS5b_dat,layout="circle") # this is Fig. S5b
plot_psem(n=530,taxa="birds",cc=figS5c_dat,layout="circle") # this is Fig. S5c
plot_psem(n=153,taxa="fish",cc=figS5d_dat,layout="circle") # this is Fig. S5d

#=================== code for Figure S6 ================================

mydat<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS6")

gp1<-ggplot(mydat,aes(x=stability_allsp,y=stability_commonsp))+geom_point(alpha=0.2)+
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

gp2<-ggplot(data = mydat, 
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

gridExtra::grid.arrange(gp2, gp1, nrow=1) # this is Fig. S6

#=================== code for Figure S7 ================================
xx<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS7")
xx<-as.data.frame(xx)
gp<-ggplot(data=xx, aes(x=sp_i_normalized_rank_abundance,y=sp_j_normalized_rank_abundance))+geom_point(alpha=0.5)+
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

gp # this is Fig. S7

#=================== code for Figure S8 ================================

xx<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS8")
xx<-as.data.frame(xx)
xx$A_nbin4<-as.numeric(xx$A_nbin4)

sbf30<-xx

g1<-ggplot(sbf30,aes(x=A_nbin2,y=A_nbin3))+geom_point(alpha=0.3,col="blue")+
  geom_smooth(method="lm",se=F)+theme_bw()+
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 4.6,col="blue")+
  stat_regline_equation(label.x = 0.1, label.y = 5,col="blue")+
  xlab("Community-level tail-dependent synchrony, A \n (LT for <50% and UT for >50% of joint distribution)")+
  ylab("Community-level tail-dependent synchrony, A \n (LT for <33.3% and UT for >66.7% of joint distribution)")+
  ggtitle(label="100 communities with minimum 30 years of data")

sbf40<-sbf30%>%filter(nyr_used>=40)

g2<-ggplot(sbf40,aes(x=A_nbin2,y=A_nbin3))+geom_point(col="blue",alpha=0.3)+
  geom_smooth(method="lm",se=F)+theme_bw()+
  stat_cor(aes(label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 1.9,col="blue")+
  stat_regline_equation(label.x = 0.1, label.y = 2,col="blue")+
  geom_point(col="red",alpha=0.3, data=sbf40, aes(x=A_nbin2,y=A_nbin4))+
  geom_smooth(method="lm",se=F,col="red",data=sbf40, aes(x=A_nbin2,y=A_nbin4))+
  stat_cor(data=sbf40, aes(x=A_nbin2,y=A_nbin4,label = paste(..r.label..,..rr.label.., ..p.label.., sep = "*`,`~")),
           label.x = 0.1, label.y = 1.6,col="red")+
  stat_regline_equation(data=sbf40, aes(x=A_nbin2,y=A_nbin4),label.x = 0.1, label.y = 1.7,col="red")+
  xlab("Community-level tail-dependent synchrony, A \n (LT for <50% and UT for >50% of joint distribution)")+
  ylab("Community-level tail-dependent synchrony, A \n (LT for <33.3% and UT for >66.7% of joint distribution), \n (LT for <25% and UT for >75% of joint distribution)")+
  ggtitle(label="24 communities with minimum 40 years of data")

grid.arrange(g1,g2,nrow=1) # this is Fig. S8

#=================== code for Figure S9 ================================

data<-read_excel(here("source_data_suppmat_figures/Ghosh_SourceData_Figures_S1toS9.xls"), sheet= "Data_for_FigS9")
data<-as.data.frame(data)
data$id<-c(1:nrow(data))
data$individual<-paste(data$countryCode,data$How_many_species_observed,sep=", ")

label_data <- data

number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     

label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

gpr<-ggplot(data, aes(x=as.factor(id), y=How_many_species_observed, fill=TAXA)) +   
    geom_bar(stat="identity") +
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 0.5))+
  ylim(-500,500) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major.x = element_line(color="gray95"),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")     
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, 
                                 y=500, label=individual, 
                                 hjust=hjust), color="black", 
            fontface="bold",alpha=0.5, size=2.5, 
            angle= label_data$angle, inherit.aes = FALSE )+facet_wrap(~TAXA, ncol=1) 

gpr # this is Fig. S9
