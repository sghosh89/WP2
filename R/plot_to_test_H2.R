library(here)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(dplyr)

#--------- first for birds ---------
birds_meta<-read.csv(here("DATA/STI_related/birds_gbif_data/cleaned/birds_occurrence_metadata_with_tolerance.csv"))
birds_meta<-birds_meta%>%dplyr::select(Species,
                                       maxTtol=maxTtol_celcius,
                                       minTtol=minTtol_celcius)

# now extract community wide value of tolerance limit variability
df<-read.csv(here("Results/birds_splist_with_temp_sensitivity.csv"))
df<-df%>%dplyr::select(STUDY_ID,newsite,spname)

dfc<-read.csv(here("DATA/traitsdata/bird_traits_from_AVONET.csv"))
dfc<-dfc%>%dplyr::select(spname,possible_sp)
# first extract temperature-cor traits per community

dfc<-dfc%>%distinct(spname,.keep_all = T)

df2<-left_join(df,dfc,by=c("spname"))
df3<-left_join(df2,birds_meta,by=c("possible_sp"="Species"))

df4<-df3%>%group_by(STUDY_ID,newsite)%>%
  summarise(avg_maxTtol=mean(maxTtol,na.rm=T),
            avg_minTtol=mean(minTtol,na.rm=T),
            maxTtol_var=sd(maxTtol,na.rm=T)/abs(mean(maxTtol,na.rm=T)),
            minTtol_var=sd(minTtol,na.rm=T)/abs(mean(minTtol,na.rm=T)))%>%ungroup()
#which(is.na(df4$avg_maxTtol))
taxa<-"birds"
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sfi<-sm_all%>%filter(TAXA==taxa)

mydat<-left_join(sfi,df4,by=c("STUDY_ID","newsite"))
# response traits (variability) vs. t_med plot

mydat$A<-mydat$L+abs(mydat$U)
#range(mydat$A)

mydat$gr<-ifelse(mydat$t_skw>0,1,0)
mydat$gr<-as.factor(mydat$gr)

g3b<-ggplot(mydat,aes(x=t_skw,y=minTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.8)+
  scale_color_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g3b

g4b<-ggplot(mydat,aes(x=t_skw,y=maxTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.8)+
  scale_color_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g4b

g5b<-ggplot(mydat,aes(y=A,x=t_skw, col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.95)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  scale_color_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Tail-dependent \n synchrony, A")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g5b


g6bl<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T,color="green",fill="greenyellow")+
  stat_cor(aes(label=after_stat(p.label)), label.x = 0.8, label.y = 35)+
  stat_regline_equation(label.x = 0.7, label.y = 55)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g6bl

g6b<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="green",fill="greenyellow")+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))


g7bl<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T,color="green",fill="greenyellow")+
  stat_cor(aes(label=after_stat(p.label)), label.x = 0.09, label.y = 35)+
  stat_regline_equation(label.x = 0.09, label.y = 55)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g7bl

g7b<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="green",fill="greenyellow")+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

######################
#------- now for fish -------
fish_meta<-read.csv(here("DATA/STI_related/fish_gbif_data/cleaned/fish_occurrence_metadata_with_tolerance.csv"))
fish_meta<-fish_meta%>%dplyr::select(SpecCode,
                                     maxTtol=maxTtol_celcius,
                                     minTtol=minTtol_celcius)

# now extract community wide value of tolerance limit variability
df<-read.csv(here("Results/fish_splist_with_temp_sensitivity.csv"))
df<-df%>%dplyr::select(STUDY_ID,newsite,spname)

dfc<-read.csv(here("DATA/traitsdata/fish_traits_from_FishBase.csv"))
dfc<-dfc%>%dplyr::select(SpecCode,Species,Author,name_in_mydb)
# first extract temperature-cor traits per community

df2<-left_join(df,dfc,by=c("spname"="name_in_mydb"))# spname is name_in_mydb, search "Catasomus"
df3<-left_join(df2,fish_meta,by="SpecCode")

df4<-df3%>%group_by(STUDY_ID,newsite)%>%
  summarise(avg_maxTtol=mean(maxTtol),
            avg_minTtol=mean(minTtol),
            maxTtol_var=sd(maxTtol)/abs(mean(maxTtol)),
            minTtol_var=sd(minTtol)/abs(mean(minTtol)))%>%ungroup()

taxa<-"fish"
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
sfi<-sm_all%>%filter(TAXA==taxa)

mydat<-left_join(sfi,df4,by=c("STUDY_ID","newsite"))
# response traits (variability) vs. t_med plot

mydat$A<-mydat$L+abs(mydat$U)
mydat$gr<-ifelse(mydat$t_skw>0,1,0)
mydat$gr<-as.factor(mydat$gr)


g3f<-ggplot(mydat,aes(x=t_skw,y=minTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.9)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_color_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g3f


g4f<-ggplot(mydat,aes(x=t_skw,y=maxTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.9)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_color_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g4f

g5f<-ggplot(mydat,aes(y=A,x=t_skw, col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=F)+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.85)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.85)+
  scale_color_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Tail-dependent \n synchrony, A")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g5f

g6fl<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(aes(label=after_stat(p.label)), label.x = 0.5, label.y = 10)+
  stat_regline_equation(label.x = 0.5, label.y = 15)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g6fl

g6f<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="dodgerblue",fill="steelblue1")+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))


g7fl<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(aes(label=after_stat(p.label)), label.x = 0.2, label.y = 10)+
  stat_regline_equation(label.x = 0.2, label.y = 15)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
g7fl

g7f<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="dodgerblue",fill="steelblue1")+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

pdf(here("Results/res_Prelim_Report/traditional_stability_res/birds-fish-skw-taildepsyn.pdf"), width = 10, height = 11.6) 
g3b<-g3b+ annotate("text",  x=Inf, y = Inf, label = "A", vjust=1.5, hjust=1.5, size=10)
g4b<-g4b+ annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5, size=10)
g5b<-g5b+ annotate("text",  x=Inf, y = Inf, label = "C", vjust=1.5, hjust=1.5, size=10)
g6b<-g6b+ annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5, size=10)
g7b<-g7b+ annotate("text",  x=Inf, y = Inf, label = "E", vjust=1.5, hjust=1.5, size=10)
g3f<-g3f+ annotate("text",  x=Inf, y = Inf, label = "F", vjust=1.5, hjust=1.5, size=10)
g4f<-g4f+ annotate("text",  x=Inf, y = Inf, label = "G", vjust=1.5, hjust=1.5, size=10)
g5f<-g5f+ annotate("text",  x=Inf, y = Inf, label = "H", vjust=1.5, hjust=1.5, size=10)
g6f<-g6f+ annotate("text",  x=Inf, y = Inf, label = "I", vjust=1.5, hjust=1.5, size=10)
g7f<-g7f+ annotate("text",  x=Inf, y = Inf, label = "J", vjust=1.5, hjust=1.5, size=10)

grid.arrange(g3b,g4b,g5b,g6b, g7b, 
             g3f,g4f,g5f,g6f, g7f,ncol=2, as.table = FALSE)
dev.off()
