# first for fish
library(tidyverse)
library(ggbreak)
library(dplyr)
library(gridExtra)
library(here)
library(ggpubr)
sm_all<-read.csv(here("Results/stability_metric_and_env_all.csv"))
#range(sm_all$t_med)

#---- for birds -----
taxa<-"birds"
sfi<-sm_all%>%filter(TAXA==taxa)

# all cor with temperature
df<-read.csv(here("Results/birds_splist_with_temp_sensitivity.csv"))

# consistency table
dfc<-read.csv(here("Results/birds_splist_consistency_table.csv"))

# first extract temperature-cor traits per community
df<-df%>%dplyr::select(spearcor_with_t,STUDY_ID,newsite,spname)
df2<-left_join(df,dfc,by="spname")
df2<-df2%>%dplyr::select(spearcor_with_t,STUDY_ID,newsite,spname,avgcorval)

df3<-df2%>%group_by(STUDY_ID,newsite)%>%
  summarise(avgcortraitval=mean(avgcorval),
            cortrait_sd=sd(avgcorval),
            cortrait_var=sd(avgcorval)/abs(mean(avgcorval)))%>%ungroup()

mydat<-left_join(sfi,df3,by=c("STUDY_ID","newsite"))

# Synchrony vs. temperature t_med
gb_H11<-ggplot(data=mydat, aes(x=t_med_celcius, y=phi_LdM)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0, label.y.npc = 0.9)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 1)+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("Overall synchrony")+ylim(0,1)+
  #ggtitle(taxa)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#gb_H11
#ms<-summary(lm(formula = phi_LdM ~ t_med_celcius, data = mydat))
#sl<-ms$coefficients[2,1] #slope
#pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gb_H11<-gb_H11+
#  annotate("text",  x=-12, y = 0.9, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=-0.5, size=4)
#}


# response traits (variability) vs. t_med plot
gb_H12<-ggplot(mydat,aes(x=t_med_celcius,y=cortrait_var))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_y_log10()+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("Variability among species' response \n in community, log10 scale")+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#gb_H12
#ms<-summary(lm(formula = log10(cortrait_var) ~ t_med_celcius, data = mydat))
#sl<-ms$coefficients[2,1] #slope
#pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gb_H12<-gb_H12+
#  annotate("text",  x=-10, y = 7, label=paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=-0.5, size=4)
#}


# variance ratio vs. community-wide correlation traits variability
gb_H13<-ggplot(mydat,aes(x=cortrait_var,y=phi_LdM))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0.7, label.y.npc = 0.6)+
  stat_regline_equation(label.x.npc = 0.5, label.y.npc = 0.7)+
  scale_x_log10() +
  xlab("Variability among species' response \n in community, log10 scale")+
  ylab("Overall synchrony")+ylim(0,1)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#gb_H13
#ms<-summary(lm(formula = phi_LdM ~ log10(cortrait_var), data = mydat))
#sl<-ms$coefficients[2,1] #slope
#pval<-ms$coefficients[2,4] # pvalue
#if(pval<0.05){
#gb_H13<-gb_H13+
#  annotate("text",  x=0.5, y = 0.9, label = paste("sl=",round(sl,2),", p=",round(pval,3),sep=""),
#           vjust=0, hjust=-0.5, size=4)
#}

#---- for fish -----
taxa<-"fish"
sfi<-sm_all%>%filter(TAXA==taxa)

# all cor with temperature

df<-read.csv(here("Results/fish_splist_with_temp_sensitivity.csv"))

# consistency table

dfc<-read.csv(here("Results/fish_splist_consistency_table.csv"))

# first extract temperature-cor traits per community
df<-df%>%dplyr::select(spearcor_with_t,STUDY_ID,newsite,spname)
df2<-left_join(df,dfc,by="spname")
df2<-df2%>%dplyr::select(spearcor_with_t,STUDY_ID,newsite,spname,avgcorval)

df3<-df2%>%group_by(STUDY_ID,newsite)%>%
  summarise(avgcortraitval=mean(avgcorval),
            cortrait_sd=sd(avgcorval),
            cortrait_var=sd(avgcorval)/abs(mean(avgcorval)))%>%ungroup()

mydat<-left_join(sfi,df3,by=c("STUDY_ID","newsite"))

#df4<-df3%>%filter(cortrait_var<0)

# Synchrony vs. temperature t_med
gf_H11<-ggplot(data=mydat, aes(x=t_med_celcius, y=phi_LdM)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0, label.y.npc = 0.06)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.15)+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("")+ylim(0,1)+
  #ggtitle(taxa)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

#gf_H11

# response traits (variability) vs. t_med plot
gf_H12<-ggplot(mydat,aes(x=t_med_celcius,y=cortrait_var))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_y_log10() +
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("")+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#gf_H12

g1wbrk<-ggplot(mydat,aes(x=t_med_celcius,y=cortrait_var))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",color="dodgerblue",fill="steelblue1")+
  geom_hline(yintercept=0,linetype='dashed')+
  scale_y_break(c(11.5,38,42,50,56, 129))+
  ylim(0,132)+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("Variability among species' response \n in community")+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#print(g1wbrk)

# variance ratio vs. community-wide correlation traits variability
gf_H13<-ggplot(mydat,aes(x=cortrait_var,y=phi_LdM))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(aes(label=after_stat(p.label)), label.x.npc = 0, label.y.npc = 0.05)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.13)+
  scale_x_log10() +
  xlab("Variability among species' response \n in community, log10 scale")+ylab("")+ylim(0,1)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
#gf_H13

pdf(here("Results/res_Prelim_Report/traditional_stability_res/birds-fish-medianT-overallsyn.pdf"), width = 7, height = 9) 
gb_H11<-gb_H11+ annotate("text",  x=Inf, y = Inf, label = "A", vjust=1.5, hjust=1.5, size=8)
gb_H12<-gb_H12+ annotate("text",  x=Inf, y = Inf, label = "B", vjust=1.5, hjust=1.5, size=8)
gb_H13<-gb_H13+ annotate("text",  x=Inf, y = Inf, label = "C", vjust=1.5, hjust=1.5, size=8)
gf_H11<-gf_H11+ annotate("text",  x=Inf, y = Inf, label = "D", vjust=1.5, hjust=1.5, size=8)
gf_H12<-gf_H12+ annotate("text",  x=Inf, y = Inf, label = "E", vjust=1.5, hjust=1.5, size=8)
gf_H13<-gf_H13+ annotate("text",  x=Inf, y = Inf, label = "F", vjust=1.5, hjust=1.5, size=8)

grid.arrange(gb_H11,gb_H12,gb_H13, 
             gf_H11,gf_H12,gf_H13,ncol=2, as.table = FALSE)
dev.off()

