# This is a script that will pull source data and generate figures for the main text
# You can choose size of PDF/ figure file to save these figures, 
# here I did not save any figure, rather you can visualize in R studio plot window


rm(list=ls())
library(here)
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)

#==================== code for Fig. 2b-f ==========================

sumdat<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig2.xls"), sheet= "summary_data_Fig2b-f")
sumdat<-sumdat[1:5,]
gp<-vector(mode = "list", length = nrow(sumdat))
titles<-c("No trend, no skewness", 
          "No trend, no skewness, but higher MedianT, lower VarT than B",
          "No trend, extreme cold (-ve skewness)",
          "Increasing trend, no skewness",
          "Increasing trend, heatwaves (+ve skewness)")

for(i in 1:nrow(sumdat)){
  
  j<-i+1
  m<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig2.xls"), sheet= j)
  gp[[i]]<-ggplot(m,aes(x=year,y=t_in_celcius))+
    geom_point(col="gray")+
    geom_line(col="gray")+ggtitle(paste(" TrendT= ",round(sumdat$TrendT[i],3),
                                        ", SkewT= ",round(sumdat$SkewT[i],3),
                                        ", VarT= ",round(sumdat$VarT[i],3),sep=""))+
    geom_hline(yintercept = sumdat$MedianT[i],linetype = "dashed")+
    xlab("Year")+ylab("Temperature (\u00B0C)")+xlim(1997,2020)+
    annotate(geom = 'text', color="blue",
             label = titles[i], 
             x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5)+
    annotate("text",  x=Inf, y = Inf, label = letters[i+1], vjust=1.2, hjust=1.2, size=10)+
    theme_bw()+
    theme(text = element_text(size = 16),axis.text = element_text(size = 16),
          plot.title = element_text(size=14),
          #plot.tag.position = c(0.1, 0.9),
          plot.margin = margin(t = 8, r = 15, b = 4, l = 4, unit = "pt"),
          panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))
}

grid.arrange(gp[[1]],gp[[2]],gp[[3]],gp[[4]],gp[[5]],nrow=5) # this is Fig2, panels b-f



#================= code for Fig. 3 ============================

library(piecewiseSEM)
source(here("R/plot_psem.R")) 
datfig3<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig3.xls"), sheet= 2)
datfig3<-datfig3[,1:7]
db3<-datfig3%>%filter(Taxa=="Birds")
db3<-as.data.frame(db3)
df3<-datfig3%>%filter(Taxa=="Fish")
df3<-as.data.frame(df3)

plot_psem(n=1246,taxa="Birds",cc=db3,layout="circle") # Fig. 3a
plot_psem(n=580,taxa="Fish",cc=df3,layout="circle") # Fig. 3b

gp<-ggplot(datfig3, aes(x=path, y=Std.Estimate, fill=Taxa)) + 
  geom_bar(stat="identity", position="identity")+theme_bw()+
  ylab("Standardized estimate")+xlim(0.05,30)+
  theme(
    panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(),
    panel.background=element_rect(fill="white", colour="white"), 
    legend.position="top",text=element_text(size=15), axis.ticks.x = element_blank(),
    axis.text.x = element_blank())+
  scale_fill_manual(values=alpha(c("green3","dodgerblue"), 0.5))+
  scale_color_manual(values=alpha(c("green3","dodgerblue"), 0.5))
gp # Fig. 3c (simple version, later it was developed in inkscape)



#================= code for Fig. 4 ============================

library(ggpubr)
dbf<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig4.xls"), sheet= 1)
dbf<-dbf[,1:4]


# first for birds

mydat<-dbf%>%filter(TAXA=="birds")

gb_H11<-ggplot(data=mydat, aes(x=MedianT, y=Overall_Synchrony)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  #stat_poly_line() +
  #stat_poly_eq(use_label(c("eq", "p", "n"))) +
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.9)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 1)+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("Overall synchrony")+ylim(0,1)+
  #ggtitle(taxa)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

gb_H12<-ggplot(mydat,aes(x=MedianT,y=Variability_in_response))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_y_log10()+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("Variability among species' response \n in community, log10 scale")+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

gb_H13<-ggplot(mydat,aes(x=Variability_in_response,y=Overall_Synchrony))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="green",fill="greenyellow")+
  stat_cor(method="pearson", label.x.npc = 0.5, label.y.npc = 0.6)+
  stat_regline_equation(label.x.npc = 0.5, label.y.npc = 0.7)+
  scale_x_log10() +
  xlab("Variability among species' response \n in community, log10 scale")+
  ylab("Overall synchrony")+ylim(0,1)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

# now for fish

mydat<-dbf%>%filter(TAXA=="fish")

gf_H11<-ggplot(data=mydat, aes(x=MedianT, y=Overall_Synchrony)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.06)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.15)+
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("")+ylim(0,1)+
  #ggtitle(taxa)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

gf_H12<-ggplot(mydat,aes(x=MedianT,y=Variability_in_response))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_y_log10() +
  xlab("Temperature, MedianT (\u00B0C)")+
  ylab("")+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

gf_H13<-ggplot(mydat,aes(x=Variability_in_response,y=Overall_Synchrony))+geom_point(alpha=0.2)+
  geom_smooth(method="lm",se=T,color="dodgerblue",fill="steelblue1")+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.05)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.13)+
  scale_x_log10() +
  xlab("Variability among species' response \n in community, log10 scale")+ylab("")+ylim(0,1)+
  theme_bw()+
  theme(text = element_text(size = 12),axis.text = element_text(size = 12),
        plot.margin = margin(t = 8, r = 9, b = 4, l = 4, unit = "pt"),
        panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))


gb_H11<-gb_H11+ annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=1.5, size=8)
gb_H12<-gb_H12+ annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=1.5, size=8)
gb_H13<-gb_H13+ annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5, size=8)
gf_H11<-gf_H11+ annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5, size=8)
gf_H12<-gf_H12+ annotate("text",  x=Inf, y = Inf, label = "e", vjust=1.5, hjust=1.5, size=8)
gf_H13<-gf_H13+ annotate("text",  x=Inf, y = Inf, label = "f", vjust=1.5, hjust=1.5, size=8)

grid.arrange(gb_H11,gb_H12,gb_H13, 
             gf_H11,gf_H12,gf_H13,
             ncol=2, as.table = FALSE) # Fig. 4


#================= code for Fig. 5 ============================

dbf<-read_excel(here("source_data_maintext_figures/Ghosh_SourceData_Fig5.xls"), sheet= 1)
dbf<-dbf[,1:5]

colnames(dbf)<-c("TAXA","t_skw","minTtol_var","maxTtol_var","A")
dbf$t_skw<-as.numeric(dbf$t_skw) # make sure this is numeric
dbf$gr<-ifelse(dbf$t_skw>0,1,0)
dbf$gr<-as.factor(dbf$gr)
dbf<-as.data.frame(dbf)

# first for birds
mydat<-dbf%>%filter(TAXA=="birds")
g3b<-ggplot(mydat,aes(x=t_skw,y=minTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.6)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.8)+
  scale_color_manual(values=c("green2","green4"))+
  scale_fill_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g4b<-ggplot(mydat,aes(x=t_skw,y=maxTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 1)+
  scale_color_manual(values=c("green2","green4"))+
  scale_fill_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g5b<-ggplot(mydat,aes(y=A,x=t_skw, col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.75)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  scale_color_manual(values=c("green2","green4"))+
  scale_fill_manual(values=c("green2","green4"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Tail-dependent \n synchrony, A")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g6b<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="green",fill="greenyellow", alpha=0.3)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g7b<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="green",fill="greenyellow", alpha=0.3)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))


# now for fish
mydat<-dbf%>%filter(TAXA=="fish")
g3f<-ggplot(mydat,aes(x=t_skw,y=minTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.7)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.9)+
  scale_color_manual(values=c("skyblue2","navy"))+
  scale_fill_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g4f<-ggplot(mydat,aes(x=t_skw,y=maxTtol_var,col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.8)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 1)+
  scale_color_manual(values=c("skyblue2","navy"))+
  scale_fill_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Community-level variability \n in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g5f<-ggplot(mydat,aes(y=A,x=t_skw, col=gr),add="reg.line")+
  geom_point(alpha=0.2)+
  geom_smooth(method="lm", se=T, aes(fill=gr), alpha=0.3)+
  stat_cor(method="pearson", label.x.npc = 0, label.y.npc = 0.6)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.85)+
  scale_color_manual(values=c("skyblue2","navy"))+
  scale_fill_manual(values=c("skyblue2","navy"))+
  xlab("Temperature skewness, SkewT")+
  ylab("Tail-dependent \n synchrony, A")+
  theme_bw()+theme(text = element_text(size = 12),
                   legend.position = "none",
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g6f<-ggplot(mydat,aes(y=A,x=minTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="dodgerblue",fill="steelblue1", alpha=0.3)+
  #geom_smooth(method="loess", se=T,color="dodgerblue",fill="steelblue1")+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' minimum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g7f<-ggplot(mydat,aes(y=A,x=maxTtol_var))+
  #scale_y_log10()+
  geom_point(alpha=0.2)+
  geom_smooth(method="loess", se=T,color="dodgerblue",fill="steelblue1", alpha=0.3)+
  ylab("Tail-dependent \n synchrony, A")+
  xlab("Community-level variability in species' maximum \n thermal tolerance limit")+
  theme_bw()+theme(text = element_text(size = 12),
                   axis.text = element_text(size = 12),
                   plot.margin = margin(t = 9, r = 7, b = 4, l = 12, unit = "pt"),
                   panel.grid = element_line(color = rgb(235, 235, 235, 100, maxColorValue = 255)))

g3b<-g3b+ annotate("text",  x=Inf, y = Inf, label = "a", vjust=1.5, hjust=1.5, size=10)
g4b<-g4b+ annotate("text",  x=Inf, y = Inf, label = "b", vjust=1.5, hjust=1.5, size=10)
g5b<-g5b+ annotate("text",  x=Inf, y = Inf, label = "c", vjust=1.5, hjust=1.5, size=10)
g6b<-g6b+ annotate("text",  x=Inf, y = Inf, label = "d", vjust=1.5, hjust=1.5, size=10)
g7b<-g7b+ annotate("text",  x=Inf, y = Inf, label = "e", vjust=1.5, hjust=1.5, size=10)
g3f<-g3f+ annotate("text",  x=Inf, y = Inf, label = "f", vjust=1.5, hjust=1.5, size=10)
g4f<-g4f+ annotate("text",  x=Inf, y = Inf, label = "g", vjust=1.5, hjust=1.5, size=10)
g5f<-g5f+ annotate("text",  x=Inf, y = Inf, label = "h", vjust=1.5, hjust=1.5, size=10)
g6f<-g6f+ annotate("text",  x=Inf, y = Inf, label = "i", vjust=1.5, hjust=1.5, size=10)
g7f<-g7f+ annotate("text",  x=Inf, y = Inf, label = "j", vjust=1.5, hjust=1.5, size=10)

grid.arrange(g3b,g4b,g5b,g6b, g7b, 
             g3f,g4f,g5f,g6f, g7f,
             ncol=2, as.table = FALSE) # Fig. 5

