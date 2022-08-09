library(igraph)
library(qgraph)
#pdf()
#op<-par(mfrow=c(2,2))
effet=NULL
# n = number of data points to be pasted in the plot title
# taxa = a character to be pasted in the plot title
# cc = coefficients from psem object
# layout = layout in qgraph
plot_psem<-function(n,taxa,cc,layout,red_edge=1,red_arr=2){
  center=10
  left=0
  right=10
  haut=20
  bas=0
  cex=0.8
  scale_arrow=0.2
  l=cc[,c("Predictor","Response","Std.Estimate","P.Value")]
  l[,3]=as.numeric(l[,3])
  l[,4]=as.numeric(l[,4])
  l$colo=ifelse(l$Std.Estimate>0,0,1)# first value of rgb: black=0, red=1
  #l$colo[which(l$Std.Estimate<0)]=1
  l$lty<-ifelse(l$P.Value<0.05,1,2)# sig=1, nonsig=2(dashed line)
  l$colo<-ifelse(l$lty==2, rgb(l$colo,0,0,0.2),rgb(l$colo,0,0,1)) # now nonsig lines would be transperent
  l$edgelabcex<-ifelse(l$P.Value<0.05,cex,0.0000001)
  
  g <- graph.data.frame(l, directed=T)
  g= g %>% set_edge_attr("color", value =l$colo)
  E(g)$width <- abs(l[,3])
  E(g)$value <- l[,3]
  edge_attr(g,"color")=l$colo
  EL=as_edgelist(g)
  EL=cbind(EL,l[,3])
  asi=abs(l[,3])/(red_arr*scale_arrow)
  asi[asi<5]=3
  
  
  qgraph(EL,edge.color=l$colo,lty=l$lty,layout=layout,
         border.color="white",label.cex=cex,label.scale=F,
         edge.label.cex = l$edgelabcex,edge.label.position=0.6,
         vsize2=4,vsize=9,title=paste("Taxa= ",taxa,", n= ",n," communities",sep=""),
         title.cex=cex,shape="rectangle",edge.labels=T,fade=F,
         label.color = 'blue',
         esize=max(abs(l[,3]))/(red_edge*scale_arrow),asize=asi)
  
}
#par(op)
#dev.off()




