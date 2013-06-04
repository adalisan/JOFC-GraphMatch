library(abind)

cache.files<-list.files("./cache/bitflip_1_k_match_clone/",full.names=TRUE)

load(cache.files[[1]])
dims.result.mats<- dim(nc.jofc.dice.wt.f)
dims.result.mats[2]<-0
nc.jofc.sp.f.agg <- array(0,dim=dims.result.mats)
nc.jofc.sp.p.agg <- array(0,dim=dims.result.mats)
nc.jofc.sp.r.agg <- array(0,dim=dims.result.mats)
nc.jofc.sp.tm.agg <- array(0,dim=dims.result.mats)


 for (file in cache.files){
 load(file)
 
#nc.jofc.sp.f.agg<- Reduce(f <-function(x,y){ abind(x,y,along=2) },
#nc.jofc.sp.f)
nc.jofc.sp.f.agg<- abind(nc.jofc.sp.f.agg,nc.jofc.dice.wt.f,along=2)
nc.jofc.sp.tm.agg<- abind(nc.jofc.sp.tm.agg,nc.jofc.dice.wt.tm,along=2)
nc.jofc.sp.p.agg<- abind(nc.jofc.sp.p.agg,nc.jofc.dice.wt.p,along=2)
nc.jofc.sp.r.agg<- abind(nc.jofc.sp.r.agg,nc.jofc.dice.wt.r,along=2)
 }


nc.jofc.sp.f<-nc.jofc.sp.f.agg
nc.jofc.sp.p<-nc.jofc.sp.p.agg
nc.jofc.sp.r<-nc.jofc.sp.r.agg
nc.jofc.sp.tm<-nc.jofc.sp.tm.agg








pdf("./graphs/plot-PRF2.pdf")
colors.vec<-c( "red","blue","orange","green")
par(lty=1)
for (m_v_i in 1:m_len){
par(lty=m_v_i)
plot.graph.with.CI.error(t(nc.jofc.sp.f[,,m_v_i]),plot.title="",plot.col=colors.vec[1],
                         conf.int=TRUE,add=m_v_i>1,fp.points=pert,
                         customx.labels=NULL,customy.labels=NULL,
                         ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                         ylab="Average F-measure/Precision/Recall")
par(lty=m_v_i)
plot.graph.with.CI.error(t(nc.jofc.sp.p[,,m_v_i]),plot.title="",plot.col=colors.vec[3],
                         conf.int=TRUE,add=TRUE,fp.points=pert,
                         customx.labels=NULL,customy.labels=NULL,
                         ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                         ylab="Average F-measure/Precision/Recall")
par(lty=m_v_i)
plot.graph.with.CI.error(t(nc.jofc.sp.r[,,m_v_i]),plot.title="",plot.col=colors.vec[4],
                         conf.int=TRUE,add=TRUE,fp.points=pert,
                         customx.labels=NULL,customy.labels=NULL,
                         ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                         ylab="Average F-measure/Precision/Recall")
}
legend.txt<- c("F-measure","Precision","Recall")
title("1-to-k matching o jofc")
abline(h=1/(m),lty=2) ### chance?  apparently not!?
abline(v=1/2,lty=2) ### chance?  gotta be!?
legend.txt<-c(legend.txt,m_vals)
ltypes<- c(rep(1,3),1:m_len)
legend(x="topright",legend=legend.txt, col =colors.vec[c(c(1,3,4),rep(1,m_len))],lty=ltypes)



dev.off()

pdf("./graphs/Total_precision_1_to_k_match.pdf")
colors.vec<-c( "red","blue","orange","green","magenta")
par(lty=1)
for (m_v_i in 1:m_len){
#  par(lty=m_v_i)
  plot.graph.with.CI(t(nc.jofc.sp.tm[,,m_v_i]),plot.title="",plot.col=colors.vec[m_v_i],
                           conf.int=TRUE,add=m_v_i>1,fp.points=pert,
                           customx.labels=NULL,customy.labels=NULL,
                           ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                           ylab="Average F-measure")
}
  
legend.txt<- c()
title("1-to-k matching o jofc")
abline(h=1/(m),lty=2) ### chance?  apparently not!?
abline(v=1/2,lty=2) ### chance?  gotta be!?
legend.txt<-c(legend.txt,n-m_vals)
ltypes<- c(1:m_len)
legend(x="topright",legend=legend.txt, col =colors.vec[1:m_len],lty=1,title="Num. of Hard Seeds")

dev.off()



##ggplot

tm.shortpath.long.format <- melt(nc.jofc.sp.tm,
			varnames=c("pert","it","m"),value.name="match.ratio")
tm.shortpath.long.format$m <- (n-m_vals)[tm.shortpath.long.format$m]
tm.shortpath.long.format$pert <- as.factor(pert[tm.shortpath.long.format$pert])
tm.shortpath.summ <- summarySE(tm.shortpath.long.format,measurevar="match.ratio",
						groupvars=c("m","pert") )
			



ggplot(tm.shortpath.summ , aes(m, y=match.ratio, colour=pert)) +  
  geom_errorbar(aes(ymin=match.ratio-ci, ymax=match.ratio+ci),size=1, width=2) +
  geom_point(size=2)+geom_line(size=1.2) +theme_minimal()+theme(text=element_text(size=22)) +
    labs(title="Many-to-one Matching",x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,100,10)) +
    scale_y_continuous(breaks=seq(0,1,.2)) +
           guides(colour=guide_legend( title =expression(rho),title.hjust=1,title.vjust=-1,label.hjust=1))+
scale_colour_hue()

  scale_colour_manual(breaks = pert,
                      labels =pert,
                      values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00")) +
  scale_fill_manual(breaks = pert,
                      labels =pert,

                      values = c("#E69F00", "#56B4E9", "#009E73", 
                                 "#F0E442", "#0072B2", "#D55E00"))



  


























