library(ggplot2)
library(reshape2)

corr.results.unlist.agg<-corr.matches
n<-total_v
nmc<- length(corr.matches)
corr.results.avg<-Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var<-lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")


SGM.vs.JOFC<- TRUE

proj.dataframe<-list()

corr.results.lf<-data.frame()
w.max.index<-length(w.vals)

for (l.i in 1:length(corr.results.unlist.agg)){
  
  corr.results.avg.frac.w.i<- (corr.results.unlist.agg[[l.i]])
  #corr.results.avg.frac.w.i<-cbind(n_vals,corr.results.avg.fracs.w.i)
  colnames(corr.results.avg.frac.w.i) <-w.vals
  corr.results.l.i<-melt(corr.results.avg.frac.w.i)
  corr.results.l.i$Var2<-as.factor(corr.results.l.i$Var2)
  corr.results.l.i$Var1<-n_vals[corr.results.l.i$Var1]
  corr.results.l.i$value<-corr.results.l.i$value/(n-corr.results.l.i$Var1)
  
  corr.results.lf <-rbind(corr.results.lf ,corr.results.l.i)
}
names(corr.results.lf)<- c("m","w","value")

if (SGM.vs.JOFC){
  library(R.matlab)
  library(arrayhelpers)
  cnet_sgm <- readMat("~/projects/SeededGraphMatch/cache/cnet-carey-exp_short.mat")
  corr.results.lf<- cbind(corr.results.lf,algo=rep("JOFC",nrow(corr.results.lf)))
  match.ratio.SGM <- cnet_sgm$fc.agg
  match.ratio.SGM.molten.df <- array2df(match.ratio.SGM )
  nmc_SGM<- dim(match.ratio.SGM)[2]
  n_vals_SGM <- as.vector(cnet_sgm$n.vals)
  match.ratio.SGM.molten.df$d1[match.ratio.SGM.molten.df$d1==7]=6
  m <- factor(match.ratio.SGM.molten.df$d1)
  levels(m)=unique(n_vals_SGM)
  match.ratio.SGM.lf<- cbind(m=m,w=factor(rep(w.vals[1],nmc_SGM),labels=w.vals),value=match.ratio.SGM.molten.df$match.ratio.SGM,algo=rep("SGM",nmc_SGM))
  
  joint.results.lf <- rbind(corr.results.lf, match.ratio.SGM.lf)
  
  #corr.results.lf <- joint.results.lf
  
}



if (!SGM.vs.JOFC){
  
  corr.summ<-summarySE(data=corr.results.lf,measurevar="value",groupvar=c("w","m"))
  
first.plot<- ggplot(corr.summ, aes(x=m, y=value, colour=w)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
  geom_point(size=2)+geom_line(size=1.2)+theme_minimal()+theme(text=element_text(size=22)) +
  labs(title=paste("JOFC",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,180,25),limits=c(20,180)) +
  scale_y_continuous(breaks=seq(0,1,.1),limits=c(0,1)) +
  guides(colour=guide_legend( title =expression(w),title.hjust=1,title.vjust=-1,label.hjust=1))

first.plot 
first.plot<- first.plot+geom_errorbar(data=corr.summ, aes(ymin=value-ci, ymax=value+ci),size=1, width=6,colour="green") 

first.plot
} else{
  corr.summ<-summarySE(data=corr.results.lf,measurevar="value",groupvar=c("algo","w","m"))
  
  
  first.plot<- ggplot(corr.summ, aes(x=m, y=value, colour=algo)) + 
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
  #  geom_point(size=2) +# geom_line(size=1.2) +
  theme_minimal()+theme(text=element_text(size=22)) +
    labs(title=paste("JOFC vs FAQ for ",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,400,25),limits=c(0,300)) +
    scale_y_continuous(breaks=seq(0,1,.1),limits=c(0,0.3)) #+
   # guides(colour=guide_legend( title =algo,title.hjust=1,title.vjust=-1,label.hjust=1))
  
  first.plot 
  
  #corr.summ.sgm<-summarySE(data= match.ratio.SGM.lf,measurevar="value",groupvar=c("m"))
  corr.summ.sgm.mean <- cnet_sgm$avg.line
  corr.summ.sgm.sd <- cnet_sgm$sd.line
  corr.summ.sgm<-data.frame(value=corr.summ.sgm.mean,ci=2*corr.summ.sgm.sd/sqrt(992),algo="FAQ",w=w.vals[1],m=n_vals_SGM)
  
  
  first.plot<- first.plot+geom_errorbar(data=corr.summ.sgm, aes(x=n_vals_SGM,ymin=value-ci, ymax=value+ci),size=1, width=6,colour="green") 
  first.plot
  
  corr.summ.agg<- rbind(corr.summ[,c("value","ci","algo","w","m")],corr.summ.sgm)
  second.plot <- ggplot(corr.summ.agg, aes(x=m, y=value, colour=algo)) + 
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
    #  geom_point(size=2) +# geom_line(size=1.2) +
    theme_minimal()+theme(text=element_text(size=22)) +
    labs(title=paste("JOFC vs FAQ for ",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,400,25),limits=c(0,300)) +
    scale_y_continuous(breaks=seq(0,1,.1),limits=c(0,0.3))
  second.plot
}
 
