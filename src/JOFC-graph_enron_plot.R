library(ggplot2)
library(reshape2)


corr.results.unlist.agg<-corr.matches.e

corr.results.avg<-Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var<-lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")
n<-total_v



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
corr.summ<-summarySE(data=corr.results.lf,measurevar="value",groupvar=c("w","m"))


ggplot(corr.summ, aes(x=m, y=value, colour=w)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
  geom_point(size=2)+geom_line(size=1.2)+theme_minimal()+theme(text=element_text(size=22)) +
  labs(title=paste("JOFC",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,300,25)) +
  scale_y_continuous(breaks=seq(0,1,.2)) +
  guides(colour=guide_legend( title =expression(w),title.hjust=1,title.vjust=-1,label.hjust=1))
