load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 1  at  2013-08-25 38 .Rdata")
w.vals<-1
corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)





load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 2  at  2013-08-20 83 .Rdata")
w.vals<-2
corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)







load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 4  at  2013-08-12 96 .Rdata")
w.vals<-4
corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)
first.plot<- ggplot(corr.summ.agg, aes(x=m, y=value, colour=w)) +
geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
geom_point(size=2)+geom_line(size=1.2)+theme_minimal()+theme(text=element_text(size=22)) +
labs(title=paste("JOFC",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,180,25),limits=c(20,180)) +
scale_y_continuous(breaks=seq(0,1,.1),limits=c(0,1)) +
guides(colour=guide_legend( title =expression(w),title.hjust=1,title.vjust=-1,label.hjust=1))
first.plot




load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 10  at  2013-08-16 57 .Rdata")
w.vals<-10
corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)
load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 11  at  2013-08-26 69 .Rdata")
w.vals<-11
corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)







load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 5  at  2013-08-12 90 .Rdata")
w.vals<-5

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)





load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 6  at  2013-08-15 79 .Rdata")
w.vals<-6

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)


load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 7  at  2013-08-15 57 .Rdata")
w.vals<-7

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)



load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 8  at  2013-08-16 42 .Rdata")
w.vals<-8

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)






load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 9  at  2013-08-16 27 .Rdata")
w.vals<-9

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)







load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 13  at  2013-09-01 26 .Rdata")
w.vals<-13

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)
load("~/projects/JOFC-GraphMatch/cache/JOFC-graph- charitynet _param 14  at  2013-09-02 81 .Rdata")
w.vals<-14

corr.results.unlist.agg<-corr.matches
n<-total_v
corr.results.avg <- Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var <- lapply(corr.results.unlist.agg,function(x){(x-corr.results.avg)^2})
corr.results.var <- Reduce("+",corr.results.var)/nmc
corr.results.var <- corr.results.var/(nmc)
corr.results.sd  <- sqrt(corr.results.var)
corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,  1,n-n_vals,"/")
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
corr.summ.agg<-rbind(corr.summ.agg,corr.summ)
first.plot<- ggplot(corr.summ.agg, aes(x=m, y=value, colour=w)) +
geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
geom_point(size=2)+geom_line(size=1.2)+theme_minimal()+theme(text=element_text(size=22)) +
labs(title=paste("JOFC",graph_data),x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,180,25),limits=c(20,180)) +
scale_y_continuous(breaks=seq(0,1,.1),limits=c(0,1)) +
guides(colour=guide_legend( title =expression(w),title.hjust=1,title.vjust=-1,label.hjust=1))
first.plot
