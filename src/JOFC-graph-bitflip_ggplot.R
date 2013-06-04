windows()
library(ggplot2)
library(reshape2)

source("./lib/simulation_math_util_fn.R")
  w.i <- 1

#cache.files<-list.files("./cache/JOFC_sim_bitflip_300_sp_highdim_questmark",full.names=TRUE)
cache.files<-list.files("./cache/JOFC_sim_bitflip_300_sp",full.names=TRUE)


corr.results.unlist.agg<-list()
 for (file in cache.files){
 load(file)
 corr.results.unlist.agg<-c( corr.results.unlist.agg,corr.results.unlist)
 }





corr.results.avg<-Reduce("+",corr.results.unlist.agg)/nmc
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")




proj.dataframe<-list()

corr.results.lf<-data.frame()

for (l.i in 1:length(corr.results.unlist.agg)){

corr.results.avg.frac.w.i<- drop(corr.results.unlist.agg[[l.i]][,,w.i])
#corr.results.avg.frac.w.i<-cbind(n_vals,corr.results.avg.frac.w.i)
colnames(corr.results.avg.frac.w.i) <-c(pert)
corr.results.l.i<-melt(corr.results.avg.frac.w.i)
corr.results.l.i$Var2<-as.factor(corr.results.l.i$Var2)
corr.results.l.i$Var1<-n_vals[corr.results.l.i$Var1]
corr.results.l.i$value<-corr.results.l.i$value/(n-corr.results.l.i$Var1)

corr.results.lf <-rbind(corr.results.lf ,corr.results.l.i)
}
names(corr.results.lf)<- c("m","rho","value")
corr.summ<-summarySE(data=corr.results.lf,measurevar="value",groupvar=c("rho","m"))


ggplot(corr.summ, aes(x=m, y=value, colour=rho)) + 
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
  geom_point(size=2)+geom_line(size=1.2)+theme_minimal()+theme(text=element_text(size=22)) +
    labs(title="JOFC",x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,300,25)) +
    scale_y_continuous(breaks=seq(0,1,.2)) +
           guides(colour=guide_legend( title =expression(rho),title.hjust=1,title.vjust=-1,label.hjust=1))


colors.vec <- c("red","green","aquamarine","purple",
		"darkblue","salmon","rosybrown","magenta","orange")
colors.vec[3]<-"gold4"
colors.vec[2]<-"darkblue"
colors.vec[4]<-"darkorange4"
colors.vec[9]<-"red"
colors.vec.len<-length(colors.vec)

require(RColorBrewer)
colors.vec.brew<- brewer.pal(colors.vec.len,"YlOrRd")

colors.vec.brew[colors.vec.len+1]<-"cornflowerblue"
colors.vec.brew[colors.vec.len+2]<-"azure3"
colors.vec.brew[colors.vec.len+3]<-"cyan"





colors.vec.len <- length(colors.vec.brew)
colors.vec.brew[(colors.vec.len-2):colors.vec.len] <- brewer.pal(3,"Set3")
#palette("YlOrRd")



w.i = 1
plot(n_vals, as.vector(corr.results.avg.frac[,1,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")

npert<-length(pert)
for(ipert in 2:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert])
}  




  

  ggplot(corr.summ.FAQ, aes(x=m, y=value, colour=rho)) + 
         geom_errorbar(aes(ymin=value-ci, ymax=value+ci),size=1, width=6) +
         geom_point(size=2)+geom_line(size=1.2)+theme_minimal() +theme(text=element_text(size=22)) +
    labs(title="FAQ",x=expression(m),y=((expression(delta^{(m)}))))+scale_x_continuous(breaks=seq(0,300,25)) +
    scale_y_continuous(breaks=seq(0,1,.2)) +
    guides(colour=guide_legend( title =expression(rho),title.hjust=1,title.vjust=-1,label.hjust=1))








