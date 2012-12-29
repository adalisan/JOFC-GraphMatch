
attach(result.mats)
require(R.matlab)
plot.graph.with.CI<-function(plot.roc.points,plot.title,plot.col,conf.int=TRUE,add=FALSE,
                             fp.points=seq(0,1,0.01),customx.labels=NULL,customy.labels=NULL,ispowercurve=TRUE,...){
  
  standardx.axis <- FALSE
  standardy.axis <- FALSE
  if (is.null(customx.labels))
    standardx.axis<-TRUE
  if (is.null(customy.labels))
    standardy.axis<-TRUE
  
  num.sims<-dim(plot.roc.points)[1]
  
  y.points<-colMeans(plot.roc.points,na.rm=TRUE)
  var.y.points <-rep (0,length(fp.points))
  var.y.points <- colVars(plot.roc.points,na.rm=TRUE)
  std.y.points <- 2*sqrt(var.y.points)/sqrt(num.sims)
  ucl <- y.points+std.y.points
  lcl <- y.points-std.y.points
  
  if (add){
    lines(x=fp.points,y= y.points,main=plot.title,
          col=plot.col,xaxt=ifelse(standardx.axis,"s","n"),
          yaxt=ifelse(standardy.axis,"s","n"),...)
    
  }
  else{
    plot(x=fp.points,y= y.points,main=plot.title,xaxt=ifelse(standardx.axis,"s","n"),
         yaxt=ifelse(standardy.axis,"s","n"), col=plot.col,type='l', ...)
  }
  
  if (!standardx.axis)
    axis(1, at=fp.points,labels=customx.labels)
  if (!standardy.axis)		
    axis(2, at=y.points,labels=customy.labels)
  
  
  
  if (conf.int){
    arrows(fp.points,ucl,fp.points,lcl,length=.1,angle=90,code=3, lwd=1.5,lty=1,col=plot.col)
  }
  
  par(lty=1)		
}


#FAQ weighted undirected
plot.graph.with.CI(t(fc.rqap.agg),"C. elegans-Hermeaphrodite","red",
                   fp.points=n.vals,lwd=3,lty=2,xlab="m",ylab="delta_m",ylim=c(0,0.3))
#FAQ unweighted undirected
plot.graph.with.CI(t(fc.unwt.agg),"C. elegans-Hermeaphrodite","red",fp.points=n.vals,lwd=3,lty=1,add=T)

#FAQ weighted directed
plot.graph.with.CI(t(fc.dir.agg),"C. elegans-Hermeaphrodite","red",fp.points=n.vals,lwd=3,lty=3,add=T)
#FAQ unweighted directed
plot.graph.with.CI(t(fc.unwt.dir.agg),"C. elegans-Hermeaphrodite","red",fp.points=n.vals,lwd=3,lty=4,add=T)
#plot.graph.with.CI(t(fc.unseed.dir.agg),"C. elegans-Hermeaphrodite","red",fp.points=n.vals,lwd=3)

lty.vec <- c(2,1,3,4)
lwd.vec <- rep(3,4)
color.vec <- rep("red",4)

#FAQ unweighted undirected AND unseeded
plot.graph.with.CI(t(fc.seed.agg),"C. elegans-Hermeaphrodite","red",fp.points=n.vals,lwd=1,lty=1,add=T)

lty.vec <- c(lty.vec,1)
lwd.vec <- c(lwd.vec,1)
color.vec <- c(color.vec,"red")


#JOFC weighted undirected
plot.graph.with.CI(t(avg.corr.worm.wt.dice),"C. elegans-Hermeaphrodite","green",fp.points=n_vals_worm,
                   lwd=3,lty=2,add=T)
#JOFC unweighted undirected
plot.graph.with.CI(t(avg.corr.worm.wt.dice.unwt),"C. elegans-Hermeaphrodite","green",fp.points=n_vals_worm,
                   lwd=3,lty=1,add=T)
#JOFC weighted directed
plot.graph.with.CI(t(avg.corr.worm.wt.dice.directed),"C. elegans-Hermeaphrodite","green",fp.points=n_vals_worm,
                   lwd=3,lty=3,add=T)
#JOFC unweighted undirected
plot.graph.with.CI(t(avg.corr.worm.wt.dice.unwt.directed),"C. elegans-Hermeaphrodite","green",fp.points=n_vals_worm,
                   lwd=3,lty=4,add=T)

lty.vec <- c(lty.vec,2,1,3,4)
lwd.vec <- c(lwd.vec,rep(3,4))
color.vec <- c(color.vec,rep("green",4))


legend.txt<-c('FAQ -  undirected weighted',
              
              'FAQ -  undirected unweighted',
              'FAQ -  directed   weighted',
              
              'FAQ -  directed   unweighted',
            
              'FAQ -  undirected no seed',
              
              'JOFC - undirected weighted',
              
              'JOFC - undirected unweighted',
              'JOFC - directed   weighted',
              
              'JOFC - directed   unweighted')
legend("topleft",legend=legend.txt,col=color.vec,lty=lty.vec,lwd=lwd.vec)
detach(results.mat)              