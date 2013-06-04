

debug.mode<-TRUE
debug.mode.fine <-FALSE
verbose <- FALSE
t_stamp_orig<-130
t_stamp_multi<-131
m_vals <- seq(10,140,10)

m_len <- length(m_vals)

n <- unique.count
new.n <-gr.size[1] 
nmc<-100
pert= c(0,0.1,0.3,0.5)
pert= 0
npert <-  length(pert)
w.vals.vec<-0.7
d.start<-3

G.orig<-as.matrix(AAA.clean[t_stamp_orig,,])
G.multi<- AAA[[t_stamp_multi]]

G.orig <-apply(AAA.clean,c(2,3),sum)
G.multi <- Reduce('+',AAA)


nc.jofc.dice.wt.p <- nc.jofc.dice.wt.r <- nc.jofc.dice.wt.f <- array(0,dim=c(npert,nmc,m_len))


nc.cmds = matrix(0,npert,nmc)

matched.cost<-0.01


#w.vals.vec <- c(0.5,0.7,0.9,0.95)
w.vals.vec <- c(0.8)

w.max.index<-length(w.vals.vec)

matched.cost<-0.01 #If matched.cost is equal to 1, consider an unweighted graph, with edges between matched vertices
#If matched.cost is  between 0 and 1, the graph is weighted with edges between matched vertices with weights equal to matched.cost. Edges between 
# vertices of the same condition have either weight 1 or 2 according to whether they're connected according to given adjacency matrix or not.

d.start <- 3
T.diff<-2





require(igraph)
require(optmatch)
require(MASS)
source("./lib/simulation_math_util_fn.R")
source("./lib/smacofM.R")
source("./lib/oosIM.R")
source("./lib/oosMDS.R")
source("./lib/diffusion_distance.R")
source("./lib/graph_embedding_fn_many.R")




for(imc in 1:nmc)
{
  
  
  for (m_it in 1:m_len) {
    m<- m_i <- m_vals[m_it]
    
    oos.sampling<-sample(1:n, size=m_i, replace=FALSE)
    in.sample.ind.1<-rep(TRUE,new.n)
    for ( s in 1:m){
      oos.in.G.rep <- Find(function(x){x$b==oos.sampling[s]},corr.list)
      
      in.sample.ind.1[oos.in.G.rep$a]<-FALSE
    }
    
    in.sample.ind.2<-rep(TRUE,n)
    in.sample.ind.2[oos.sampling]<-FALSE
    
    #if (imc==1) print(in.sample.ind)
    
    Gp<-G.orig
    G.rep<- G.multi
    for (pert_i in  1:npert) {
    #  Gp <- Gp.list[[pert_i]]
    #  G.rep <- G.list[[pert_i]]
      J.1 = try(JOFC.graph.custom.dist.many (G.rep, Gp, corr.list,
                                        in.sample.ind.1,in.sample.ind.2,
                                        d.dim=d.start,
                                        w.vals.vec=w.vals.vec,
                                        graph.is.directed=FALSE,
                                        vert_diss_measure  =  'C_dice_weighted',
                                        T.param  =  NULL,
                                        
                                        graph.is.weighted=FALSE)
           )
      if (inherits(J.1,"try-error")){
        nc.jofc.dice.wt.p[pert_i,imc,m_it] <- nc.jofc.dice.wt.r[pert_i,imc,m_it] <- nc.jofc.dice.wt.f[pert_i,imc,m_it] <- NA
        next
      }
      
      #print(head(J.1[[1]]))
      #print(diag(J.1[[1]]))
      M = solveMarriage.many(J.1[[1]],10)
      print(head(M))
      match.perf.eval <- present.many(M,corr.list)
      nc.jofc.dice.wt.p[pert_i,imc,m_it] = mean(match.perf.eval$P)
      nc.jofc.dice.wt.r[pert_i,imc,m_it] = mean(match.perf.eval$R)
      nc.jofc.dice.wt.f[pert_i,imc,m_it] <- mean(match.perf.eval$F)
      print(dim(nc.jofc.dice.wt.p))
      if (pert_i>1 && imc>1){
        print("Precision")
        print(apply(nc.jofc.dice.wt.p[,1:imc,m_it],1,mean))
        print("Recall")
        print(apply(nc.jofc.dice.wt.r[,1:imc,m_it],1,mean))
        print("F-measure")
        print(apply(nc.jofc.dice.wt.f[,1:imc,m_it],1,mean))
      }
      else{
        print("Precision")
        print(nc.jofc.dice.wt.p[pert_i,imc,m_it])
        print("Recall")
        print(nc.jofc.dice.wt.r[pert_i,imc,m_it])
        print("F-measure")
        print(nc.jofc.dice.wt.f[pert_i,imc,m_it])
      } 
      
    }
    
    
  }
}




pdf("./graphs/plot-PRF2-enron.pdf")
colors.vec<-c( "red","blue","orange","green")
par(lty=1)

plot.graph.with.CI((nc.jofc.dice.wt.f[1,,]),plot.title="",plot.col=colors.vec[1],
                   conf.int=TRUE,add=FALSE,fp.points=n-m_vals,
                   customx.labels=NULL,customy.labels=NULL,
                   ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                   ylab="Average F-measure/Precision/Recall")
plot.graph.with.CI((nc.jofc.dice.wt.p[1,,]),plot.title="",plot.col=colors.vec[3],
                   conf.int=TRUE,add=TRUE,fp.points=n-m_vals,
                   customx.labels=NULL,customy.labels=NULL,
                   ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                   ylab="Average F-measure/Precision/Recall")
plot.graph.with.CI((nc.jofc.dice.wt.r[1,,]),plot.title="",plot.col=colors.vec[4],
                   conf.int=TRUE,add=T,fp.points=n-m_vals,
                   customx.labels=NULL,customy.labels=NULL,
                   ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                   ylab="Average F-measure/Precision/Recall")



legend.txt<- c("F-measure","Precision","Recall")
title("1-to-k matching o jofc")
abline(h=1/(m),lty=2) ### chance?  apparently not!?
abline(v=1/2,lty=2) ### chance?  gotta be!?
legend.txt<-c(legend.txt)
ltypes<- c(rep(1,3))
legend(x="topright",legend=legend.txt, col =colors.vec[c(1,3,4)],lty=ltypes)



dev.off()





pdf("./graphs/enron-F-measure_1_to_k_match.pdf")
colors.vec<-c( "red","blue","orange","green","magenta")
par(lty=1)

  #  par(lty=m_v_i)
  plot.graph.with.CI((nc.jofc.dice.wt.f[1,,]),plot.title="",plot.col=colors.vec[1],
                           conf.int=TRUE,add=FALSE,fp.points=n-m_vals,
                           customx.labels=NULL,customy.labels=NULL,
                           ispowercurve=FALSE,ylim=c(0,1),xlab="perturbation parameter",
                           ylab="Average F-measure")

legend.txt<- c()
title("1-to-k matching o jofc")
abline(h=1/(m),lty=2) ### chance?  apparently not!?
abline(v=1/2,lty=2) ### chance?  gotta be!?
#legend.txt<-c(legend.txt,n-m_vals)
#ltypes<- c(1:m_len)
#legend(x="topright",legend=legend.txt, col =colors.vec[1],lty=1,title="Num. of Hard Seeds")

dev.off()









pdf("./graphs/enron-F-measure_1_to_k_match.pdf")
colors.vec<-c( "red","blue","orange","green","magenta")
par(lty=1)
for (m_v_i in 1:m_len){
  #  par(lty=m_v_i)
  plot.graph.with.CI(t(nc.jofc.dice.wt.f[,,m_v_i]),plot.title="",plot.col=colors.vec[m_v_i],
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



