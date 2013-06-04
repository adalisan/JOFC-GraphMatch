# TODO: Add comment
# 
# Author: Sancar
###############################################################################



source("./src/JOFC-graph-experiment-sim-fn.R")


T.param <- 2
n_vals_enron=c(12:20,seq(21,49,3),seq(50,140,10))

num_iter <- 120

run.in.linux<- .Platform$OS.type=="unix"






w.vals<-c(0.2,0.8,0.99)
w.vals<-0.8
embed.dim.start<- 8
w.max.index<-length(w.vals)
corr.matches.e <- 
		enron_exp_par_sf_w(num_iter,n_vals_enron,
embed.dim=embed.dim.start,weighted.graph=FALSE,
				diss_measure="diffusion",symmetrize=TRUE,
				preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) 

sink()
sink()
   load("./data/AAA-187As-184x184.Rbin")
	Ac=AAA[[130]]
	Ag=AAA[[131]]
  
  sum_row_c = apply(Ac,1,sum)
  sum_col_c = apply(Ac,2,sum)
  sum_row_g = apply(Ag,1,sum)
  sum_col_g = apply(Ag,2,sum)
  
  disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))

total_v<-sum(!disc_v)

corr.results.unlist <- corr.matches.e
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/num_iter
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_enron,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_enron,"/")

corr.results.avg.undir<- corr.results.avg
corr.results.sd.undir<- corr.results.sd


save.image(paste("JOFC-graph_enron_diff",date(),".Rdata"))
colors.vec<-rainbow(w.max.index)


w.i = 1
plot(n_vals_enron, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l",lwd=2)

if (w.i>1){
for(w.i in 2:w.max.index)
{
	lines(n_vals_enron, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[w.i],lwd=2)
}  
}
title("Undirected Enron Graph")


library(R.matlab)

R.matlab::writeMat("JOFC_enron_diff.mat",
JOFC_corr_enron_=corr.results.avg, 
                   
			JOFC_corr_enron_sd=corr.results.sd, 
                  n_vals_enron=n_vals_enron,total_v=total_v)








