# TODO: Add comment
# 
# Author: Sancar
###############################################################################

load("./data/Wiki_orig.RData")
  Ac=AG_wiki_en_mat
  Ag=AG_wiki_fr_mat
  
  sum_row_c = apply(Ac,1,sum)
  sum_col_c = apply(Ac,2,sum)
  sum_row_g = apply(Ag,1,sum)
  sum_col_g = apply(Ag,2,sum)
  
  disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))

total_v<-sum(!disc_v)
# Of 1382 vertices only  total_v=1323  are not isolated vertices in BOTH graphs.
#however, The isolated vertices are not removed  for Wiki graph 
sink("wiki_v_count.txt")
print(total_v)
print("unconnected vertices ignored")
sink()
rm(list=(ls()[!(ls()=="total_v")]))




source("./src/JOFC-graph-experiment-sim-fn.R")

n_vals_wiki=c(seq(20,100,10),seq(125,225,25),seq(250,450,50),seq(500,1300,100))

num_iter <- 120

run.in.linux<- .Platform$OS.type=="unix"






w.vals<- 0.8
embed.dim.start<- 8
w.max.index<-length(w.vals)
corr.matches.w <- 
		wiki_exp_par_sf_w(num_iter,n_vals_wiki,
embed.dim=embed.dim.start,weighted.graph=FALSE,
				diss_measure="C_dice_weighted",symmetrize=TRUE,
				preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) 

total_v <- 1382
corr.results.unlist <- corr.matches.w
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_wiki,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_wiki,"/")

colors.vec<-rainbow(w.max.index)

w.i = 1
plot(n_vals_wiki, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")

if (w.i>1){
for(w.i in 2:w.max.index)
{
	lines(n_vals_wiki, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[w.i])
}  
}
title("Undirected Wiki G")

lines(n_vals_wiki,1/(total_v-n_vals_wiki),lty=3,col="black",lwd=2)


corr.matches.w.dir <- 
		wiki_exp_par_sf_w(num_iter,n_vals_wiki,embed.dim=3,weighted.graph=FALSE,
				diss_measure="C_dice_weighted",symmetrize=FALSE,
				preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) 

total_v <- 1382
corr.results.unlist.dir <- corr.matches.w.dir
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg.dir<-Reduce("+",corr.results.unlist.dir)/num_iter
corr.results.var.dir<-lapply(corr.results.unlist.dir,function(x){(x-corr.results.avg.dir)^2})
corr.results.var.dir<-Reduce("+",corr.results.var.dir)/num_iter
corr.results.var.dir<-corr.results.var.dir/num_iter
corr.results.sd.dir<-sqrt(corr.results.var.dir)

corr.results.avg.frac.dir <- sweep( corr.results.avg.dir,1,total_v-n_vals_wiki,"/")
corr.results.sd.frac.dir <- sweep( corr.results.sd.dir,1,total_v-n_vals_wiki,"/")

save.image(paste("JOFC-graph_wiki_C_dice_dir_undir",date(),".Rdata")) 


w.i = 1
lines(n_vals_wiki, as.vector(corr.results.avg.frac.dir[,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l",lty=2)

if (w.i>1){
for(w.i in 2:w.max.index)
{
	lines(n_vals_wiki, as.vector(corr.results.avg.frac.dir[,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[w.i])
}  
}
title("Directed Wiki Graph")

library(R.matlab)

R.matlab::writeMat("JOFC_wiki.mat",
JOFC_corr_wiki_undir=corr.results.avg.undir, 
                   JOFC_corr_e_dir=corr.results.avg.dir, 
			JOFC_corr_wiki_sd_undir=corr.results.sd.undir, 
                   JOFC_corr_wiki_sd_dir=corr.results.sd.dir,n_vals_wiki=n_vals_wiki,total_v=total_v)






