# TODO: Add comment
# 
# Author: Sancar
###############################################################################



source("./src/JOFC-graph-experiment-sim-fn.R")

n_vals_enron=c(5, 8:20,seq(21,49,3),seq(50,140,10))

num_iter <- 120

run.in.linux<- .Platform$OS.type=="unix"






w.vals<-c(0.2,0.8,0.99)
w.vals<-0.8
embed.dim.start<- 8
w.max.index<-length(w.vals)

diss_meas_list <- c("default","ell1","jaccard",'invlogweighted')
for (diss_meas in diss_meas_list){
corr.matches.e <- 
		try(
               enron_exp_par_sf_w(num_iter,n_vals_enron,
                embed.dim=embed.dim.start,weighted.graph=FALSE,
				diss_measure=diss_meas,symmetrize=TRUE,
				preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) 
		)
  if (inherits(corr.matches.e , "try-error")) next
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


save.image(paste("JOFC-graph_enron_",diss_meas,date(),".Rdata"))
}
colors.vec<-rainbow(w.max.index)

rm(AAA)


for (diss_meas in diss_meas_list){
corr.matches.e.dir <- 
		enron_exp_par_sf_w(num_iter,n_vals_enron,embed.dim=embed.dim.start,weighted.graph=FALSE,
				diss_measure=diss_meas,symmetrize=FALSE,
				preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) 


corr.results.unlist.dir <- corr.matches.e.dir

corr.results.avg.dir<-Reduce("+",corr.results.unlist.dir)/num_iter
corr.results.var.dir<-lapply(corr.results.unlist.dir,function(x){(x-corr.results.avg.dir)^2})
corr.results.var.dir<-Reduce("+",corr.results.var.dir)/num_iter
corr.results.var.dir<-corr.results.var.dir/num_iter
corr.results.sd.dir<-sqrt(corr.results.var.dir)

corr.results.avg.frac.dir <- sweep( corr.results.avg.dir,1,total_v-n_vals_enron,"/")
corr.results.sd.frac.dir <- sweep( corr.results.sd.dir,1,total_v-n_vals_enron,"/")

save.image(paste("JOFC-graph_enron_",diss_meas,"_dir_undir",date(),".Rdata")) 
}










