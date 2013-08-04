

source("./src/JOFC-graph-experiment-sim-fn.R")
####################
# A smaller graph

while (sink.number()>0) {
	sink()
}

#Bitflip experiment


run.in.linux<- .Platform$OS.type=="unix"

n<-300
nmc <- 120

pert<- seq(0,0.5,0.1)
sep.err.w <-TRUE





n_vals<- c(seq(10,75,5),seq(80,190,10),seq(200,275,25))
#n_vals <-c(20,50)


#w.vals<- c(0.01,0.3,0.8,0.99)
w.vals<- 0.8
w.max.index<-length(w.vals)

npert<-length(pert)
  w.max.index<-length(w.vals)
  
  seq <- FALSE
  require(foreach)
  
  num.cores<-parallel::detectCores()
  iter_per_core <- ceiling(nmc/num.cores)
  
  num_iter<- iter_per_core*num.cores
 
  
  if(seq){
    registerDoSEQ()
  } else if (.Platform$OS.type != "windows" && require("multicore")) {
    require(doMC)
    registerDoMC()
  } else if (FALSE &&                     # doSMP is buggy
    require("doSMP")) {
    workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
    on.exit(stopWorkers(workers), add = TRUE)
    registerDoSMP(workers)
  } else if (require("doSNOW")) {
    cl <- snow::makeCluster(num.cores, type = "SOCK")
    on.exit(snow::stopCluster(cl), add = TRUE)
    registerDoSNOW(cl)
  } else {
    registerDoSEQ()
  }  







  corr.results.list<- list()

  corr.results.list<- foreach(i=1:num.cores, .combine="c",.export=c("bitflip_MC_rep","run.experiment.JOFC")) %dopar% {
    #	setwd('~/projects/DataFusion-graphmatch/')
    require(optmatch)
    require(igraph)
    require(MASS)
    require(MCMCpack)
    require(clue)
    source("./lib/graph_embedding_fn.R")
    source("./lib/simulation_math_util_fn.R")
    source("./lib/smacofM.R")
    source("./lib/oosIM.R")
    source("./lib/diffusion_distance.R")

    corr.results.mc <- try(bitflip_MC_rep (pert,n,n_vals,embed.dim=5,
			diss_measure="ECT",it.per.G=1,
			num_v_to_embed_at_a_time=1,w.vals=w.vals,sep.err.w=sep.err.w))
     if (!inherits(corr.results.mc , "try-error")){		
	corr.results.list<- c(corr.results.list,list(corr.results.mc))}
    else {nmc <- nmc-1}
}


nmc<- num_iter

corr.results.unlist<-Reduce("c",corr.results.list,init=list())
corr.results.avg<-Reduce("+",corr.results.unlist)/nmc
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")



save.image(paste("JOFC-graph_bitflip_ECT_sim_300_",date(),".Rdata"))