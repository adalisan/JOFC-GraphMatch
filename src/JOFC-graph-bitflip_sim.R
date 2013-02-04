

source("./src/JOFC-graph-experiment-sim-fn.R")
####################
# A smaller graph

while (sink.number()>0) {
	sink()
}

#Bitflip experiment


run.in.linux<- .Platform$OS.type=="unix"

n<-100
nmc <- 20

pert<- seq(0,0.5,0.1)

n_vals<- c(seq(10,20,5),seq(30,90,10))
#n_vals <-c(20,50)
sep.err.w<- TRUE

w.vals<- c(0.01,0.3,0.8,0.99)
w.max.index<-length(w.vals)

corr.results.list<- list()

for (mc in 1:nmc){
	corr.results.mc <- try(bitflip_MC_rep (pert,n,n_vals,embed.dim=6,
			diss_measure="C_dice_weighted",it.per.G=1,
			num_v_to_embed_at_a_time=1,w.vals=w.vals,sep.err.w=sep.err.w))
     if (!inherits(corr.results.mc , "try-error")){
		
	corr.results.list<- c(corr.results.list,list(corr.results.mc))}
    else {nmc <- nmc-1}
}

corr.results.unlist<-Reduce("c",corr.results.list,init=list())
corr.results.avg<-Reduce("+",corr.results.unlist)/nmc
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")



save.image(paste("JOFC-graph_bitflip_sim_100_",date(),".Rdata"))