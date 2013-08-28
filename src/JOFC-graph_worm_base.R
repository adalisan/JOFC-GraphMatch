# Take arguments from shell script: POSSIBLY NOT WORKING ATM
graph_data <-"worm"

# Author: Sancar
###############################################################################

library(optmatch)
  library(igraph)
  library(MASS)
  library(MCMCpack)
  library(clue)
library(parallel)
library(doMC)


args=(commandArgs(TRUE))

paramsFile=NULL
if(length(args)==0){
    print("No arguments supplied.")
    ##supply default values
    paramsFile= paste("./src/JOFC-graph_",graph_data,"_Params_")

}else{
    for(i in 1:length(args)){
         eval(parse(text=args[[i]]))
    }
}

#if the paramsFile was not set 
if (is.null (paramsFile))
  paramsFile= paste("./src/JOFC-graph_",graph_data,"_Params_1.R")


paramsFile = paste("./src/JOFC-graph_",graph_data,"_Params_",Sys.getenv("SGE_TASK_ID"),".R",sep="",collapse="")
print(paste("running" , graph_data," with the parameter file",sep=""))
print(paramsFile)
num.cpus <- parallel::detectCores()

source(paramsFile)
#print("The parameter values are")



source("./src/JOFC-graph-experiment-sim-fn.R")
  source("./lib/graph_embedding_fn.R")
  source("./lib/simulation_math_util_fn.R")
  source("./lib/smacofM.R")
  source("./lib/oosIM.R")
  source("./lib/diffusion_distance.R")



if (!exists("n_vals"))
n_vals=c(seq(20,100,20),seq(125,200,25))
num_iter <- 6
run.in.linux<- .Platform$OS.type=="unix"






corr.matches <-
        worm_exp_par_sf_w(num_iter=num_iter,n_vals=n_vals,
                            embed.dim=starting.embed.dim, weighted.graph=graph.is.weighted,
                            diss_measure=diss_measure,symmetrize = symmetrize.graph,
				preselected.seeds=NULL,preselected.test=NULL,w.vals=w.vals, seq=FALSE,
				sep.err.w=use.separability.error.terms)
avg.corr.worm <- corr.matches/(total_v-n_vals)

save.image(paste("JOFC-graph-",graph_data,"_param_",Sys.getenv("SGE_TASK_ID")
,".at.",Sys.Date(),as.character(ceiling(runif(1)*100)),".Rdata",sep=""))

library(R.matlab)

R.matlab::writeMat(paste("JOFC-graph-",graph_data,"_param",Sys.getenv("SGE_TASK_ID"),".mat" ,sep="") ,
JOFC_corr_worm=corr.matches,n_vals_worm=n_vals,total_v=total_v)








