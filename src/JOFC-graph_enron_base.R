# Take arguments from shell script: POSSIBLY NOT WORKING ATM
graph_data <-"enron"

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
print(paste("running" , graph_data," with the parameter file ",paramsFile,sep=""))

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
n_vals=c(12:20,seq(21,49,3),seq(50,140,10))

num_iter <- 120

run.in.linux<- .Platform$OS.type=="unix"







corr.matches.e <- 
		enron_exp_par_sf_w(num_iter,n_vals,
                           embed.dim=starting.embed.dim,weighted.graph=graph.is.weighted,
				           diss_measure=diss_measure,symmetrize=symmetrize.graph,
				           preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE,
						T1=T1,T2=T2,sep.err.w = use.separability.error.terms ) 

sink()
sink()
   load("./data/AAA-187As-184x184.Rbin")
	Ac=AAA[[T1]]
	Ag=AAA[[T2]]
  
  sum_row_c = apply(Ac,1,sum)
  sum_col_c = apply(Ac,2,sum)
  sum_row_g = apply(Ag,1,sum)
  sum_col_g = apply(Ag,2,sum)
  
  disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))

total_v<-sum(!disc_v)

corr.results.unlist <- corr.matches.e
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())


save.image(paste("JOFC-graph-",graph_data,"_param",Sys.getenv("SGE_TASK_ID")," at ",Sys.Date(),as.character(ceiling(runif(1)*100)),".Rdata"))


library(R.matlab)

R.matlab::writeMat(paste("JOFC_enron_ECT",Sys.Date(),"-Params_",Sys.getenv("SGE_TASK_ID"),".mat",sep="")
,JOFC_corr_enron_undir=corr.matches.e,n_vals_enron=n_vals,total_v=total_v)












