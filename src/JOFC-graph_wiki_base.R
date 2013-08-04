# TODO: Add comment
# 
# Author: Sancar
###############################################################################

graph_data <-"wiki"
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
#print(params)


source("./src/JOFC-graph-experiment-sim-fn.R")
  source("./lib/graph_embedding_fn.R")
  source("./lib/simulation_math_util_fn.R")
  source("./lib/smacofM.R")
  source("./lib/oosIM.R")
  source("./lib/diffusion_distance.R")







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







if (!exists("n_vals"))
n_vals=c(seq(20,100,10),seq(125,225,25),seq(250,450,50),seq(500,1300,100))

num_iter <- 120

run.in.linux<- .Platform$OS.type=="unix"





w.max.index<-length(w.vals)
corr.matches.w <- 
		wiki_exp_par_sf_w(num_iter,n_vals,
        embed.dim=starting.embed.dim,weighted.graph=graph.is.weighted,
				diss_measure=diss_measure,symmetrize=symmetrize.graph,
				preselected.seeds=NULL,preselected.test=NULL,w.vals=w.vals, seq=FALSE, subset=subset,
                sep.err.w = use.separability.error.terms) 


corr.results.unlist <- corr.matches.w
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals,"/")
library(R.matlab)

R.matlab::writeMat("JOFC_wiki.mat",
JOFC_corr_wiki=corr.matches.w , 
              n_vals_wiki=n_vals,total_v=total_v)






