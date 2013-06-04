

source("./src/JOFC-graph-experiment-sim-fn.R")
  source("./lib/graph_embedding_fn.R")
  source("./lib/simulation_math_util_fn.R")
  source("./lib/smacofM.R")
  source("./lib/oosIM.R")
  source("./lib/diffusion_distance.R")

library(optmatch)
  library(igraph)
  library(MASS)
  library(MCMCpack)
  library(clue)
library(parallel)
library(doMC)

total_v<-253

n_vals_worm=c(seq(20,100,20),seq(125,200,25))






#n_vals_worm= c(20,50,100,150,200)

corr.matches.wt.dice.unwt<-worm_exp_par_sf(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = TRUE)
avg.corr.worm.wt.dice.unwt <- corr.matches.wt.dice.unwt/(total_v-n_vals_worm)

save.image(paste("JOFC-graph-worm",date(),".Rdata"))




