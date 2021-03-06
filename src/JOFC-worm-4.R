

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
library(foreach)
total_v<-253

n_vals_worm=c(seq(20,100,20),seq(125,200,25))







corr.matches.wt.dice.unwt.directed<-worm_exp_par_sf(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE)
avg.corr.worm.wt.dice.unwt.directed <- corr.matches.wt.dice.unwt.directed/(total_v-n_vals_worm)
save.image(paste("JOFC-graph-worm",date(),".Rdata"))










