

source("./src/JOFC-graph-experiment-sim-fn.R")
####################
# A smaller graph

while (sink.number()>0) {
  sink()
}

#Bitflip experiment

n<-100
nmc <- 50

pert<- seq(0,0.5,0.1)

n_vals<- c(seq(10,20,5),seq(20,90,10))
#n_vals <-c(20,50)
corr.results.list<- list()

for (mc in 1:nmc){
  corr.results.mc <- bitflip_MC_rep (pert,n,n_vals,embed.dim=12,
                                     diss_measure="default",it.per.G=1,
                                     num_v_to_embed_at_a_time=1)
  corr.results.list<- c(corr.results.list,list(corr.results.mc))
}




corr.results.avg <- array(0, dim( corr.results.list[[1]]))
corr.results.agg<-  array(0, c(dim( corr.results.list[[1]]),20))
i<-1
for (corr.results in corr.results.list){
  corr.results.avg <- corr.results.avg+corr.results
  corr.results.agg[,,i] <- corr.results
  i <- i+1
}  
corr.results.avg <- corr.results.avg/length( corr.results.list)


#Bitflip experiment

n<-300
nmc <- 50

pert<- seq(0,0.5,0.1)
n_vals<- c(seq(15,20,5),seq(20,90,10),seq(100,250,25))

corr.results.list.300<- list()

for (mc in 1:nmc){
  corr.results.mc <-  bitflip_MC_rep (pert,n,n_vals,embed.dim=15,
                                      diss_measure="default",it.per.G=1,
                                      num_v_to_embed_at_a_time=1)
  corr.results.list.300<- c(corr.results.list.300,list(corr.results.mc))
}





corr.results.avg <- array(0, dim( corr.results.list[[1]]))
for (corr.results in corr.results.list){
  corr.results.avg <- corr.results.avg+corr.results
}  
corr.results.avg <- corr.results.avg/length( corr.results.list)





total_v<-253

n_vals_worm=c(seq(20,100,20),seq(125,200,25))





corr.matches.wt.dice<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
                               weighted.graph=TRUE,diss_measure="C_dice_weighted",symmetrize = TRUE)
avg.corr.worm.wt.dice <- corr.matches.wt.dice/(total_v-n_vals_worm)





corr.matches.wt.dice.unwt<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
                                    weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = TRUE)
avg.corr.worm.wt.dice.unwt <- corr.matches.wt.dice.unwt/(total_v-n_vals_worm)



corr.matches.wt.dice.directed<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=TRUE,diss_measure="C_dice_weighted",symmetrize = FALSE)
avg.corr.worm.wt.dice.directed <- corr.matches.wt.dice.directed/(total_v-n_vals_worm)





corr.matches.wt.dice.unwt.directed<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE)
avg.corr.worm.wt.dice.unwt.directed <- corr.matches.wt.dice.unwt.directed/(total_v-n_vals_worm)









n_vals_enron=c(5, 8:20,seq(21,49,3),seq(50,160,10))
corr.matches.e<-enron_exp(num_iter=55,n_vals=n_vals_enron,embed.dim=5)



