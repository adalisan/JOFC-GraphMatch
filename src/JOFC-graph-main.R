
####################
# A smaller graph

n<-100
nmc <- 100

#pert<- seq(0,0.5,0.1)
pert<- seq(0,0.4,0.1)
n_vals<- c(seq(3,20,2),seq(20,95,5))

corr.results.list<- list()

for (mc in 1:nmc){
  corr.results.mc <- bitflip_MC_rep (pert,n,n_vals,embed.dim=1,
                                     diss_measure="default",it.per.G=1,
                                     num_v_to_embed_at_a_time=1)
  corr.results.list<- c(corr.results.list,list(corr.results.mc))
}





corr.results.avg <- array(0, dim( corr.results.list[[1]]))
for (corr.results in corr.results.list){
  corr.results.avg <- corr.results.avg+corr.results
}  
corr.results.avg <- corr.results.avg/length( corr.results.list)




n<-300
nmc <- 100

pert<- seq(0,0.5,0.1)
n_vals<- c(seq(3,20,2),seq(20,95,5))

corr.results.list<- list()

for (mc in 1:nmc){
  corr.results.mc <-  bitflip_MC_rep (pert,n,n_vals,embed.dim=2,
                                      diss_measure="default",it.per.G=1,
                                      num_v_to_embed_at_a_time=1)
  corr.results.list<- c(corr.results.list,list(corr.results.mc))
}





corr.results.avg <- array(0, dim( corr.results.list[[1]]))
for (corr.results in corr.results.list){
  corr.results.avg <- corr.results.avg+corr.results
}  
corr.results.avg <- corr.results.avg/length( corr.results.list)


