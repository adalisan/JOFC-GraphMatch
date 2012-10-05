

source("./src/JOFC-graph-experiment-sim-fn.R")
####################
# A smaller graph

while (sink.number()>0) {
  sink()
}

n<-100
nmc <- 20

pert<- seq(0,0.5,0.1)
#pert<- seq(0,0.4,0.1)
#pert<- 0.3
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







n_vals_worm=c(5, 8:20,seq(21,49,3),seq(50,200,10))

corr.matches.wt.exp.par<-worm_exp_par(num_iter=12,n_vals=n_vals_worm,embed.dim=2,
                              weighted.graph=TRUE,diss_measure="exp_minus")
#avg.corr.worm.wt.exp.par <- corr.matches.wt.exp.par$avg/(279-n_vals_worm)

corr.matches.wt.exp.par$agg


#test


n_vals_worm=c(150)

sp<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=100,
                               weighted.graph=TRUE,diss_measure="default")

corr.matches.wt.dice<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=10,
                               weighted.graph=FALSE,diss_measure="C_dice_weighted")
avg.corr.worm.wt.dice <- corr.matches.wt.dice/(279-n_vals_worm)



corr.matches.wt.expm<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=2,
                               weighted.graph=TRUE,diss_measure="exp_minus")
avg.corr.worm.wt.expm<- corr.matches.wt.expm/(279-n_vals_worm)


corr.matches.wt.diff<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=2,
                               weighted.graph=FALSE,diss_measure="diffusion" )
avg.corr.worm.wt.diff <- corr.matches.wt.diff/(279-n_vals_worm)

corr.matches.wt.diff<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=2,
                               weighted.graph=TRUE,diss_measure="diffusion")
avg.corr.worm.wt.diff <- corr.matches.wt.diff/(279-n_vals_worm)

corr.matches.wt.ect<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=2,
                               weighted.graph=FALSE,diss_measure="ECT")
avg.corr.worm.wt.ect <- corr.matches.wt.ect/(279-n_vals_worm)






n_vals_worm=c(5, 8:20,seq(21,49,3),seq(50,200,10))

corr.matches.wt.exp<-worm_exp(num_iter=5,n_vals=n_vals_worm,embed.dim=2,
					weighted.graph=TRUE,diss_measure="exp_minus")
avg.corr.worm.wt.exp <- corr.matches.wt.exp/(279-n_vals_worm)


corr.matches.wt.diff<-worm_exp(num_iter=15,n_vals=n_vals_worm,embed.dim=2,
					weighted.graph=FALSE,diss_measure="diffusion")
avg.corr.worm.wt.diff <- corr.matches.wt.diff/(279-n_vals_worm)



n_vals_worm=c(5, 8:20,seq(21,49,3),seq(50,200,10))


corr.matches.wt.diff<-worm_exp(num_iter=15,n_vals=n_vals_worm,embed.dim=2,
                               weighted.graph=TRUE,diss_measure="diffusion")
avg.corr.worm.wt.diff <- corr.matches.wt.diff/(279-n_vals_worm)



corr.matches.wt.ect<-worm_exp(num_iter=15,n_vals=n_vals_worm,embed.dim=2,
                              weighted.graph=TRUE,diss_measure="ECT")
avg.corr.worm.wt.ect <- corr.matches.wt.ect/(279-n_vals_worm)



corr.matches<-worm_exp(num_iter=15,n_vals=n_vals_worm,embed.dim=2,
                       weighted.graph=FALSE)
avg.corr.worm <- corr.matches/(279-n_vals_worm)





n_vals_enron=c(5, 8:20,seq(21,49,3),seq(50,160,10))
corr.matches.e<-enron_exp(num_iter=5,n_vals=n_vals_enron,embed.dim=2)








n_vals_worm=c(seq(20,100,20),seq(125,200,25))
n_vals_worm=c(20,50,100,150,200)

corr.matches.wt.dice<-worm_exp(num_iter=10,n_vals=n_vals_worm,embed.dim=10,
                               weighted.graph=TRUE,diss_measure="C_dice_weighted")
avg.corr.worm.wt.dice <- corr.matches.wt.dice/(253-n_vals_worm)




