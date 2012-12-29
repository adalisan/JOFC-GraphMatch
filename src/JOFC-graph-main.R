

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
  corr.results.mc <- bitflip_MC_rep (pert,n,n_vals,embed.dim=2,
                                     diss_measure="C_dice_weighted",it.per.G=1,
                                     num_v_to_embed_at_a_time=1,w.vals=c(0.01,0.8))
  corr.results.list<- c(corr.results.list,list(corr.results.mc))
}

corr.results.unlist<-Reduce("c",corr.results.list,init=list())
corr.results.avg<-Reduce("+",corr.results.unlist)/nmc
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/sqrt(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")



w.i = 1
plot(n_vals, as.vector(corr.results.avg.frac[,1,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")

npert<-length(pert)
for(ipert in 2:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert])
}  
title("w.i=1 varying pert.param")


w.i=2
for(ipert in 1:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert],lty=2)
}  
title("w.i=2 varying pert.param")





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

n_vals< -  c(seq(15,20,5),seq(20,90,10))
corr.results.list.300<- list()

for (mc in 1:nmc){
  corr.results.mc <-  bitflip_MC_rep (pert,n,n_vals,embed.dim=10,
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
w_vals_worm = c(0.01,0.3,0.5,0.65,0.75,0.8,0.85,0.9,0.95,0.99)




corr.matches.wt.dice.unwt.directed.2<-worm_exp_par_sf_w(num_iter=24,n_vals=n_vals_worm,embed.dim=10,
                                                        weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
                                                        preselected.seeds=NULL,preselected.test=NULL,
                                                        w.vals=  w_vals_worm)

avg.corr.worm.unwt.directed.2<- corr.matches.wt.dice.unwt.directed.2/(total_v-n_vals_worm)

corr.match.list<-list()
for (core_it in 1:8){
  reshap.vec <- array(corr.matches.wt.dice.unwt.directed.2[(core_it-1)*270+(1:270)],dim=c(length(n_vals_worm),3,w.max.index))
  
  list.em <- list(reshap.vec[,1,],reshap.vec[,2,],reshap.vec[,3,])
  corr.match.list<-c(corr.match.list,list.em)
}




corr.results.unlist<-Reduce("c",corr.match.list,init=list())
corr.results.avg<-Reduce("+",corr.match.list)/24
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/24
corr.results.var<-corr.results.var/sqrt(24)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_worm,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_worm,"/")




w.i = 1
plot(n_vals_worm, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
     ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")


for(w.i in 2:w.max.index)
{
  lines(n_vals_worm, as.vector(corr.results.avg.frac[,w.i]) ,xlab="Hard seeds",
        ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[w.i])
}  
title("varying w.val")







corr.matches.wt.dice.unwt.directed.2<-worm_exp_par_sf_w(num_iter=16,n_vals=n_vals_worm,embed.dim=10,
                                                        weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
                                                        preselected.seeds=NULL,preselected.test=NULL,w.vals=  c(0.3,0.5)#c(0.3,0.5,0.65,0.75,0.8,0.85,0.9,0.95)
)



corr.matches.wt.dice.unwt<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
                                    weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = TRUE)
avg.corr.worm.wt.dice.unwt <- corr.matches.wt.dice.unwt/(total_v-n_vals_worm)



corr.matches.wt.dice.directed<-worm_exp(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=TRUE,diss_measure="C_dice_weighted",symmetrize = FALSE)
avg.corr.worm.wt.dice.directed <- corr.matches.wt.dice.directed/(total_v-n_vals_worm)


n_vals_worm=c(seq(20,100,20),seq(125,200,25))


corr.matches.wt.dice.unwt.directed<-worm_exp_par_sf_w(num_iter=20,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals= 0.8
)



n_vals_worm <- c(100)
corr.matches.wt.dice.unwt.directed.2<-worm_exp_par_sf_w(num_iter=100,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals=  c(0.01,0.1,0.3,0.5,0.65,0.75,0.85,0.9,0.95,0.99)
)


n_vals_worm <- c(15)
corr.matches.wt.dice.unwt.directed.2<-worm_exp_par_sf_w(num_iter=2,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals=  c(0.1)#,0.5,0.65,0.75,0.8,0.85,0.9,0.95)
)



avg.corr.worm.wt.dice.unwt.directed <- corr.matches.wt.dice.unwt.directed/(total_v-n_vals_worm)






test_1<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=10,
                                             weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
                                             preselected.seeds=c(5,10,15,50,60,70,80,90,100))


test_2<-worm_exp(num_iter=1,n_vals=n_vals_worm,embed.dim=10,
                                             weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
                                             preselected.seeds=c(5,10,15,50,60,70,80,90,100),
                                             preselected.test=c(11:14,16:49,51:59,71:81,101:200))


avg.corr.worm.wt.dice.unwt.directed <- corr.matches.wt.dice.unwt.directed/(total_v-n_vals_worm)




n_vals_enron=c(5, 8:20,seq(21,49,3),seq(50,160,10))
corr.matches.e<-enron_exp(num_iter=55,n_vals=n_vals_enron,embed.dim=5)



