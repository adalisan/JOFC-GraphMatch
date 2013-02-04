

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





colors.vec <- c("red","green","aquamarine","purple",
		"darkblue","salmon","rosybrown","magenta","orange")
colors.vec[3]<-"gold4"
colors.vec[2]<-"darkblue"
colors.vec[4]<-"darkorange4"
colors.vec[9]<-"red"
colors.vec.len<-length(colors.vec)

require(RColorBrewer)
colors.vec.brew<- brewer.pal(colors.vec.len,"YlOrRd")

colors.vec.brew[colors.vec.len+1]<-"cornflowerblue"
colors.vec.brew[colors.vec.len+2]<-"azure3"
colors.vec.brew[colors.vec.len+3]<-"cyan"





colors.vec.len <- length(colors.vec.brew)
colors.vec.brew[(colors.vec.len-2):colors.vec.len] <- brewer.pal(3,"Set3")
#palette("YlOrRd")




w.i = 1
plot(n_vals, as.vector(corr.results.avg.frac[,1,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")

npert<-length(pert)
for(ipert in 2:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert])
}  



w.i=2
for(ipert in 1:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert],lty=2)
}  



w.i=3
for(ipert in 1:npert)
{
	lines(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert],lty=3)
}  
title("w.i=1:3 varying pert.param")



if (!run.in.linux) windows()


ipert<-2
w.i = 1
plot(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
		ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="p")

for(w.i in 2:w.max.index)
{
	points(n_vals, as.vector(corr.results.avg.frac[,ipert,w.i]) ,xlab="Hard seeds",
			ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[w.i])
}  
title("ipert=2 varying w.i")






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



corr.results.list.300<- list()

for (mc in 1:nmc){
	corr.results.mc <- bitflip_MC_rep (pert,n,n_vals,embed.dim=2,
			diss_measure="C_dice_weighted",it.per.G=1,
			num_v_to_embed_at_a_time=1,w.vals=w.vals)
	corr.results.list.300<- c(corr.results.list.300,list(corr.results.mc))
}

corr.results.unlist<-Reduce("c",corr.results.list.300,init=list())
corr.results.avg<-Reduce("+",corr.results.unlist)/nmc
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/nmc
corr.results.var<-corr.results.var/sqrt(nmc)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,n-n_vals,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,n-n_vals,"/")





total_v<-253

n_vals_worm=c(seq(20,100,20),seq(125,200,25))
w_vals_worm = c(0.01,0.3,0.5,0.65,0.75,0.8,0.85,0.9,0.95,0.99)
test_v = total_v-n_vals_worm
num_iter =24


corr.matches.wt.dice.unwt.directed.2<-worm_exp_par_sf_w(num_iter=num_iter,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,
		w.vals =  w_vals_worm)
#
#
#num.cores<-parallel::detectCores()
#
#corr.match.list<-list()
#
#result.len<-length(corr.matches.wt.dice.unwt.directed.2)
#result.per.core<- result.len/num.cores
#iter.per.core <- ceiling(num_iter/num.cores)
#for (core_it in 1:num.cores){
#  reshap.vec <- array(corr.matches.wt.dice.unwt.directed.2[(core_it-1)*(result.per.core)+(1:result.per.core)],dim=c(length(n_vals_worm),iter.per.core,w.max.index))
#  list.em <- list()
#  for (it_core_it in 1:iter.per.core)
#   list.em<- c(list.em,list(reshap.vec[,it_core_it,]))
#  corr.match.list<-c(corr.match.list,list.em)
#}
#


corr.results.unlist <- corr.matches.wt.dice.unwt.directed.2
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
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











w_vals_worm <- c(0.8)

num_iter <- 100

corr.matches.wt.dice.unwt.directed<-worm_exp_par_sf_w(num_iter=num_iter,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals= w_vals_worm 
)

corr.results.unlist <- corr.matches.wt.dice.unwt.directed
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed.2,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_worm,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_worm,"/")








corr.matches.wt.dice.unwt.undir<-worm_exp(num_iter=num_iter,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = TRUE)


corr.results.unlist <- corr.matches.wt.dice.unwt.undir
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.undir,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_worm,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_worm,"/")




corr.matches.wt.dice.unwt.directed<-worm_exp_par_sf_w(num_iter=num_iter,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=FALSE,diss_measure="C_dice_weighted",symmetrize = FALSE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals= w_vals_worm 
)

corr.results.unlist <- corr.matches.wt.dice.unwt.directed
#corr.results.unlist <-Reduce("c",corr.matches.wt.dice.unwt.directed,init=list())

corr.results.avg<-Reduce("+",corr.results.unlist)/num_iter
corr.results.var<-lapply(corr.results.unlist,function(x){(x-corr.results.avg)^2})
corr.results.var<-Reduce("+",corr.results.var)/num_iter
corr.results.var<-corr.results.var/sqrt(num_iter)
corr.results.sd<-sqrt(corr.results.var)

corr.results.avg.frac <- sweep( corr.results.avg,1,total_v-n_vals_worm,"/")
corr.results.sd.frac <- sweep( corr.results.sd,1,total_v-n_vals_worm,"/")






corr.matches.wt.dice.directed<-worm_exp(num_iter=num_iter,n_vals=n_vals_worm,embed.dim=10,
		weighted.graph=TRUE,diss_measure="C_dice_weighted",symmetrize = FALSE)
avg.corr.worm.wt.dice.directed <- corr.matches.wt.dice.directed/(total_v-n_vals_worm)






n_vals_enron=c(5, 8:20,seq(21,49,3),seq(50,160,10))
corr.matches.e<-enron_exp(num_iter=55,n_vals=n_vals_enron,embed.dim=5)



