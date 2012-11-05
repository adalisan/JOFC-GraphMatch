
run.experiment.JOFC<-function(G,Gp,n_vals,num_iter,embed.dim,diss_measure="default",
                              graph.is.weighted=FALSE,graph.is.directed=FALSE,...){
  
  
  matched.cost<-0.01
  N<-nrow(G)
  corr.matches =matrix(0,length(n_vals),num_iter)
  
  
  for (n_v_i in 1:length(n_vals)){
    n_v = n_vals[n_v_i]
    print("n is ")
    print(n_v)
    for (it in 1:num_iter){
      print("iteration")
      print(it)
      
      init.time <- proc.time()
      #insample_logic_vec <- 1:N %in% 1:n_v
      insample_logic_vec <- 1:N %in% sample(1:N,n_v,replace=FALSE)
      insample_logic_vec <- c(insample_logic_vec,insample_logic_vec)
      
      num_v_to_embed_at_a_time = 1
      jofc.result<- 
	#try(
	JOFC.graph.custom.dist(G,Gp,in.sample.ind=insample_logic_vec, 
                                               d.dim=embed.dim, w.vals.vec=0.5, graph.is.directed= graph.is.directed, 
                                               vert_diss_measure=diss_measure,  T.param  =  2,
											   
                                              ...)
      #)
      
      if (inherits(jofc.result,"try-error")) {
        print('Skipping iteration')
        corr.matches[n_v_i,it] <- NA
        next}
      
      jofc.res.1<-jofc.result[[1]]
      
      #M.result.1<-try(solveMarriage(jofc.res.1))
      test.m <- sum(!insample_logic_vec)/2
     
      
      rownames(jofc.res.1) <- 1:test.m
      colnames(jofc.res.1) <- 1:test.m
      M.result.1<- solve_LSAP(jofc.res.1)
      NumofTruePairing.1 <- sum(as.matrix(M.result.1)==1:test.m)
      print(paste(NumofTruePairing.1," out of ", test.m,sep="",collapse=""))
      
      end.time <- proc.time()
      
      print(paste("run took ", end.time[2]-init.time[2] ," s ",sep="",collapse =""))
      print(paste("(usertime )run took ", end.time[1]-init.time[1] ," s ",sep="",collapse =""))

      if (inherits(M.result.1,"try-error"))    {  
        print('Skipping iteration')
        next}
      
      #NumofTruePairing.1<-present(M.result.1)
      corr.matches[n_v_i,it] = NumofTruePairing.1
    }
  }
  #   save( list=c("corr.matches"),
  #         file=paste("./logs/JOFC_graph",Sys.time(),floor(runif(n=1,max=100))), 
  #         ascii=TRUE)
  return(corr.matches)
}

bitflip_MC_rep <- function (pert,n,n_vals,embed.dim,diss_measure,it.per.G=1, 
                            num_v_to_embed_at_a_time=NULL){
  require(optmatch)
  require(igraph)
  require(MASS)
  require(MCMCpack)
  require(clue)
  source("./lib/graph_embedding_fn.R")
  source("./lib/simulation_math_util_fn.R")
  source("./lib/smacofM.R")
  source("./lib/oosIM.R")
  source("./lib/diffusion_distance.R")
  
  npert<-length(pert)
  
  
  corr.match.array.mc<-array(0,dim=c(length(n_vals),npert))
  
  for(ipert in 1:npert)
  {
    G<-ER(n,0.5)
    Y.emb<-NULL
    Gp<-bitflip(G ,pert[ipert],pert[ipert])
    corr.matches<-run.experiment.JOFC(G,Gp,n_vals,num_iter=it.per.G,
                                      embed.dim=embed.dim, diss_measure=diss_measure
                                     
    )
    corr.match.array.mc[,ipert] <- corr.matches
  }
  return (corr.match.array.mc)
}

bitflip_exp<-function (nmc,pert,n,n_vals,embed.dim=6)
{
  npert<-length(pert)
  corr.match.array<-array(0,dim=c(length(n_vals),nmc,npert))
  corr.match.avg<-array(0,dim=c(length(n_vals),npert))
  
  seed<-123
  set.seed(seed)
  sfInit( parallel=TRUE)
  print(sfCpus())
  corr.match.list <- sfLApply(1:nmc,bitflip_MC_rep,pert,n,n_vals,embed.dim)
  
  sfStop()
  for (t in 1:length(corr.match.list))
    corr.match.array[,t,] <- corr.match.list[[t]]
  
  
  corr.match.array<-sweep(corr.match.array, 1, n-n_vals, "/")
  print(corr.match.array)
  print(dim(corr.match.array))
  colors.vec<-c( "red","blue","orange","green")
  for (i in 1:length(n_vals)) 
    for (j in 1:npert){
      corr.match.avg[i,j] <- mean(corr.match.array[i,,j])
    }
  
  #windows()
  plot(n_vals, as.vector(corr.match.avg[,1]) ,xlab="Hard seeds",
       ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")
  
  for(ipert in 2:npert)
  {
    lines(n_vals, as.vector(corr.match.avg[,ipert]) ,xlab="Hard seeds",ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert])
  }  
  
  return(corr.match.avg)
}

wiki_exp <- function(num_iter,n_vals,embed.dim=13) {
  #Not yet implemented
  load ("./data/wiki.RData")
  corr.matches<-run.experiment.JOFC(GE,GF,n_vals,num_iter=num_iter,
                                    embed.dim, diss_measure="default")
  
}	

worm_exp_par <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,diss_measure="default") {
  
  load("./data/celegansGraph.Rd")
  if (weighted.graph){
    scale_f <- lm(as.vector(Ac) ~ as.vector(Ag) + 0)$coefficients
    Ac_graph <- Ac
    Ag_graph <- scale_f*Ag
    
    
    #symmetrize
    Ac_graph <- (Ac_graph+t(Ac_graph))/2
    Ag_graph <- (Ag_graph+t(Ag_graph))/2
  } else{
    
    Ac_graph<- Ac>0
    Ag_graph<- Ag>0
  }
  num.cores<-parallel::detectCores()
  iter_per_core <- ceiling(num_iter/num.cores)
  require(foreach)
  

  if (.Platform$OS.type != "windows" && require("multicore")) {
  require(doMC)
    registerDoMC()
  } else if (FALSE &&                     # doSMP is buggy
           require("doSMP")) {
  workers <- startWorkers(num.cores) # My computer has 4 cores
       on.exit(stopWorkers(w), add = TRUE)
    registerDoSMP(w)
} else if (require("doSNOW")) {
    cl <- snow::makeCluster(num.cores, type = "SOCK")
    on.exit(snow::stopCluster(cl), add = TRUE)
    registerDoSNOW(cl)
    } else {
    registerDoSEQ()
}  

    
 
  corr_match_list<- foreach(i=1:num.cores, .combine="cbind",.export="run.experiment.JOFC") %dopar% {
    setwd('~/projects/DataFusion-graphmatch/')
    require(optmatch)
    require(igraph)
    require(MASS)
    require(MCMCpack)
    source("./lib/graph_embedding_fn.R")
    source("./lib/simulation_math_util_fn.R")
    source("./lib/smacofM.R")
    source("./lib/oosIM.R")
    source("./lib/diffusion_distance.R")
    corr.matches<-run.experiment.JOFC(Ac_graph,Ag_graph,n_vals,num_iter=iter_per_core,
                                      embed.dim,diss_measure=diss_measure, graph.is.weighted=weighted.graph)
  }
  
  #corr.results.avg <- array(0, dim( corr_match_list[[1]]))
  #for (corr.results in corr_match_list){
  #   corr.results.avg <- corr.results.avg+corr.results
  #}  
  #corr.results.avg <- corr.results.avg/length( corr_match_list)
  return (list(agg=corr_match_list ))
}  

worm_exp_par_sf <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,diss_measure="default",symmetrize=TRUE) {
	load("./data/celegansGraph.Rd")
	
	
	sum_row_c = apply(Ac,1,sum)
	sum_col_c = apply(Ac,2,sum)
	sum_row_g = apply(Ag,1,sum)
	sum_col_g = apply(Ag,2,sum)
	
	disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
	Ac <- Ac[!disc_v,!disc_v]
	Ag <- Ag[!disc_v,!disc_v]
	graph.is.directed <- TRUE
	if (weighted.graph){
		
		scale_f <- lm(as.vector(Ac) ~ as.vector(Ag) + 0)$coefficients
		Ac_graph <- Ac
		Ag_graph <- scale_f*Ag
		
		
		#symmetrize
		if (symmetrize){
			graph.is.directed <- FALSE
			Ac_graph <- (Ac_graph+t(Ac_graph))/2
			Ag_graph <- (Ag_graph+t(Ag_graph))/2
		}
	} else{
		if (symmetrize){
			graph.is.directed <- FALSE
			Ac_graph <- (Ac+t(Ac))/2
			Ag_graph <- (Ag+t(Ag))/2
		}
		Ac_graph<- (Ac_graph>0)
		Ag_graph<- (Ag_graph>0)
	}
	num.cores<-parallel::detectCores()
	iter_per_core <- ceiling(num_iter/num.cores)
	
	  num.cores<-parallel::detectCores()
  iter_per_core <- ceiling(num_iter/num.cores)
  require(foreach)


  if (.Platform$OS.type != "windows" && require("multicore")) {
  require(doMC)
    registerDoMC()
  } else if (FALSE &&                     # doSMP is buggy
           require("doSMP")) {
	workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
       on.exit(stopWorkers(w), add = TRUE)
    registerDoSMP(w)
} else if (require("doSNOW")) {
    cl <- snow::makeCluster(num.cores, type = "SOCK")
    on.exit(snow::stopCluster(cl), add = TRUE)
    registerDoSNOW(cl)
    } else {
    registerDoSEQ()
}  

	
	corr_match_list<- foreach(i=1:num.cores, .combine="cbind",.export="run.experiment.JOFC") %dopar% {
		setwd('~/projects/DataFusion-graphmatch/')
		require(optmatch)
		require(igraph)
		require(MASS)
		require(MCMCpack)
		require(clue)
		source("./lib/graph_embedding_fn.R")
		source("./lib/simulation_math_util_fn.R")
		source("./lib/smacofM.R")
		source("./lib/oosIM.R")
		source("./lib/diffusion_distance.R")
#		
		corr.matches<-run.experiment.JOFC(Ac_graph,Ag_graph,n_vals,num_iter=iter_per_core,
				embed.dim,diss_measure=diss_measure,
				
				graph.is.weighted=weighted.graph,
				graph.is.directed= graph.is.directed 
		)
		
		corr.matches
	}
	

	print (str(corr_match_list))
	
	
	return (corr_match_list )
}


worm_exp <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,diss_measure="default",symmetrize=TRUE) {
  
  load("./data/celegansGraph.Rd")
	sum_row_c = apply(Ac,1,sum)
	sum_col_c = apply(Ac,2,sum)
	sum_row_g = apply(Ag,1,sum)
	sum_col_g = apply(Ag,2,sum)

      disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
      Ac <- Ac[!disc_v,!disc_v]
      Ag <- Ag[!disc_v,!disc_v]
	  graph.is.directed <- TRUE
  if (weighted.graph){
	
    scale_f <- lm(as.vector(Ac) ~ as.vector(Ag) + 0)$coefficients
    Ac_graph <- Ac
    Ag_graph <- scale_f*Ag
    
   
 
    
    #symmetrize
    if (symmetrize){
		graph.is.directed <- FALSE
    Ac_graph <- (Ac_graph+t(Ac_graph))/2
    Ag_graph <- (Ag_graph+t(Ag_graph))/2
    }
  } else{
     if (symmetrize){
		 graph.is.directed <- FALSE
    Ac_graph <- (Ac+t(Ac))/2
    Ag_graph <- (Ag+t(Ag))/2
}
    Ac_graph<- (Ac_graph>0)
    Ag_graph<- (Ag_graph>0)
  }
  
  corr.matches<-run.experiment.JOFC(Ac_graph,Ag_graph,n_vals,num_iter=num_iter,
                                    embed.dim,diss_measure=diss_measure,
						                       
                                    graph.is.weighted=weighted.graph,
                                   graph.is.directed= graph.is.directed 
						)
  
  return (corr.matches)
}



enron_exp <- function (num_iter,n_vals_vec,embed.dim=2){
  #Not yet implemented
  load("./data/AAA-187As-184x184.Rbin")
  corr.matches<-run.experiment.JOFC(AAA[[130]],AAA[[131]],
                                    n_vals_vec,num_iter=num_iter,embed.dim=embed.dim,
                                    diss_measure="default")
  
}








