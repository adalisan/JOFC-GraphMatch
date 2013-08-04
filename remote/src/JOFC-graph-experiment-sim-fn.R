
run.experiment.JOFC <- function(G,Gp,n_vals,num_iter,embed.dim,diss_measure='C_dice_weighted',
                              graph.is.weighted=FALSE,graph.is.directed=FALSE, preselected.seeds=NULL,
                              preselected.test= NULL,w.vals,num_v_to_embed_at_a_time = 1,  
                              return.list=FALSE,  
                              ...){
  
  print("run.experiment.JOFC start")
  matched.cost<-0.01
  
  N<-nrow(G)
  corr.matches =array(0,dim=c(length(n_vals),num_iter,length(w.vals)))
  G.1<-G
  G.2<-Gp
  w.max.index<-length(w.vals)
  corr.match.list<- list()
  
  #If list of preselected.seeds is given (is not NULL)
  # number of seeds is equal to the length of preselected.seeds
  #If both preselected.seeds and preselected.test are given (are not NULL)
  #select the rows and columns of adjacency/weight matrices  preselected.seeds and preselected.test
  #to get the graph only consisting of  vertices only in preselected.seeds and preselected.test
  if (!is.null(preselected.seeds)){
    n_vals <- length(preselected.seeds)
 
  }
  
  for (n_v_i in 1:length(n_vals)){
    n_v = n_vals[n_v_i]
    print("n is ")
    print(n_v)
    for (it in 1:num_iter){
      print("iteration")
      print(it)
      
      init.time <- proc.time()
      if (!is.null(preselected.seeds)){
        insample_logic_vec <- 1:N %in% preselected.seeds
        n_v = length(preselected.seeds)
      } else {
        
        #insample_logic_vec <- 1:N %in% 1:n_v
        insample_logic_vec <- 1:N %in% sample(1:N,n_v,replace=FALSE)
      }
      
      insample_logic_vec <- c(insample_logic_vec,insample_logic_vec)
      
  
      jofc.result<- 
        #try(
        JOFC.graph.custom.dist(G.1,G.2,in.sample.ind=insample_logic_vec, 
                               d.dim=embed.dim, w.vals.vec=w.vals, graph.is.directed= graph.is.directed, 
                               vert_diss_measure=diss_measure,  T.param  =  2,num_v_to_embed_at_a_time = num_v_to_embed_at_a_time,
							   
							   ...)
      #)
      
      if (inherits(jofc.result,"try-error")) {
        print('Skipping iteration')
        corr.matches[n_v_i,it,1:w.max.index] <- NA
        next}
     for (l in 1:w.max.index){ 
      jofc.res.l<-jofc.result[[l]]
      
      #M.result.1<-try(solveMarriage(jofc.res.1))
      test.m <- sum(!insample_logic_vec)/2
      
      
      rownames(jofc.res.l) <- 1:test.m
      colnames(jofc.res.l) <- 1:test.m
      M.result.l<- solve_LSAP(jofc.res.l)
      num_matched_v<-test.m
      if (!is.null(preselected.test)){
        corr_matched_v <-as.matrix(M.result.l)==1:test.m
        preselected.log<-rep(F,N)
        preselected.log[preselected.test]<-T
        preselected.new.index <- which(preselected.log[-preselected.seeds])
        
        NumofTruePairing.l <- sum(corr_matched_v[preselected.new.index])
        num_matched_v <- length(preselected.test)
      }
      else{
      
      
      NumofTruePairing.l <- sum(as.matrix(M.result.l)==1:test.m)
      }
      
      print(paste(NumofTruePairing.l," out of ", num_matched_v,sep="",collapse=""))
      
      end.time <- proc.time()
      
      print(paste("run took ", end.time[2]-init.time[2] ," s ",sep="",collapse =""))
      print(paste("(usertime )run took ", end.time[1]-init.time[1] ," s ",sep="",collapse =""))
      
      if (inherits(M.result.l,"try-error"))    {  
        print('Skipping iteration')
        next}
      
      #NumofTruePairing.1<-present(M.result.1)
      corr.matches[n_v_i,it,l] = NumofTruePairing.l
     }
    }
	
	#
	
  }
  
  for (it  in 1:num_iter)
  corr.match.list <-c(corr.match.list,
                      list(array(drop(corr.matches[,it,]),
                                 dim=c(length(n_vals),w.max.index))))
  print(str(corr.match.list))
  dimnames(corr.matches)
  print("corr.matches")
  print(head(corr.matches))
  print(str(corr.matches))
    # save( list=c("corr.matches"),
    #       file=paste("./logs/JOFC_graph",format(Sys.time(), "%b_%d_%Y"),floor(runif(n=1,max=100)),".RData",collapse=""), 
    #      ascii=TRUE) 
  if (return.list) {
  return(corr.match.list)   
  } else{
    return(corr.matches)
  }
  
}

bitflip_MC_rep <- function (pert,n,n_vals,embed.dim,diss_measure, it.per.G=1, 
                            num_v_to_embed_at_a_time=NULL,w.vals,sep.err.w=TRUE){
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
  w.max.index<- length(w.vals)
  corr.match.array.mc.list <- list()
  
  
  for (it in 1:it.per.G){
    corr.match.array.mc<-array(0,dim=c(length(n_vals),npert,w.max.index))
  for(ipert in 1:npert)
  {
    G<-ER(n,0.5)
    Y.emb<-NULL
    Gp<-bitflip(G ,pert[ipert],pert[ipert])
    corr.matches<-run.experiment.JOFC(G,Gp,n_vals,num_iter=1,
                                      embed.dim=embed.dim, diss_measure=diss_measure,w.vals=w.vals,
                                      num_v_to_embed_at_a_time= num_v_to_embed_at_a_time,
                                      return.list=TRUE,sep.err.w=sep.err.w)
    
    corr.match.array.mc[,ipert,] <- corr.matches[[it]]
  }
  
  corr.match.array.mc.list <-c(corr.match.array.mc.list,list(corr.match.array.mc))
  }
  return (corr.match.array.mc.list)
}



bitflip_exp_w<-function (nmc,pert,n,n_vals,embed.dim=6,w.vals=0.8,diss_measure)
{
  npert<-length(pert)
  w.max.index<-length(w.vals)
  
  seq <- TRUE
  require(foreach)
  
  num.cores<-parallel::detectCores()
  iter_per_core <- ceiling(nmc/num.cores)
  
  num_iter<- iter_per_core*num.cores
  corr.match.array<-array(0,dim=c(length(n_vals),num_iter,npert,w.vals))
  corr.match.avg<-array(0,dim=c(length(n_vals),npert,w.vals))
  
  
  if(seq){
    registerDoSEQ()
  } else if (.Platform$OS.type != "windows" && require("multicore")) {
    require(doMC)
    registerDoMC()
  } else if (FALSE &&                     # doSMP is buggy
    require("doSMP")) {
    workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
    on.exit(stopWorkers(workers), add = TRUE)
    registerDoSMP(workers)
  } else if (require("doSNOW")) {
    cl <- snow::makeCluster(num.cores, type = "SOCK")
    on.exit(snow::stopCluster(cl), add = TRUE)
    registerDoSNOW(cl)
  } else {
    registerDoSEQ()
  }  
  

  corr.match.list<- foreach(i=1:num.cores, .combine="c",.export=c("bitflip_MC_rep","run.experiment.JOFC")) %dopar% {
    #	setwd('~/projects/DataFusion-graphmatch/')
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
    rep.result <- bitflip_MC_rep (pert,n,n_vals,embed.dim=embed.dim,diss_measure=diss_measure,it.per.G=iter_per_core, 
                                num_v_to_embed_at_a_time=1,w.vals=w.vals)
  }
    
  print(str(corr.match.list))
  for (t in 1:length(corr.match.list))
    corr.match.array[,t, ,] <- corr.match.list[[t]]
  
  
  corr.match.array<-sweep(corr.match.array, 1, n-n_vals, "/")
  #print(corr.match.array)
  print(dim(corr.match.array))
  
  corr.match.sd <- corr.match.avg
  for (w.i  in 1:w.max.index)
  for (i in 1:length(n_vals)) 
    for (j in 1:npert){
      corr.match.avg[i,j,w.i] <- mean(corr.match.array[i,,j,w.i])
      corr.match.sd[i,j,w.i] <- sd(corr.match.array[i,,j,w.i])
    }
  
  require(RColorBrewer)
  
  
  colors.vec <- c("red","green","aquamarine","purple",
                  "darkblue","salmon","rosybrown","magenta","orange")
  colors.vec[3]<-"gold4"
  colors.vec[2]<-"darkblue"
  colors.vec[4]<-"darkorange4"
  colors.vec[9]<-"red"
  colors.vec.len<-length(colors.vec)
  
  colors.vec.brew<- brewer.pal(colors.vec.len,"YlOrRd")
  
  colors.vec.brew[colors.vec.len+1]<-"cornflowerblue"
  colors.vec.brew[colors.vec.len+2]<-"azure3"
  colors.vec.brew[colors.vec.len+3]<-"cyan"
  
  
  
  
  
  colors.vec.len <- length(colors.vec.brew)
  colors.vec.brew[(colors.vec.len-2):colors.vec.len] <- brewer.pal(3,"Set3")
  #palette("YlOrRd")
  
  
  
  w.i<-1
  #windows()
  plot(n_vals, as.vector(corr.match.avg[,1,w.i]) ,xlab="Hard seeds",
       ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[1],type="l")
  
  for(ipert in 2:npert)
  {
    lines(n_vals, as.vector(corr.match.avg[,ipert,w.i]) ,xlab="Hard seeds",
          ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec[ipert])
  }  
  lg.txt<-c("varying pert.param")
  title(lg.txt)
  if (.Platform$OS.type != "windows") {windows() 
  } else {X11()}
  
  ipert = 2
  plot(n_vals, as.vector(corr.match.avg[,ipert,1]) ,xlab="Hard seeds",
       ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec.brew[1],type="l")
  if (w.max.index>1){
  for(w.i in 2:w.max.index)
  {
    lines(n_vals, as.vector(corr.match.avg[,ipert,w.i]) ,xlab="Hard seeds",
          ylab="Fraction of  correct matches",ylim=c(0,1),col=colors.vec.brew[w.i])
  }  
  }
  
  title.txt <- "ipert=2 varying w"
  title(title.txt)
  
  
  
  
  return (list(avg= corr.match.avg,sd=corr.match.sd,res.list=corr.match.list))
}









wiki_exp <- function(num_iter,n_vals,embed.dim=13) {
  #Not yet implemented
  load ("./data/wiki.RData")
  corr.matches<-run.experiment.JOFC(GE,GF,n_vals,num_iter=num_iter,
                                    embed.dim, diss_measure="default")
  
}	



worm_exp_par_sf_w <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,
		diss_measure="C_dice_weighted",symmetrize=TRUE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) {
	
	load("./data/celegansGraph.Rd")
	
	
	sum_row_c = apply(Ac,1,sum)
	sum_col_c = apply(Ac,2,sum)
	sum_row_g = apply(Ag,1,sum)
	sum_col_g = apply(Ag,2,sum)
	
	disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
	Ac <- Ac[!disc_v,!disc_v]
	Ag <- Ag[!disc_v,!disc_v]
       v_count <- sum(!disc_v)
	save(file="worm_v_count.txt", v_count ,ascii=TRUE)
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
		Ac_graph <- Ac
		Ag_graph <- Ag
		
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
	require(foreach)
	
	if(seq){
		registerDoSEQ()
	} else if (.Platform$OS.type != "windows" && require("multicore")) {
		require(doMC)
		registerDoMC()
	} else if (FALSE &&                     # doSMP is buggy
			require("doSMP")) {
		workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
		on.exit(stopWorkers(workers), add = TRUE)
		registerDoSMP(workers)
	} else if (require("doSNOW")) {
		cl <- snow::makeCluster(num.cores, type = "SOCK")
		on.exit(snow::stopCluster(cl), add = TRUE)
		registerDoSNOW(cl)
	} else {
		registerDoSEQ()
	}  
	
	
	corr_match_list<- foreach(i=1:num.cores, .combine="c",.export="run.experiment.JOFC") %dopar% {
		#	setwd('~/projects/DataFusion-graphmatch/')
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
				graph.is.directed= graph.is.directed,
				preselected.seeds=preselected.seeds,
				preselected.test =preselected.test,
				w.vals =w.vals,
				return.list=TRUE,
				sep.err.w =sep.err.w

		)
		
		#dimnames(corr.matches)[[1]]<-as.list(n_vals)
		#dimnames(corr.matches)[[2]]<-paste("iteration",1:iter_per_core)
		#dimnames(corr.matches)[[3]] <-as.list(w.vals)
		corr.matches
	}
	
	
	print (str(corr_match_list))
	
	
	return (corr_match_list )
}









worm_exp <- function(...) {
  
				 worm_exp_par_sf_w(...,seq=TRUE)
}






enron_exp <- function (num_iter,n_vals_vec,embed.dim=2){
  #Not yet implemented
  load("./data/AAA-187As-184x184.Rbin")
  corr.matches<-run.experiment.JOFC(AAA[[130]],AAA[[131]],
                                    n_vals_vec,num_iter=num_iter,embed.dim=embed.dim,
                                    diss_measure="default",
                                    preselected.seeds=NULL)
  
}


enron_exp_par_sf_w <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,
		diss_measure="C_dice_weighted",symmetrize=TRUE,
		preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE, sep.err.w=TRUE) {
	
	load("./data/AAA-187As-184x184.Rbin")
	Ac=AAA[[130]]
	Ag=AAA[[131]]
	
	sum_row_c = apply(Ac,1,sum)
	sum_col_c = apply(Ac,2,sum)
	sum_row_g = apply(Ag,1,sum)
	sum_col_g = apply(Ag,2,sum)
	disc_v_c <- ((sum_col_c==0)&(sum_row_c==0)) 
       disc_v_g <- ((sum_col_g==0) & (sum_row_g==0))
	disc_v <- disc_v_c|disc_v_g 
       v_count <- sum(!disc_v)
       save(file="enron_v_count.txt",v_count ,ascii=TRUE)
	Ac <- Ac[!disc_v,!disc_v]
	Ag <- Ag[!disc_v,!disc_v]
	graph.is.directed <- TRUE
	
		Ac_graph <- Ac
		Ag_graph <- Ag
		
		if (symmetrize){
			graph.is.directed <- FALSE
			Ac_graph <- (Ac+t(Ac))/2
			Ag_graph <- (Ag+t(Ag))/2
		} else{

if (diss_measure=="ECT"||diss_measure=="diffusion"){ 
sum_row_c = apply(Ac_graph,1,sum)
	sum_col_c = apply(Ac_graph,2,sum)
	sum_row_g = apply(Ag_graph,1,sum)
	sum_col_g = apply(Ag_graph,2,sum)
	disc_v_c <- ((sum_col_c==0)|(sum_row_c==0)) 
       disc_v_g <- ((sum_col_g==0) | (sum_row_g==0))
       disc_v <- disc_v_c|disc_v_g 
	Ac_graph <- Ac_graph[!disc_v,!disc_v]
	Ag_graph <- Ag_graph[!disc_v,!disc_v]
       v_count <- sum(!disc_v)
       save(file="enron_v_count.txt",v_count ,ascii=TRUE)
}


			}

		
		Ac_graph<- (Ac_graph>0)
		Ag_graph<- (Ag_graph>0)
       

	
	
	num.cores<-parallel::detectCores()
	iter_per_core <- ceiling(num_iter/num.cores)
	require(foreach)
	
	if(seq){
		registerDoSEQ()
	} else if (.Platform$OS.type != "windows" && require("multicore")) {
		require(doMC)
		registerDoMC()
	} else if (FALSE &&                     # doSMP is buggy
			require("doSMP")) {
		workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
		on.exit(stopWorkers(workers), add = TRUE)
		registerDoSMP(workers)
	} else if (require("doSNOW")) {
		cl <- snow::makeCluster(num.cores, type = "SOCK")
		on.exit(snow::stopCluster(cl), add = TRUE)
		registerDoSNOW(cl)
	} else {
		registerDoSEQ()
	}  
	
	
	corr_match_list<- foreach(i=1:num.cores, .combine="c",.export="run.experiment.JOFC") %dopar% {
		#	setwd('~/projects/DataFusion-graphmatch/')
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
		corr.matches<-try( 
				run.experiment.JOFC(Ac_graph,Ag_graph,n_vals,num_iter=iter_per_core,
				embed.dim,diss_measure=diss_measure,
				
				graph.is.weighted=weighted.graph,
				graph.is.directed= graph.is.directed,
				preselected.seeds=preselected.seeds,
				preselected.test =preselected.test,
				w.vals =w.vals,
				return.list=TRUE,
				sep.err.w =sep.err.w
		)
		)
		
		#dimnames(corr.matches)[[1]]<-as.list(n_vals)
		#dimnames(corr.matches)[[2]]<-paste("iteration",1:iter_per_core)
		#dimnames(corr.matches)[[3]] <-as.list(w.vals)
		if (inherits(corr.matches,"try-error")) {
		sink("enron-debug.txt")
		traceback()
		sink()
		NULL
		}  else{  corr.matches
		}
	}
	
	
	print (str(corr_match_list))
	
	
	return (corr_match_list )
}



wiki_exp_par_sf_w <- function(num_iter,n_vals,embed.dim=3,weighted.graph=TRUE,
                               diss_measure="C_dice_weighted",symmetrize=TRUE,
                               preselected.seeds=NULL,preselected.test=NULL,w.vals, seq=FALSE) {
  
  load("./data/Wiki_orig.RData")
  Ac=AG_wiki_en_mat
  Ag=AG_wiki_fr_mat
  
  sum_row_c = apply(Ac,1,sum)
  sum_col_c = apply(Ac,2,sum)
  sum_row_g = apply(Ag,1,sum)
  sum_col_g = apply(Ag,2,sum)
  
  disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
       v_count <- sum(!disc_v)
  save(file="wiki_v_count.txt",v_count,ascii=TRUE)
  Ac <- Ac[!disc_v,!disc_v]
  Ag <- Ag[!disc_v,!disc_v]
  graph.is.directed <- TRUE
  
  Ac_graph <- Ac
  Ag_graph <- Ag
  
  if (symmetrize){
    graph.is.directed <- FALSE
    Ac_graph <- (Ac+t(Ac))/2
    Ag_graph <- (Ag+t(Ag))/2
  }
  
  Ac_graph<- (Ac_graph>0)
  Ag_graph<- (Ag_graph>0)
  
  
  num.cores<-parallel::detectCores()
  iter_per_core <- ceiling(num_iter/num.cores)
  require(foreach)
  
  if(seq){
    registerDoSEQ()
  } else if (.Platform$OS.type != "windows" && require("multicore")) {
    require(doMC)
    registerDoMC()
  } else if (FALSE &&                     # doSMP is buggy
               require("doSMP")) {
    workers <- startWorkers(num.cores,FORCE=TRUE) # My computer has 4 cores
    on.exit(stopWorkers(workers), add = TRUE)
    registerDoSMP(workers)
  } else if (require("doSNOW")) {
    cl <- snow::makeCluster(num.cores, type = "SOCK")
    on.exit(snow::stopCluster(cl), add = TRUE)
    registerDoSNOW(cl)
  } else {
    registerDoSEQ()
  }  
  
  
  corr_match_list<- foreach(i=1:num.cores, .combine="c",.export="run.experiment.JOFC") %dopar% {
    #	setwd('~/projects/DataFusion-graphmatch/')
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
    corr.matches <- try(
                    run.experiment.JOFC(Ac_graph,Ag_graph,n_vals,num_iter=iter_per_core,
                                      embed.dim,diss_measure=diss_measure,
                                      
                                      graph.is.weighted=weighted.graph,
                                      graph.is.directed= graph.is.directed,
                                      preselected.seeds=preselected.seeds,
                                      preselected.test =preselected.test,
                                      w.vals =w.vals,
                                      return.list=TRUE,
				sep.err.w =sep.err.w

    )
    )
    
    if (inherits(corr.matches,"try-error")){
    sink("enron-error-debug.txt")
    print(traceback())
     print(corr.matches)
    sink()
    }
    
    #dimnames(corr.matches)[[1]]<-as.list(n_vals)
    #dimnames(corr.matches)[[2]]<-paste("iteration",1:iter_per_core)
    #dimnames(corr.matches)[[3]] <-as.list(w.vals)
    corr.matches
  }
  
  
  print (str(corr_match_list))
  
  
  return (corr_match_list )
}



