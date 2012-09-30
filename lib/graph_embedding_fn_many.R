
JOFC.Insample.Embed.many.match <-function(D1,D2,D.W,ndimens,w.vals,sep.err.w,init.conf,wt.equalize){
  #	if (profile.mode) Rprof("JOFC.FC.out",append=TRUE)
  n.1<- nrow(D1)
  n.2<- nrow(D2)
  D<-omnibusM(D1,D2,D.W)
  
  smacof.embed<-list()
  stress.vec<-c()
  comm.sum.vec<-c()
  fid1.sum.vec<-c()
  fid2.sum.vec<-c()
  
  comm.vec<-c()
  fid1.vec<-c()
  fid2.vec<-c()
  
  
  
  for (w in w.vals){
    W.1<-matrix(1-w,n.1,n.1)
    W.2<-matrix(1-w,n.2,n.2)
    W.W<-matrix(0,n.1,n.2)
    W.W[D.W==0]<-w
    Weight.Mat<-omnibusM(W.1,W.2,W.W)
    Weight.Mat[is.na(D)]<-0
    D[is.na(D)] <-1
    
    new.embed <- smacofM(D,ndimens    ,	W=Weight.Mat        ,
                         init    = init.conf,
                         verbose = FALSE,
                         itmax   = 1000,
                         eps     = 1e-6)
    smacof.embed<-c(smacof.embed,list(new.embed ))
    stress.mat <- (as.dist(D) - dist(new.embed))^2
    
    
  }
  
  return(smacof.embed)
}

genG <- function(s=seed,n=1000,np=50,p=0.1,q=0.5) {
  x <- matrix(p, nrow=n, ncol=n)    ## null: (100-np)x98 matrix
  if (np>0) {
    egg <- matrix(q, nrow=np, ncol=np) ## alternative: npx98 matrix
    x[(n-np+1):n,(n-np+1):n] <- egg
  }
  ordering<-sample.int(n)
  diag(x) <- 0
  
  A <- matrix(0,nrow=n,ncol=n)
  for(i in 1:(n-1))
    for(j in i:n)
      A[i,j] <- A[j,i] <- sample(c(0,1),1,prob=c(1-x[i,j],x[i,j]))
  
  return(A)
}


perturbG<-function(G,q){
  n<-nrow(G)
  Flip.mat<-matrix(0,nrow=n,ncol=n)
  for(i in 1:(n-1))
    for(j in i:n){
      Flip.mat[i,j]<-sample(c(0,1),1,prob=c(1-q,q))
      
    }
  for(i in 1:(n-1))
    for(j in i:n){
      if (Flip.mat[i,j]==1){
        G[i,j]<-1-G[i,j]
        G[j,i]<-1-G[j,i]
      }
      
    }
  return(G)
  
}


graph2dissimilarity.many <- function (G,Gp,corr.list,
                                      in.sample.ind.1,in.sample.ind.2,
                                      d.dim,
                                      w.vals.vec,
                                      graph.mode,
                                      vert_diss_measure,
                                      T.param,
                                      num_v_to_embed_at_a_time  ,
                                      weighted.g) {
  n.1<-nrow(G)
  n.2<-nrow(Gp)
  in.samp.ind.1 <- which(in.sample.ind.1)
  in.samp.ind.2 <- which(in.sample.ind.2)
  test.m.1<-sum(!in.sample.ind.1)
  test.m.2<-sum(!in.sample.ind.2)
  
  Graph.1 <-  graph.adjacency(G, mode =  graph.mode,weighted=weighted.g)
  Graph.2 <-  graph.adjacency(Gp,mode  =  graph.mode,weighted=weighted.g)
  #A.M <-   diag(n)
  #A.M[!in.sample.ind[1:n],!in.sample.ind[1:n]] <-   0 # Make sure vertices that are NOT known to be matched
  #are not connected
  
  
  #Now that graphs are generated from adjacency or weight matrices,
  # compute dissimilarities using shortest.paths  
  
  
  #compute dissimilarities in separate graphs, then impute dissimilarity between different condition
  if (vert_diss_measure == 'default'){
    D.1 <-  shortest.paths(Graph.1)
    D.2 <-  shortest.paths(Graph.2)
  } else if (vert_diss_measure == "diffusion"){
    D.1 <-  diff.dist.fun(G,T.param)
    D.2 <-  diff.dist.fun(Gp,T.param)	
  } else if (vert_diss_measure == 'ell1'){
    D.1 <-   as.matrix(dist(G,'manhattan'))
    D.2 <-   as.matrix(dist(Gp,'manhattan'))
  } else if (vert_diss_measure == 'jaccard'){
    D.1  <-   similarity.jaccard( Graph.1)
    D.2  <-   similarity.jaccard( Graph.2)
    
    D.1  <-   2- 2*D.1
    D.2  <-   2- 2*D.2
  } else if (vert_diss_measure == 'dice'){
    D.1  <-   similarity.dice( Graph.1)
    D.2  <-   similarity.dice( Graph.2)
    D.1  <-   2- 2*D.1
    D.2  <-   2- 2*D.2
  }  else if (vert_diss_measure == 'invlogweighted'){
    D.1  <-   similarity.invlogweighted( Graph.1)
    D.2  <-   similarity.invlogweighted( Graph.2)
    D.1  <-   2- 2*D.1
    D.2  <-   2- 2*D.2
  } else if (vert_diss_measure == 'exp_minus'){
    D.1 <- exp(-1*G/2)
    D.2 <- exp(-1*Gp/2)    
  } else if (vert_diss_measure == 'C_dice_weighted'){
    
    D.1 <- C_dice_weighted(G)
    D.2 <- C_dice_weighted(Gp)
  }                                   
  # In case dissimilarities blow up for diss. measure, put a limit on max 
  # value for diss.
  
  
  
  if (vert_diss_measure == "diffusion"){
    
    upper_diss_limit<-min(quantile(c(as.vector(D.1),as.vector(D.2)),
                                   0.99,na.rm=TRUE),1E3)
    
    D.1[D.1>upper_diss_limit] <- upper_diss_limit
    D.2[D.2>upper_diss_limit] <- upper_diss_limit
  }
  #Should we replace   infinite values of  D.1 and D.2  to large number
  # or ignore them in embeddin?
  #D.1.temp<-D.1
  #D.2.temp<-D.2
  D.1[is.infinite(D.1)] <-  2*max(D.1[is.finite(D.1)])
  D.2[is.infinite(D.2)] <-  2*max(D.2[is.finite(D.2)])
  D.w<- matrix(NA,n.1,n.2)
  for (corr.i in corr.list){
    for ( i in corr.i[[1]]){
      for ( j in corr.i[[2]]){
        D.w[i,j]<-matched.cost
        
      }
    }
  }
  # We can also impute the btw-cond dissimilarities for insample
  # That's not cheating
  #if ((i %in% in.samp.ind.1)&& (j%in.samp.ind.2))
  # D.w[i,j]<-D.1[]
  
  D.M <-   omnibusM(D.1,D.2,D.w)
  return(list(D.M=D.M,D.1=D.1,D.2=D.2,D.W=D.w))
}



JOFC.graph.custom.dist.many  <-   function(G,Gp,corr.list,
                                           in.sample.ind.1,in.sample.ind.2,
                                           d.dim,
                                           w.vals.vec,
                                           graph.is.directed  =  FALSE,
                                           vert_diss_measure  =  "default",
                                           T.param  =  NULL,
                                           num_v_to_embed_at_a_time   =   sum(!in.sample.ind)/2,
                                           graph.is.weighted=FALSE   )  {
  
  
  
  
  n.1<-nrow(G)
  n.2<-nrow(Gp)
  test.m.1<-sum(!in.sample.ind.1)
  test.m.2<-sum(!in.sample.ind.2)
  
  graph.mode <-   ifelse(graph.is.directed,"directed","undirected")
  weighted.g <- graph.is.weighted
  if (!weighted.g)
    weighted.g<-NULL
  print("T.param")
  print(T.param)
  
  
  
  D.Mats<-graph2dissimilarity.many (G,Gp,corr.list,
                                    in.sample.ind.1,in.sample.ind.2
                                    d.dim,
                                    w.vals.vec,
                                    graph.mode,
                                    vert_diss_measure,
                                    T.param,
                                    num_v_to_embed_at_a_time  ,
                                    weighted.g)
  
  
  #Given adjacency matrix, generate unweighted graph
  if (!graph.is.weighted)   print("Using adjacency for computing dissimilarities")
  else print("Using weighted for computing dissimilarities") 
  if (isSymmetric(G)) {print("G is symmetric")}
  else   {print("G is unsymmetric")}
  
  if (isSymmetric(Gp)) {print("Gp is symmetric")}
  else   {print("G is unsymmetric")}
  
  #num_v_to_embed_at_a_time   <-   min(floor(1.2*sum(in.sample.ind)),sum(!in.sample.ind))
  #num_v_to_embed_at_a_time   <-   sum(!in.sample.ind)
  print("num_v_to_embed_at_a_time")
  print(num_v_to_embed_at_a_time)
  
  
  
  
  Embed.List <- Embed.Nodes.many (D.Mats,
                                  in.sample.ind.1,
                                  in.sample.ind.2,
                                  
                                  d.start  =  d.dim,
                                  wt.equalize=FALSE,
                                  separability.entries.w=FALSE,
                                  assume.matched.for.oos = FALSE ,
                                  w.vals=w.vals.vec)
  
  J <-  list()
  for (Y.embed in Embed.List){
    
    test.samp.size <-  nrow(Y.embed)/2
    Dist  <-  as.matrix(dist(Y.embed))[1:test.m.1,(1:test.m.2)+test.m.1]
    
    J <-  c(J,list(Dist))
    
  }
  return(J)
  
  
}


JOFC.graph.many <-  function(G,Gp,corr.list,
                             in.sample.ind,
                             d.dim,
                             w.vals.vec,
                             graph.is.directed  =  FALSE){
  
  return(JOFC.graph.custom.dist.many(G  =  G,Gp  =  Gp,corr.list= corr.list,in.sample.ind  =  in.sample.ind,d.dim  =  d.dim,w.vals.vec  =  w.vals.vec,
                                     graph.is.directed  =  graph.is.directed,vert_diss_measure  =  "default"))
  
}

JOFC.graph.diff.many <-  function(G,Gp,corr.list,
                                  in.sample.ind,
                                  d.dim,
                                  w.vals.vec,
                                  graph.is.directed  =  FALSE,
                                  T.param  =  2){
  return(JOFC.graph.custom.dist.many(G  =  G,Gp  =  Gp, corr.list= corr.list,in.sample.ind  =  in.sample.ind,d.dim  =  d.dim,w.vals.vec  =  w.vals.vec,
                                     graph.is.directed  =  graph.is.directed,vert_diss_measure  =  "diffusion",T.param  =  T.param))
  
}




jofc.many<-function(G,Gp,corr.list,
                    in.sample.ind.1,in.sample.ind.2,
                    d.dim,
                    w.vals.vec,
                    graph.is.directed=FALSE,
                    oos=TRUE,
                    notconnect.wt=10,
                    use.weighted.graph=TRUE,
                    wt.matrix.1=NULL,
                    wt.matrix.2=NULL,
                    sep.graphs=TRUE # if TRUE, treat two graphs separately to compute dissimilarities
                    #and impute W (off-diagonalblock matrix)
                    # if FALSE, join the graphs and compute dissimilarities from joint graph
                    
){
  n.1<-nrow(G)
  n.2<-nrow(Gp)
  test.m.1<-sum(!in.sample.ind.1)
  test.m.2<-sum(!in.sample.ind.2)
  
  
  graph.mode<- ifelse(graph.is.directed,"directed","undirected")
  
  Graph.1<-graph.empty(directed=graph.is.directed)
  Graph.2<-graph.empty(directed=graph.is.directed)
  Graph.M<-graph.empty(directed=graph.is.directed)
  
  if (!use.weighted.graph) {
    #Given adjacency matrix, generate unweighted graph
    print("Using adjacency for computing dissimilarities")
    Graph.1<-graph.adjacency(G, mode=graph.mode)
    Graph.2<-graph.adjacency(Gp,mode=graph.mode)
    A.M<- matrix(0,n.1,n.2)
    for (corr.i in corr.list){
      for ( i in corr.i[[1]]){
        for ( j in corr.i[[2]]){
          A.M[i,j]<-1
        }
      }
    }
    G.comb<-omnibusM(G,Gp,A.M)
    Graph.M <- graph.adjacency(G.comb,
                               weighted= NULL ,mode=graph.mode)
  } else{
    print("Using weighted graph for computing dissimilarities")
    # If creating  a weighted graph 
    # make the weight matrix from adjacency matrix. Those with same-condition edges have weights of wt.connect/10
    #those  with no edges have weights of wt.connect. Those with "matched edges has weights of matched.cost
    A.M<- matrix(500,n.1,n.2)
    for (corr.i in corr.list){
      for ( i in corr.i[[1]]){
        for ( j in corr.i[[2]]){
          A.M[i,j]<-matched.cost
        }
      }
    }    
    
    
    #A.M[!in.sample.ind[1:n]]<- 0
    if (is.null(wt.matrix.1)){
      #Given adjacency matrix, generate weighted graph
      wt.matrix.1 <- G
      wt.matrix.2 <- Gp
      wt.matrix.1[G==0] <- notconnect.wt
      wt.matrix.2[Gp==0] <- notconnect.wt						
      wt.matrix.1[G==1] <- notconnect.wt/10			
      wt.matrix.2[Gp==1] <- notconnect.wt/10
      G.comb.w<-omnibusM(wt.matrix.1,wt.matrix.2,A.M)
      
      if (sep.graphs){
        Graph.1<-graph.adjacency(wt.matrix.1, weighted= TRUE , mode=graph.mode)
        Graph.2<-graph.adjacency(wt.matrix.2, weighted= TRUE , mode=graph.mode)
      }			else{
        stop("This case is not implemented")
        
      }
    }
    else{  #if wt.matrix.1 is not null
      
      if (sep.graphs){
        #Given weight matrix, generate weighted graph                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
        Graph.1<-graph.adjacency(wt.matrix.1 ,weighted=TRUE,mode= graph.mode)
        Graph.2<-graph.adjacency(wt.matrix.2,weighted=TRUE,mode=graph.mode)
        
        #Don't need to compute Graph.M
        
      }
      else{
        stop("This case not implemented")
      }
    }
    
  }
  
  #Now that graphs are generated from adjacency or weight matrices,
  # compute dissimilarities using shortest.paths	
  
  if (sep.graphs){
    
    #compute dissimilarities in separate graphs, then impute dissimilarity between different condition
    D.1<-shortest.paths(Graph.1)
    D.2<-shortest.paths(Graph.2)
    D.1[is.infinite(D.1)]<-NA
    D.2[is.infinite(D.2)]<-NA
    D.w<- matrix(NA,n.1,n.2)
    for (corr.i in corr.list){
      for ( i in corr.i[[1]]){
        for ( j in corr.i[[2]]){
          D.w[i,j]<-matched.cost
        }
      }
    }
    D.M<- omnibusM(D.1,D.2,D.w)
    
  }
  else{
    stop("This case is not implemented")
  }
  #print("max(D.M)")
  #print(max(D.M))
  
  
  Embed.List<-Embed.Nodes.many(D.M[1:n.1,1:n.1],D.M[n.1+(1:n.2),n.1+(1:n.2)],D.M[1:n.1,n.1+(1:n.2)],
                               
                               in.sample.ind.1,in.sample.ind.2 ,oos ,
                               d=d.dim,
                               wt.equalize=FALSE,
                               separability.entries.w=FALSE,
                               assume.matched.for.oos = FALSE,
                               w.vals=w.vals.vec)	
  
  
  J<-list()
  for (Y.embed in Embed.List){
    print("dim(Y.embed)")
    print(dim(Y.embed))	
    Dist=as.matrix(dist(Y.embed))[1:test.m.1,(1:test.m.2)+test.m.1]
    J<-c(J,list(Dist))
    
  }
  return(J)
}



jofc.diffusion.dist.many<-function(G,Gp,corr.list,
                                   in.sample.ind.1,in.sample.ind.2,
                                   d.dim,
                                   w.vals.vec,
                                   graph.is.directed=FALSE,
                                   oos=TRUE,
                                   
                                   wt.matrix.1=NULL,
                                   wt.matrix.2=NULL,
                                   sep.graphs=TRUE, # if TRUE, treat two graphs separately to compute dissimilarities
                                   #and impute W (off-diagonalblock matrix)
                                   # if FALSE, join the graphs and compute dissimilarities from joint graph
                                   T.param=1
){
  n.1<-nrow(G)
  n.2<-nrow(Gp)
  test.m.1<-sum(!in.sample.ind.1)
  test.m.2<-sum(!in.sample.ind.2)
  
  graph.mode<- ifelse(graph.is.directed,"directed","undirected")
  
  
  if (sep.graphs){
    if (is.null(wt.matrix.1)){
      D.1<-diff.dist.fun(G,T.param)
      D.2<-diff.dist.fun(Gp,T.param)
    } else{
      D.1<-diff.dist.fun(wt.matrix.1,T.param)
      D.2<-diff.dist.fun(wt.matrix.2,T.param)
    }
    D.w<- matrix(NA,n.1,n.2)
    for (corr.i in corr.list){
      for ( i in corr.i[[1]]){
        for ( j in corr.i[[2]]){
          D.w[i,j]<-0
        }
      }
    }
    D.M<- omnibusM(D.1,D.2,D.w)
  }	else{ #combine graphs and compute distances on joint graph
    if (is.null(wt.matrix.1)){
      for (corr.i in corr.list){
        for ( i in corr.i[[1]]){
          for ( j in corr.i[[2]]){
            A.M[i,j]<-1
          }
        }
      }    
      #A.M[!in.sample.ind[1:n]]<- 0
      G.comb<-omnibusM(G,Gp,A.M)
      #compute dissimilarities in joint graph
      D.M<-diff.dist.fun(G.comb,T.param)
      D.M[is.infinite(D.M)]<-NA
    } else{
      wt.W<-matrix(Inf,n.1,n.2)
      for (corr.i in corr.list){
        for ( i in corr.i[[1]]){
          for ( j in corr.i[[2]]){
            wt.W[i,j]<-1
          }
        }
      }    
      
      
      Wt.M<-omnibusM(wt.matrix.1,wt.matrix.2,wt.W)
      D.M<-diff.dist.fun(Wt.M,T.param)
      D.M[is.infinite(D.M)]<-NA
    }
    
  }
  
  
  
  Embed.List<-Embed.Nodes.many(D.M[1:n.1,1:n.1],D.M[n.1+(1:n.2),n.1+(1:n.2)],D.M[1:n.1,n.1+(1:n.2)],
                               in.sample.ind.1,in.sample.ind.2 ,oos ,
                               d=d.dim,
                               wt.equalize=FALSE,
                               separability.entries.w=FALSE,
                               assume.matched.for.oos = FALSE,
                               w.vals=w.vals.vec)	
  J<-list()
  for (Y.embed in Embed.List){
    print("dim(Y.embed)")
    print(dim(Y.embed))	
    
    
    Dist=as.matrix(dist(Y.embed))[1:test.m.1,(1:test.m.2)+test.m.1]
    J<-c(J,list(Dist))
    
  }
  return(J)
  
}









Embed.Nodes.many <-function(D.Mats,
                            in.sample.ind.1,
                            in.sample.ind.2,
                            oos, 
                            d,
                            wt.equalize=FALSE,
                            separability.entries.w=FALSE,
                            assume.matched.for.oos = FALSE ,
                            w.vals=0.99){
  
  D.1 <- D.Mats$D.1
  D.2 <- D.Mats$D.2
  D.W <- D.Mats$D.W
  
  
  n.1<-nrow(D.1)
  n.2<-nrow(D.2)
  in.sample.ind <- c(in.sample.ind.1,in.sample.ind.2)
  in.indices<- which(in.sample.ind)
  oos.indices<- which(oos.sample.ind)
  n.1.in <- sum(in.sample.ind.1)
  n.2.in <- sum(in.sample.ind.2)
  
  test.m.1 <- n.1 - n.1.in
  test.m.2 <- n.2 - n.2.in
  test.m <- test.m.1+test.m.2
  n.1.oos <- n.1-n.1.in 
  n.2.oos <- n.2-n.2.in
  in.indices.1<- in.indices[1:n.1.in]
  in.indices.2<- in.indice[n.1.in+(1:n.2.in)]
  
  
  oos.indices.1<- oos.indices[1:(test.m.1)]
  oos.indices.2<-  oos.indices[test.m.1+(1:test.m.2)]
  
  Y.embeds<-list()
  oos.use.imputed<- FALSE
  w.max.index<-length(w.vals)
  
  
  #in.sample.ind<-which(in.sample.ind)
  
  # Embed in-sample using different weight matrices (differentw values)
  
  
  D.omnibus<-omnibusM(D.1,D.2,D.W)
  D.in.1 <- D.1[in.sample.ind.1,in.sample.ind.1]
  D.in.2 <- D.2[in.sample.ind.2,in.sample.ind.2]
  D.in.W <- D.W[in.sample.ind.1,in.sample.ind.2]
  
  D.in<-omnibusM(D.in.1,D.in.2,D.in.W)
  init.conf=NULL
  if (sum(is.na(D.in))==0) {		
    init.conf<-cmdscale(d=D.in,k=d)
  }	
  
  #   # Find the minimum embedding dimension that matches all the seeds correctly
  #   full.seed.match  <-   FALSE
  #   embed.dim  <-  d.start
  #   prevTrueMatch = -1
  #   True.match.last.memory <- rep(-1,3)
  #   X.embeds <- list()
  #   while  (!full.seed.match) {
  #     embed.dim   <-   embed.dim + 1
  #     
  #     X.embeds <-  try(JOFC.Insample.Embed(D.in,embed.dim,
  #                                          w.vals,separability.entries.w,
  #                                          init.conf  =  init.conf,
  #                                          wt.equalize  =  wt.equalize))
  #     if (inherits(X.embeds,"try-error")) {
  #       print('Unable to embed via smacof')
  #       X.embeds<-list(cmdscale(D.in, k=d.start))
  #       embed.dim<-d.start
  #       full.seed.match   <-    TRUE
  #     }
  #     
  #     pw.dist.insample <- as.matrix(dist(X.embeds[[1]]))
  #     
  #     insample.match <-   pairmatch(pw.dist.insample[1:n,n+(1:n)])
  #     
  #     numTrueMatch <-  present(insample.match)
  #     
  #     if (numTrueMatch == n){
  #       full.seed.match   <-    TRUE
  #       print(paste("optimal dim is ", embed.dim))
  #     }
  #     if (all( True.match.last.memory== numTrueMatch)) {
  #       full.seed.match   <-    TRUE
  #       print(paste("optimal dim is ", embed.dim))
  #     }
  #     True.match.last.memory[1] <-    True.match.last.memory[2]
  #     True.match.last.memory[2] <-    True.match.last.memory[3]
  #     True.match.last.memory[3] <-   numTrueMatch
  #     
  #   }
  
  
  
  X.embeds<-JOFC.Insample.Embed.many.match(D.in.1,D.in.2,D.in.W,d,w.vals,separability.entries.w,init.conf=init.conf,
                                           wt.equalize=wt.equalize)
  for (l in 1:w.max.index){
    if (verbose) print("OOS embedding for JOFC for w= \n")
    
    
    w.val.l <- w.vals[l]
    X <- X.embeds[[l]]
    Y.w <- matrix(0,test.m,d)
    
    #Compute Weight matrix corresponding in-sample  entries
    # Since we are going to oos-embedding, set the weights  of in-sample embedding of stress
    # We are using previous in-sample embeddings, anyway
    oos.Weight.mat.1<-matrix(0,n.1.in+n.2.in,n.1.in+n.2.in)
    
    
    
    for (oos.samp.i in 1:test.m){
      omnibus.oos.D.0 <- rbind(
        cbind(D.in,D.omnibus[in.indices,oos.indices[oos.samp.i]]),
        cbind(D.omnibus[oos.indices[oos.samp.i],in.indices],0)
      )
      
      
      #Compute Weight matrix corresponding OOS  entries
      oos.Weight.mat.2 <- 0
      oos.Weight.mat.w <- 1-w
      
      oos.Weight.mat<-omnibusM(oos.Weight.mat.1,oos.Weight.mat.2,oos.Weight.mat.w)
      
      
      in.sample.ind<-c(in.sample.ind.1,in.sample.ind.2)
      
      
      oos.Weight.mat[is.na(omnibus.oos.D.0)]<-0
      omnibus.oos.D.0[is.na(omnibus.oos.D.0)]<-1
      
      if (verbose) print("JOFC null omnibus OOS embedding \n")
      
      Y.0t<-oosIM(D=omnibus.oos.D.0,
                  X=X,
                  init     = "random",
                  verbose  = FALSE,
                  itmax    = 1000,
                  eps      = 1e-8,
                  W        = oos.Weight.mat,
                  isWithin = NULL,
                  bwOos    = FALSE)
      Y.w[oos.samp.i,]<-Y.0t
      
    }
    
    if (verbose) print("JOFC alternative omnibus OOS embedding \n")
    Y.embeds<-c(Y.embeds,list(Y))
  }
  
  
  return(Y.embeds)
  
}

solveMarriage<- function(Dist){
  matches<-fullmatch(Dist)
  return(matches)
  
}

present.many<-function(M,corr.list,in.sample.ind.1,in.sample.ind.2){
  test.m.2.indices <- which(!in.sample.ind.2)
  test.m.1.indices <- which(!in.sample.ind.1)
  
  test.m.1<-length(test.m.1.indices)
  test.m.2<-length(test.m.2.indices)
  precision.list<-rep(0,test.m.2)
  recall.list<-rep(0,test.m.2)
  F.meas.list<-rep(0,test.m.2)
  for (test.i in 1:test.m.2){
    M.i<- test.i+test.m.1
    
    match.label<- M[M.i]
    #match.label:The match label for test.i^{th} instance in the second group
    matches.i.indic <- M[1:test.m.1]==match.label
    #matches.i.indic: the indicator(logical) vector for which of the instances in the first group
    #match test.i^{th} instance in the second group
    matches.indices.i<- test.m.1.indices[matches.i.indic]
    #matches.indices.i the indices for the instances the indicator vector
    for (corr.i in corr.list){
      for ( j in corr.i[[2]]){ # necessary only  if it's k-k matching instead  
        if (j==test.m.2.indices[test.i]){ #sanity check
          true.matches<-matches.indices.i%in%corr.i[[1]] #which of the matches found are in the list of true matches 
          precision<- sum(true.matches)/length(true.matches) #what proportion of the matches found are correct
          recall <-sum(true.matches)/length(corr.i[[1]]) #what proportion of the true matches are found
          p.r<-(precision+recall)
          F.meas<-0
          if(p.r!=0)
            F.meas<- 2*precision*recall/p.r
          
          precision.list[test.i]<-precision
          recall.list[test.i]<-recall
          F.meas.list[test.i]<-F.meas
          break
        }
      }
    }
  }
  return(list(P=precision.list,R=recall.list,F=F.meas.list))	
}


ER   <-   function(n,p)
{
  A   <-   matrix( rbinom(n^2,1,p) , ncol  =  n,byrow  =  T )
  A  <-  A*upper.tri(A)
  A  <-  A+t(A)
  return(A)
}

bitflip   <-   function(G,q10,q01,binary  =  T,symmetric = T,hollow  =  T)
  # takes graph (adjacency matrix) G and flips 1s to 0s with prob q10 and flips 0s to 1s with prob q01
  # assumes binary=T,symmetric=T,hollow=T
{
  n  <-  dim(G)[1]
  for(u in 1:(n-1))
    for(v in (u+1):n)
      G[u,v]  <-  G[v,u]  <-  ifelse(G[u,v],1-rbinom(1,1,q10),rbinom(1,1,q01))
  return(G)
}
adj.Mat.2.P<-function(A){
  a.avg<-rowMeans(A)
  for (i in 1:nrow(A)) A[,i]<-A[,i]/a.avg[i]	
}
diff.dist<-function(P){
  eig(P)
}


diff.dist.fun <-  function(A,T.diff,dissimilarity=FALSE){
  P <-  transition.matrix(A,dissimilarity  =  dissimilarity)
  D<-diffusion.distance(P, T.diff, directed = FALSE)
  D
}

C_dice_weighted <- function(W){
  n<-nrow(W)
  D<- matrix(0,n,n)
  for (i in 1:n) {
    for (j in 1:n) {
      sym_neigh_diff = xor(W[i,]>0 ,W[j,]>0)
      r_ij= sum((W[i,]+W[j,])*sym_neigh_diff)
      a_ij <- sum((W[i,]+W[j,])*(W[i,]>0)*(W[j,0]>0))
      D[i,j] <- (r_ij+2*(W[i,j]==0))/(r_ij+a_ij+2)
    }
    
  }
  return(D)
}


