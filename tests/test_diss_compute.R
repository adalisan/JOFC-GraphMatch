# test dissimilarities for all graphs

library(igraph)
source("./lib/diffusion_distance.R")
library(MASS) #ginv function

datasets <- c("worm","enron","wiki","cnet")
weighted <- c(TRUE,FALSE)
symmetrize <- c(TRUE,FALSE)
normalize.diss<-c(TRUE,FALSE)

dissim.meas<- c('default',"diffusion",'ECT','ell1','jaccard'
                , 'dice','invlogweighted','C_dice_weighted'
                ,'hybrid_DICE_SP')
test.cases <- expand.grid(datasets,weighted,symmetrize,dissim.meas,normalize.diss)
colnames(test.cases) <-c("datasets","weighted","symmetrize","dissim.meas", "normalize.diss")

load("./data/celegansGraph.Rd")
load("./data/AAA-187As-184x184.Rbin")
load("./data/Wiki_adj.RData")

load("./data/Ajt1-5699.Rbin")
load("./data/Ajt2-5699.Rbin")

num.test.cases<- dim(test.cases)[1]

subset = 400

for (test.case.it in 1:num.test.cases){
  
  if (test.cases[test.case.it,"datasets"]=="worm"){
    Graph.1 <- Ac
    Graph.2 <- Ag
    if (inherits(Graph.1,"igraph"))
      Graph.1<-get.adjacency(Graph.1)
    
    if (inherits(Graph.2,"igraph"))
      Graph.2<-get.adjacency(Graph.2)
  } else if (test.cases[test.case.it,"datasets"]=="enron"){
    Graph.1<- AAA[[130]]
    Graph.2 <- AAA[[131]]
    if (inherits(Graph.1,"igraph"))
      Graph.1<-get.adjacency(Graph.1)
    
    if (inherits(Graph.2,"igraph"))
      Graph.2<-get.adjacency(Graph.2)
  } else if (test.cases[test.case.it,"datasets"]=="wiki"){
    #en_a<- get.adjacency(en_a)
    #fr_a<- get.adjacency(fr_a)
    #en_a_cl <- giant.component(en_a)
    #fr_a_cl <- giant.component(fr_a)
    
    Graph.1 <- en_a
    Graph.2 <- fr_a
    
    N<-nrow(Graph.1)
    v_count<- N
    if (!is.null(subset) & subset<N){
      subset.v <-sample (1:nrow(Graph.1),subset,replace=FALSE)
      Graph.1<- Graph.1[subset.v,subset.v]
      Graph.2<- Graph.2[subset.v,subset.v]
      v_count<- subset
      
    }
    
    
  } else if (test.cases[test.case.it,"datasets"]=="cnet"){
    
    
    
    
    diag(Ajt1)<- 0
    diag(Ajt2)<- 0
    # Ajt1<-as.matrix(Ajt1)
    #  Ajt2<-as.matrix(Ajt2)
    sum_row_c = rowSums(Ajt1) #apply(Ajt1,1,sum)
    sum_col_c = colSums(Ajt1) #apply(Ajt1,2,sum)
    sum_row_g = rowSums(Ajt2) #apply(Ajt2,1,sum)
    sum_col_g = colSums(Ajt2) #apply(Ajt2,2,sum)
    
    disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
    v_count <- sum(!disc_v)
    save(file="charitynet_v_count_orig.txt",v_count,ascii=TRUE)
    Ajt1 <- Ajt1[!disc_v,!disc_v]
    Ajt2 <- Ajt2[!disc_v,!disc_v]
    
    graph.1 <- graph.adjacency(Ajt1,weighted=TRUE)
    graph.2 <- graph.adjacency(Ajt2,weighted=TRUE)
    
    
    graph.1  <- giant.component(graph.1)
    graph.2  <- giant.component(graph.2)
    V.common <- intersect(V(graph.1)$name,V(graph.2)$name)
    graph.1  <- induced.subgraph(graph.1,vids=V.common)
    graph.2  <- induced.subgraph(graph.2,vids=V.common)
    pruned.graph.size <- length(V.common)
    save(file="charitynet_v_count.txt",pruned.graph.size ,ascii=TRUE)
    write.graph(graph.1,"pruned_cnet_graph_1.gml",format="gml")
    write.graph(graph.2,"pruned_cnet_graph_2.gml",format="gml")
    corr_match_list_agg <-list()
    for (rep.seed.i in 1:rep.seeds) {
      if (!is.null(subset)&(subset<pruned.graph.size)){
        
        found.conn.subgraphs.in.both.graphs <- FALSE 
        v.cent<-0
        while ( !found.conn.subgraphs.in.both.graphs){
          v.cent <- sample (V(graph.1),1)
          found.conn.subgraph.1 <-NULL
          found.conn.subgraph.2 <-NULL
          found.neigh.size <-0
          prev.subgraph.1 <- v.cent 
          prev.subgraph.2 <- v.cent
          for (k in 1:15){
            #look at kth neighborhood of v.cent in first.graph
            subgraph.1<-(graph.neighborhood(graph.1,order=k,nodes=v.cent))[[1]]
            subgraph.vert.1 <- V(subgraph.1)$name
            #if # of vertices in subgraph is larger than subset
            if (length( subgraph.vert.1 )>=subset){
              # if largest component in graph.2 is large enough
              subgraph.2<- giant.component(induced.subgraph(graph.2, subgraph.vert.1))                                      
              if (vcount(subgraph.2)>=subset){
                subgraph.1<-induced.subgraph(subgraph.1,V(subgraph.2)$name)
                if (vcount(subgraph.1)>=subset) {
                  found.conn.subgraph.1 <- subgraph.1
                  found.conn.subgraph.2 <- subgraph.2
                  found.conn.subgraphs.in.both.graphs<- TRUE
                  found.neigh.size <- k
                  break
                }
              }
              
            }
            
          }
        }
        
        subgraph.verts.1.prev <- (neighborhood(graph.1,order=found.neigh.size-1,nodes=v.cent))[[1]]
        subgraph.2<- giant.component(induced.subgraph(graph.2, subgraph.verts.1.prev ))
        
        common.verts.prev<- intersect(V(graph.1)$name[subgraph.verts.1.prev ],V(subgraph.2)$name)
        common.verts <- intersect(V(found.conn.subgraph.1)$name,V(found.conn.subgraph.2)$name)
        
        needed.verts <- subset- length(common.verts.prev)
        sample.from.set.difference <- sample(setdiff(common.verts,common.verts.prev),needed.verts)
        common.verts.select<- union(common.verts.prev,sample.from.set.difference )
        
        found.conn.subgraph.1<- induced.subgraph(graph.1,vids=common.verts.select)
        found.conn.subgraph.2<- induced.subgraph(graph.2,vids=common.verts.select)
        
        Ac <- get.adjacency(found.conn.subgraph.1,attr="weight")
        Ag <- get.adjacency(found.conn.subgraph.2,attr="weight")
        v_count<- vcount(found.conn.subgraph.1)
      } else{
        Ac=Ajt1
        Ag=Ajt2
        
      }
      
      
      graph.is.directed <- TRUE
      
      Ac_graph <- Ac
      Ag_graph <- Ag
      
      if (symmetrize){
        graph.is.directed <- FALSE
        Ac_graph <- (Ac+t(Ac))/2
        Ag_graph <- (Ag+t(Ag))/2
      }
      
      if (!weighted.graph) {
        Ac_graph<- (Ac_graph>0)
        Ag_graph<- (Ag_graph>0)
      }
      Ac_graph<- (Ac_graph)+0
      Ag_graph<- (Ag_graph)+0
      
      
      
    }
    
  }
    n_v<- floor(v_count/2)
    
    
    sum_row_c = apply(Graph.1,1,sum)
    sum_col_c = apply(Graph.1,2,sum)
    sum_row_g = apply(Graph.2,1,sum)
    sum_col_g = apply(Graph.2,2,sum)
    
    disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
    Graph.1 <- Graph.1[!disc_v,!disc_v]
    Graph.2 <- Graph.2[!disc_v,!disc_v]
    v_count <- sum(!disc_v)
    
    graph.is.directed <- TRUE
    if (test.cases[test.case.it,"weighted"]){
      
      scale_f <- lm(as.vector(Graph.1) ~ as.vector(Graph.2) + 0)$coefficients
      Graph.1_graph <- Graph.1
      Graph.2_graph <- scale_f*Graph.2
      
      
      #symmetrize
      if (test.cases[test.case.it,"symmetrize"]){
        graph.is.directed <- FALSE
        Graph.1_graph <- (Graph.1_graph+t(Graph.1_graph))/2
        Graph.2_graph <- (Graph.2_graph+t(Graph.2_graph))/2
      }
    } else{
      Graph.1_graph <- Graph.1
      Graph.2_graph <- Graph.2
      
      if (test.cases[test.case.it,"symmetrize"]){
        graph.is.directed <- FALSE
        Graph.1_graph <- (Graph.1+t(Graph.1))/2
        Graph.2_graph <- (Graph.2+t(Graph.2))/2
      }
      
      Graph.1_graph<- (Graph.1_graph>0)+0
      Graph.2_graph<- (Graph.2_graph>0)+0
    }
    insample_logic_vec <- 1:N %in% sample(1:N,n_v,replace=FALSE) 
    insample_logic_vec <- c(insample_logic_vec,insample_logic_vec)
    
    graph.mode <-   ifelse(graph.is.directed,"directed","undirected")
    
    weight.igraph.param<-NULL
    weight.igraph.param <- if (test.cases[test.case.it,"weighted"]) {TRUE}
    G.M<-graph2dissimilarity(G=Graph.1_graph,Gp=Graph.2_graph,in.sample.ind=insample_logic_vec
                             , vert_diss_measure=test.cases[test.case.it,"dissim.meas"]
                             , w.vals.vec=0.8, graph.mode=graph.mode
                             , T.param=2, num_v_to_embed_at_a_time=1
                             , weighted.g= weight.igraph.param)
    print(test.cases[test.case.it,])
    print(str(G.M))
    print(sum(is.na(G.M)))
    
  }
  
  