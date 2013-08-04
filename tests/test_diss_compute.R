# test dissimilarities for all graphs
datasets <- c("worm","enron","wiki","cnet")
weighted <- c(TRUE,FALSE)
symmetrize <- c(TRUE,FALSE)
normalize.diss<-c(TRUE,FALSE)

dissim.meas<- c('default',"diffusion",'ECT','ell1','jGraph.1card'
                , 'dice','invlogweighted','C_dice_weighted'
                ,'hybrid_DICE_SP')
test.cases <- expand.grid(datasets,weighted,symmetrize,normalize.diss)
colnames(test.caste) <-("datasets","weighted","symmetrize","normalize.diss")

load("./data/celegansGraph.Rd")
load("./data/AAA-187As-184x184.Rbin")
load("./data/Wiki_adj.RData")

load("./data/Ajt1-5699.Rbin")
load("./data/Ajt2-5699.Rbin")

num.test.cases<- dim(test.cases)[1]
for (test.case.it in 1:num.test.cases){
  
  if (test.case[test.case.it,"datasets"]=="worm"){
    Graph.1 <- Ac
    Graph.2 <- Ag
  } else if (test.case[test.case.it,"datasets"]=="enron"){
    Graph.1<- AAA[130]
    Graph.2 <- AAA[131]
  } else if (test.case[test.case.it,"datasets"]=="wiki"){
    en_a<- graph.adjacency(en_a)
    fr_a<- graph.adjacency(fr_a)
    #en_a_cl <- giant.component(en_a)
    #fr_a_cl <- giant.component(fr_a)
    subset = 400
    Graph.1 <- en_a
    Graph.2 <- fr_a
    
   
    
  } else if (test.case[test.case.it,"datasets"]=="cnet"){
    Graph.1<- as.matrix(Ajt1)
    Graph.2<- as.matrix(Ajt2)
  }
  
  
  N<-nrow(Graph.1)
  
  if (!is.null(subset)& subset<N){
    subset.v <-sample (1:nrow(Graph.1),subset,replace=FALSE)
    Graph.1<- Graph.1[subset.v,subset.v]
    Graph.2<- Graph.2[subset.v,subset.v]
    v_count<- subset
    
    
  }
  
  
  n_v<- floor(N/2)
 
  
  sum_row_c = apply(Graph.1,1,sum)
  sum_col_c = apply(Graph.1,2,sum)
  sum_row_g = apply(Graph.2,1,sum)
  sum_col_g = apply(Graph.2,2,sum)
  
  disc_v <- ((sum_col_c==0)&(sum_row_c==0)) | ((sum_col_g==0) & (sum_row_g==0))
  Graph.1 <- Graph.1[!disc_v,!disc_v]
  Graph.2 <- Graph.2[!disc_v,!disc_v]
  v_count <- sum(!disc_v)
  
  graph.is.directed <- TRUE
  if (test.case[test.case.it,"weighted"]){
    
    scale_f <- lm(as.vector(Graph.1) ~ as.vector(Graph.2) + 0)$coefficients
    Graph.1_graph <- Graph.1
    Graph.2_graph <- scale_f*Graph.2
    
    
    #symmetrize
    if (test.case[test.case.it,"symmetrize"]){
      graph.is.directed <- FALSE
      Graph.1_graph <- (Graph.1_graph+t(Graph.1_graph))/2
      Graph.2_graph <- (Graph.2_graph+t(Graph.2_graph))/2
    }
  } else{
    Graph.1_graph <- Graph.1
    Graph.2_graph <- Graph.2
    
    if (test.case[test.case.it,"symmetrize"]){
      graph.is.directed <- FALSE
      Graph.1_graph <- (Graph.1+t(Graph.1))/2
      Graph.2_graph <- (Graph.2+t(Graph.2))/2
    }
    
    Graph.1_graph<- (Graph.1_graph>0)
    Graph.2_graph<- (Graph.2_graph>0)
  }
  insample_logic_vec <- 1:N %in% sample(1:N,n_v,replGraph.1e=FALSE) 
  insample_logic_vec <- c(insample_logic_vec,insample_logic_vec)
  
  graph.mode <-   ifelse(graph.is.directed,"directed","undirected")
  
  G.M<-graph2dissimilarity(G=Graph.1_graph,Gp=Graph.2_graph,in.sample.ind=insample_logic_vec
                           , vert_diss_measure=test.case[test.case.it,"dissim.meas"]
                           ,  d.dim=10, w.vals.vec=0.8, graph.mode=graph.mode
                           , T.param=2, num_v_to_embed_at_a_time=1
                           , weighted.g=test.case[test.case.it,"weighted"])
  
}

