createDisMatrix <- function(rf, data){

  # predList <- lapply(seq_len(rf$ntree), function(z)
  #   predict(rf, newdata = training[rf$inbag[, z] == 1, ], nodes = TRUE))

  ## osservazioni utilizzate nell'albero

  # obs_nodes <- lapply(predList, function(l){
  #   l <- as.data.frame(attr(l,"nodes"))
  #   l <- cbind(row.names(l),l)
  #   names(l) <- c("OBS",paste("Tree",seq(1,(ncol(l)-1)), sep=""))
  #   return(l)
  # })
  #
  # obs <- do.call(rbind,obs_nodes)
  # obs <- obs %>% distinct(.data$OBS, .keep_all = TRUE)

  obs <- as.data.frame(attr(predict(rf, newdata = data, nodes = TRUE),"nodes"))
  obs <- cbind(row.names(obs), obs)
  names(obs) <- c("OBS",paste("Tree",seq(1,(ncol(obs)-1)), sep=""))
  row.names(obs)=NULL
  #obs <- obs %>% arrange(as.numeric(.data$OBS))

  nodes <- sort(unique(as.numeric(as.matrix(((obs %>%
                                                select(starts_with("Tree"))))))))

  #risposta
  obs$resp <- rf$y

  ## matrice dei tassi di ben classificati (Obs x Trees)
  w <- matrix(NA,nrow(obs),rf$ntree)
  a <- Matrix(0, nrow(obs),nrow(obs), sparse = TRUE)
  for (i in seq_len(rf$ntree)){
    print(i)
    # Tasso ben classificati in tutti i nodi terminali dell'i-esimo albero
    R <- obs %>% group_by_at(i+1) %>%
      mutate(n = n()) %>%
      summarize(freq = sort(table(.data$resp), decreasing=T)[1]/n) %>%
      slice(1) %>% as.data.frame()
    w[,i] <- R[as.numeric(factor(obs[,i+1])),2]
    # calcolo co-presenza pesata tra le osseravzioni
    a <- a+((outer(obs[,i+1],obs[,i+1],"==")+0)*w[,i])
  }


  ## a è la matrice di similarità tra le osservazioni

  ## aa è la matrice di similarità massima tra le osservazioni (max(Ci,Cj))
  aa=diag(a)

  aa=outer(aa,aa,"maxValue")

  a <- a/aa

  ## Dissimilarity matrix among observations (respect to the co-occurrences in the same node)
  dis <- 1-a
  row.names(dis)=colnames(dis)=obs$OBS

  return(dis)
}

maxValue <- function(x,y){
  apply(cbind(x,y),1,max)
}

