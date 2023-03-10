#' Explainable Random Forest
#'
#' It creates an explainable tree for Random Forest
#'
#' @param Y bla bla bla
#' @param X bla bla bla
#' @param response bla bla bla
#' @param setting bla bla bla
#'
#' @return an object.
#'
#'
#'
#' @examples
#'
#' @export



eTree <- function(Y, X, response, setting=list(impTotal=0.1, maxDec=0.01, n=5, level=5, tMax=5)){


  for (i in 1:setting$level) setting$tMax=setting$tMax*2+1


  ## Generate the split matrix S from the original predictor matrix X
  res <- split(X)
  S <- res$S
  l <- res$lab
  rm(res)

  # Initilize node vector
  nodes <- rep(1,nrow(S))

  # list of no-terminal nodes
  nterm <- 1
  labels <- c("node","n", "pred", "prob", "impTotal", "impChildren","decImp","decImpSur","variable" ,"split", "splitLabel", "variableSur", "splitLabelSur","parent","children","terminal", "obs", "path")
  info <- data.frame(matrix(NA,setting$tMax,length(labels)))
  names(info) <- labels

  while(length(nterm)>0){

    t <- tail(nterm,1)
    print(t)
    index <- which(nodes == t)

        ### verifica regole di arresto
    results <- eStoppingRules(Y,index, t, setting, response)

    ### Calcolo risposta nel nodo (per ora solo classificazione)
    res <- moda(response[index])
    ###

    info$pred[t] <- res[1]
    info$prob[t] <- res[2]
    info$node[t] <- t
    info$parent[t] <- floor(t/2)
    info$n[t] <- length(index)
    info$impTotal[t] <- results$impTotal

    if (isTRUE(results$sRule)){
      #list(imp=NA,decImp=NA, split=NA, splitLabel=NA, terminal=TRUE, impTotal=results$impTotal, obs=index, path=NA)
      info$terminal[t] <- TRUE
      #info$impTotal[t] <- results$impTotal
      info$obs[t] <- list(index)
      info$path[t] <- paths(info,t)

      #### aggiungere la predizione
      #info[[t]]$pred <- prediction(Y[index])
      ####

    } else{
      imp<- eImpurity(Y,index,S)
      s <- which.min(imp)
      v <- gsub(" <=.*| %in%.*","",names(s))
      s2 <- which.min(imp[!(l %in% v)])

      # max decrease of impurity
      decImp <- results$impTotal-imp[s]
      decImpSur <- results$impTotal-imp[s2]
      #check for max impurity stopping rule
      if (decImp<=setting$maxDec){
        #info[[t]] <- list(imp=NA,decImp=NA, split=NA, splitLabel=NA, terminal=TRUE, parent= floor(t/2), children=NA, impTotal=results$impTotal, obs=index, path=NA)
        #info$parent[t] <- floor(t/2)
        info$terminal[t] <- TRUE
        #info$impTotal[t] <- results$impTotal
        info$obs[t] <- list(index)
        info$path[t] <- paths(info,t)
      } else{
        info$impChildren[t] <- as.numeric(imp[s])
        info$decImp[t] <- as.numeric(decImp)
        info$decImpSur[t] <- as.numeric(decImpSur)
        info$split[t] <- as.numeric(s)
        info$splitLabel[t] <- names(s)
        info$splitLabelSur[t] <- names(s2)
        info$terminal[t] <- FALSE
        info$parent[t] <- floor(t/2)
        info$children[t] <- list(c(t*2,t*2+1))
        info$impTotal[t] <- results$impTotal
        info$obs[t] <- list(index)
        info$path[t] <- paths(info,t)

        nodes[index] <- (nodes[index]*2+1)-S[index,s]
        nterm <- c(nterm, unique(nodes[index]))
      }
    }
    #print(info$path[t])
    nterm <- setdiff(nterm,t)

  }
  info <- info %>% drop_na(.data$node)
  info$variable <- gsub(" <=.*| %in%.*","",info$splitLabel)
  info$variableSur <- gsub(" <=.*| %in%.*","",info$splitLabelSur)
  return(info)
}



paths <- function(info,t){
  path <- NULL
  while(t[1]>1){
    tp <- floor(t[1]/2)
    symb <- ifelse((t[1] %% 2)==0,"","!")
    t <- c(tp,t)
    path <- c(paste(symb,info$splitLabel[tp],sep=""),path)
    #<- paste(path," AND ", symb,info[[tp]]$splitLabel, sep="",collapse="")
  }
  path <- paste(path,sep="",collapse=" & ")
  return(path)
}





#do.call("rbind", lapply(info, "[[", 1))

### extract data using path
#attach(ToothGrowth)
#ToothGrowth[eval(parse(text=info[[9]]$path)),]
