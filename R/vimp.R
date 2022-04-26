#' Variable Importance
#'
#' It calculate variable importance of an explainable tree
#'
#' @param info bla bla bla
#' @param response bla bla bla
#' @param X bla bla bla
#' @param graph bla bla bla
#'
#' @return an object.
#'
#'
#'
#' @examples
#'
#' @export
#'
vimp <- function(info, response, X, graph=TRUE){

  ## Calcolo decrease of Accuracy ##
  row.names(info) <- info$node
  info$prob <- as.numeric(info$prob)
  t <- row.names(info)[info$terminal==FALSE]
  tL <- as.character(info[t,"node"]*2)
  tR <- as.character(info[t,"node"]*2+1)
  info[t,"probChildren"] <- (info[tL,"prob"]*info[tL,"n"]/info[t,"n"]) + (info[tR,"prob"]*info[tR,"n"]/info[t,"n"])
  #attach(X)
  names(response) <- row.names(X)
  for (i in t){
    #iL <- as.character(unlist(info[i,"children"])[1])
    #iR <- as.character(unlist(info[i,"children"])[2])
    obs <- row.names(X)[unlist(info[i,"obs"])]
    path <- info[i,"splitLabelSur"]
    x <- X[obs,]
    indL <- obs[eval(parse(text=paste("x$",path)))]
    indR <- setdiff(obs,indL)
    probL <- as.numeric(moda(response[indL])[2])
    probR <- as.numeric(moda(response[indR])[2])
    info[i,"probChildrenSur"] <- (probL*length(indL)/info[i,"n"]) + (probR*length(indR)/info[i,"n"])
  }


  info[t,"decProb"] <- info[t,"probChildren"]-info[t,"probChildrenSur"]


  vimp_resp <- info %>%
    mutate(Nimp = .data$decImp*.data$n/.data$n[1]) %>%
    group_by(.data$variable,.data$pred) %>%
    summarize(vimp = sum(.data$Nimp)) %>%
    drop_na(.data$variable) %>%
    pivot_wider(names_from = .data$pred, values_from = .data$vimp)

  names(vimp_resp)[-1] <- paste("ImpDec_",names(vimp_resp)[-1])

  vimp_prob <- info %>%
    mutate(Pimp = .data$decProb*.data$n/.data$n[1]) %>%
    group_by(.data$variable,.data$pred) %>%
    summarize(pimp = sum(.data$Pimp)) %>%
    drop_na(.data$variable) %>%
    pivot_wider(names_from = .data$pred, values_from = .data$pimp)

  names(vimp_prob)[-1] <- paste("AccDec_",names(vimp_prob)[-1])

vimp <- info %>%
  mutate(Nimp = .data$decImp*.data$n/.data$n[1],
         Pimp = .data$decProb*.data$n/.data$n[1]) %>%
  group_by(.data$variable) %>%
  summarize(vimp = sum(.data$Nimp, na.rm=TRUE),
            pimp = sum(.data$Pimp, na.rm=TRUE)) %>%
  drop_na(.data$variable) %>%
  # mutate(vimpNorm = .data$vimp/sum(.data$vimp)*100,
  #        pimpNorm = .data$pimp/sum(.data$pimp)*100) %>%
  arrange(desc(.data$vimp), by_group=FALSE) %>%
  left_join(vimp_resp, by = "variable") %>%
  left_join(vimp_prob, by = "variable")

names(vimp)[1:3] <- c("Variable","MeanImpurityDecrease","MeanAccuracyDecrease")

# Minimal theme + blue fill color
if (isTRUE(graph)){
  pImp <-ggplot(vimp, aes(y=.data$Variable, x=.data$MeanImpurityDecrease)) +
    geom_bar(stat="identity", fill="steelblue") +
    scale_y_discrete(limits = rev(vimp$Variable))+
    labs(title="Variable Importance Plot", x = "Mean Impurity Decrease", y = "Variance") +
    theme_minimal()
  plot(pImp)

  pAcc <-vimp %>%
    arrange(desc(.data$MeanAccuracyDecrease), by_group=FALSE)
  pAcc <- pAcc %>%
    ggplot(aes(y=.data$Variable, x=.data$MeanAccuracyDecrease)) +
    geom_bar(stat="identity", fill="steelblue") +
    scale_y_discrete(limits = rev(pAcc$Variable))+
    labs(title="Variable Importance Plot", x = "Mean Accuracy Decrease", y = "Variance") +
    theme_minimal()
  plot(pAcc)
}
return(vimp)
}
