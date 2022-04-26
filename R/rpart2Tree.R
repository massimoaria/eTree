rpart2Tree <- function(info){
  frame <- info %>%
    select(.data$node,.data$variable,.data$n,.data$pred,.data$prob,.data$decImp) %>%
    rename(var=.data$variable,
           yval=.data$pred) %>%
    mutate(wt=.data$n,
           ncompete=0,
           nsurrogate=0,
           yval2=.data$yval,
           complexity=1-as.numeric(.data$prob),
           dev=round(.data$n*(1-as.numeric(.data$prob)))) %>%
    as.data.frame()
  rownames(frame) <- frame$node
  frame$var[is.na(frame$var)] <- "<leaf>"
  frame$complexity[is.na(frame$complexity)] <- 0.01
  frame <- frame[,c("var","n","wt","dev","yval","complexity","ncompete","nsurrogate","yval2")]
  obs <- info1 %>%
    dplyr::filter(.data$terminal==TRUE) %>%
    select(.data$node,.data$n,.data$obs)
  where <- rep(obs$node,obs$n)
  names(where) <- do.call(c,obs$obs)
  where <- where[order(as.numeric(names(where)))]
  obj <- list(frame=frame, where=where)
  class(obj) <- "rpart"
  return(obj)
}
