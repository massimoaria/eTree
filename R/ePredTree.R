#' Predict responses through an explainable RF
#'
#' It predicts classification tree responses
#'
#' @param info bla bla bla
#' @param data bla bla bla
#' @param target bla bla bla
#'
#' @return an object.
#'
#'
#'
#' @examples
#'
#' @export
#'
ePredTree <- function(info, data, target="1"){
  #type can be type=c("value","prob")

  row.names(data)=NULL
  pred <- data.frame(fit=rep(NA,nrow(data)), accuracy=NA, score=NA, row.names = 1:nrow(data))

  # extracting paths of terminal nodes
  paths_list <- info[info$terminal==TRUE, c("node","path","pred","prob")]
  paths_list$prob = paths_list$score = as.numeric(paths_list$prob)

  ind <- which(paths_list$pred!=target)
  paths_list$score[ind] <- 1-paths_list$score[ind]

  # identifying obs by paths of terminal nodes and add predictions
  attach(data)
  for (i in 1:nrow(paths_list)){
    print(i)
    path <- paths_list[i,"path"]
    index <- row.names(data[eval(parse(text=path)),])
    pred[index,"fit"] <- paths_list[i,"pred"]
    pred[index,"accuracy"] <- paths_list[i,"prob"]
    pred[index,"score"] <- paths_list[i,"score"]

  }

  detach(data)
return(pred)

}



