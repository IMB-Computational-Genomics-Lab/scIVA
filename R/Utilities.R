#' @description to prepare input for Sunburst plot
#' @param

summary_data_sunburst <- function(stepCluster, dGE, threshold){
  cellCount <- length(which(dGE$Cluster==stepCluster))
  pos_idx <- which((dGE$Cluster==stepCluster) & (dGE$Gene>threshold))
  posCount <- length(pos_idx)
  return(list(cellCount,posCount))
}

rep.row<-function(x,n) {
  matrix(rep(x,each=n),nrow=n)
}

rep.col<-function(x,n) {
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

thresholdCheck <- function(x){
  if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

varMean <- function(x) {
  if (mean(x) > 0) {
    y <- var(x)/mean(x)
  } else {
    y<-0
  }
  return(y)
}
