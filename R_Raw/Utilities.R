#' @description generate interative table input for Sunburst
#' @param stepCluster a column of clusters for Sunburst table
#' @param dGE a column of gene expression values
#' @param threshold a numeric cutoff
#' @return a lsit of 5 values

summary_data_big <- function(stepCluster, dGE, threshold){
  cellCount <- length(which(dGE$Cluster==stepCluster))
  pos_idx <- which((dGE$Cluster==stepCluster) & (dGE$Gene>threshold))
  posCount <- length(pos_idx)
  posPercent <- posCount/cellCount*100
  dGEC <- dGE$Gene[dGE$Cluster==stepCluster]
  meanExprs <- mean(dGEC)
  meanPositive <- mean(dGEC[dGEC>threshold])
  return(list(cellCount,posCount,round(posPercent,3),round(meanExprs,3),round(meanPositive,3)))
  cluster_exprs=NULL
}


#' @description to prepare input for Sunburst plot
#' @param stepCluster
#' @param dGE
#' @param threshold

summary_data_sunburst <- function(stepCluster, dGE, threshold){
  cellCount <- length(which(dGE$Cluster==stepCluster))
  pos_idx <- which((dGE$Cluster==stepCluster) & (dGE$Gene>threshold))
  posCount <- length(pos_idx)
  return(list(cellCount,posCount))
}

#' @description to instantiate rows
#' @param x
#' @param n

rep.row<-function(x,n) {
  matrix(rep(x,each=n),nrow=n)
}

#' @description to instantiate columns
#' @param x
#' @param n

rep.col<-function(x,n) {
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

#' @description to prepare input for Sunburst plot
#' @param x
#' @param n
#'
thresholdCheck <- function(x){
  if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

#' @description to prepare input for Sunburst plot
#' @param x
#'
varMean <- function(x) {
  if (mean(x) > 0) {
    y <- var(x)/mean(x)
  } else {
    y<-0
  }
  return(y)
}
