#' @description generate table input for Sunburst
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
