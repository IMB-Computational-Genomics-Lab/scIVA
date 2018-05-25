library('shiny')
library('plotly')
library('RColorBrewer')
library('sunburstR')
library('readr')
library('readxl')
library('d3r')
library('treemap')
library('ggplot2')
library('grid')
library('randomcoloR')
library('DT')
library('shinythemes')
library('webshot')
library('data.table')
library('heatmaply')
library('ReactomePA')
library('org.Hs.eg.db')
library('clusterProfiler')
library('networkD3')



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

