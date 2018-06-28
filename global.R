install.packages('shiny')
install.packages('plotly')
install.packages('RColorBrewer')
install.packages('sunburstR')
install.packages('readr')
install.packages('readxl')
install.packages('d3r')
install.packages('treemap')
install.packages('ggplot2')
install.packages('grid')
install.packages('randomcoloR')
install.packages('DT')
install.packages('shinythemes')
install.packages('webshot')
install.packages('data.table')
install.packages('heatmaply')
source("https://bioconductor.org/biocLite.R")
biocLite("ReactomePA")
biocLite('org.Hs.eg.db')
biocLite('clusterProfiler')
install.packages('networkD3')
install.packages('ggraph')
install.packages('igraph')
install.packages('ggbeeswarm')

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
library('ggraph')
library('igraph')
library('ggbeeswarm')

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


overlap_ratio <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)
  length(intersect(x, y))/length(unique(c(x,y)))
}

update_n <- function(x, showCategory) {
  if (!is.numeric(showCategory)) {
    return(showCategory)
  }

  ## geneSets <- geneInCategory(x) ## use core gene for gsea result
  n <- showCategory
  if (nrow(x) < n) {
    n <- nrow(x)
  }

  return(n)
}


emapplot.enrichResult <- function(x, showCategory = 30, color="p.adjust", layout = "kk", ...) {
  n <- update_n(x, showCategory)
  geneSets <- geneInCategory(x) ## use core gene for gsea result
  y <- as.data.frame(x)
  y <- y[1:n,]

  if (n == 0) {
    stop("no enriched term found...")
  } else if (n == 1) {
    g <- graph.empty(0, directed=FALSE)
    g <- add_vertices(g, nv = 1)
    V(g)$name <- y$Description
    V(g)$color <- "red"
    return(ggraph(g) + geom_node_point(color="red", size=5) + geom_node_text(aes_(label=~name)))
  } else {
    id <- y[,1]
    geneSets <- geneSets[id]

    n <- nrow(y) #
    w <- matrix(NA, nrow=n, ncol=n)
    colnames(w) <- rownames(w) <- y$Description

    for (i in 1:n) {
      for (j in i:n) {
        w[i,j] = overlap_ratio(geneSets[id[i]], geneSets[id[j]])
      }
    }

    wd <- melt(w)
    wd <- wd[wd[,1] != wd[,2],]
    wd <- wd[!is.na(wd[,3]),]
    g <- graph.data.frame(wd[,-3], directed=FALSE)
    E(g)$width=sqrt(wd[,3] * 5)
    g <- delete.edges(g, E(g)[wd[,3] < 0.2])
    idx <- unlist(sapply(V(g)$name, function(x) which(x == y$Description)))

    cnt <- sapply(geneSets[idx], length)
    V(g)$size <- cnt

    colVar <- y[idx, color]
    V(g)$color <- colVar
  }

  return(g)
}

