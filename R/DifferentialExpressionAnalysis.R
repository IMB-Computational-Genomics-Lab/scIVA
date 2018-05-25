#This script performs differential expression analysis in parallel

library(Matrix)
library(gdata)
library(R.utils)
library(DESeq)
library(foreach)
library(doParallel)

no_cores=8
cl<-makeCluster(no_cores-1)
registerDoParallel(cl)

load('dat_dcvl_mtrx_T_unLog_minus1_positive_BOC_for_edgeR.Obj')
short_pos <-t
load('my.clusters.Obj')

my.clusters <-as.factor(my.clusters)

short_pos <-round(short_pos+1)

DE_analysis <- function(data=short_pos,conditions=new_clusters) {
  cds = newCountDataSet(short_pos, conditions )
  cds = estimateSizeFactors( cds )
  head(sizeFactors( cds ))
  cds = estimateDispersions( cds, method='per-condition' , fitType = "local" )
  return(cds)
}

analyseCluster <- function(clusterID,cluster=my.clusters, mat=short_pos){
  # Pick out the cluster we are looking at.
  new_clusters <-as.vector(my.clusters)
  for (cl_id in unique(new_clusters)){
    if (cl_id != clusterID){
      cl_index <-which(as.character(new_clusters) == as.character(cl_id))
      mainCl_idx <- which(as.character(new_clusters) == as.character(clusterID))
      diff_cluster <- new_clusters[c(mainCl_idx, cl_index)]
      diff_mat <-short_pos[,c(mainCl_idx, cl_index)]
      print('start estimate dispersions')
      #cds <- DE_analysis(data=diff_mat, conditions=diff_cluster)
      cds = newCountDataSet(diff_mat, diff_cluster)
      cds = estimateSizeFactors( cds )
      head(sizeFactors( cds ))
      cds = estimateDispersions( cds, method='per-condition' , fitType = "local" )
      saveRDS(new_clusters, paste0('Reporting_Done_DE_doingnbinom_for_cluster_',clusterID, '_VS_cluster', cl_id) )
      print('Done estimate dispersions')
      res1 = nbinomTest( cds, as.character(clusterID), as.character(cl_id))
      print('Done nbinom')
      # Output to a text file
      filename <- paste0("DEseq_Cluster",clusterID,"_VS_Cluster",  cl_id, ".txt")
      #full_filename <- paste0(output_dir, sep="/",filename)
      write.table(res1, filename, quote=F, sep='\t', col.names=T)
      # Reset global
      res1 <- NULL
      cds <- NULL
    }
  }
}

clusters <- sort(unique(my.clusters))
foreach(x=clusters,.packages='DESeq', .export=c("my.clusters", "short_pos")) %dopar% {analyseCluster(x, my.clusters, short_pos)}

print("Differential expression complete!")

stopCluster(cl)

```
#Cell cycle analysis for supplementary figure
```{r}
library(scran)
load('dat_dcvl_mtrx_T_unLog_minus1_positive_BOC_for_edgeR.Obj')
ddSeqdat <-t

names <-gsub(".*_", "", row.names(ddSeqdat))

hs.pairs <- readRDS(system.file("exdata", "human_cycle_markers.rds", package="scran"))
ddSeqExprsMatrix <-as.matrix(ddSeqdat)
CCassignments <- cyclone(ddSeqExprsMatrix, pairs=hs.pairs, names)
saveRDS(CCassignments, 'Cell_cycle_assignment.RDS')
#load my.clusters object
load('my.clusters.Obj')
#count cells
CC_clusters <- as.data.frame('phases'=CCassignments$phases, 'clusters'=my.clusters)
CC_clusters %>% count(clusters, phases)
CC_clusters %>% count(clusters, phases)
