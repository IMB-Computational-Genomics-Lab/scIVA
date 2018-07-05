# scIVA
_scIVA_ is an interactive web-tool for visualisation and analysis of single cell RNA-Seq data.

## Overview

#### Upload Data
Users are required to upload:

* Gene Expression Matrix
* Cluster Information Matrix

Users can then upload:

* Cluster List, to subset the data by Clusters
* Gene List, to subset the data by Specific Genes

_scIVA_ allows for flexible data uploading, with multiple formats accepted, including csv, tsv, and xlsx, each with or without quotations. It also provides options to transpose the data matrices and choose headers from the uploaded dataset. Following the initial upload, the user can subset data by clusters, or upload a gene list to subset by genes, giving greater control to the user. This then gives a preview of the uploaded data, with cell, gene and cluster counts.

#### Quality Control

_scIVA_ gives users access to a range of quality control measures for cells, including an interactive Beeswarm plot to compare between clusters or experimental design conditions. Sequencing depth quality control shows mean expression with proportions of cells expressing the gene. Also included is a ranking system of genes by mean and variance across the whole dataset and across clusters. This option facilitates gene selection based on gene expression pattern. _scIVA_ also allows for data to be normalized using different procedures, and gives users control to filter outlier genes from the dataset. The resulting data can be downloaded and re-uploaded for subsequent analysis.

#### Single Gene Visualisation

_scIVA_ has a comprehensive range of interactive visualisation and statistical tests for examining changes of any selected gene between clusters. The key features of _scIVA_ is to provide the ability to visualize the data in an intuitive and interactive way. The Sunburst plot generates a layered graph of the proportion of cells with/without expression of the genes, displaying counts and percentage representation. The Beeswarm plot graphs the proportion of expressed cells and cell-specific expression level by clusters. Multiple density plots are generated to show the distribution of gene expression between clusters for all and for zero-filtered cells. A Summary expression table is generated, which displays counts, percentages and means for each cluster across all cells and positively expressed cells.

#### Single Gene Analysis

_scIVA_ provides summary statistics in data browsing table by cells and clusters. To first identify genes with potential expression changes, a Kruskal-Wallis test is performed for all clusters, followed by a Kolmogorov-Smirnov test for pair-wise statistical differences in the distributions of gene expression between clusters. A modified form of likelihood ratio test, taking into account zero-inflated distribution, is provided to quantitatively perform differential expression analysis at gene level between clusters and to estimate fold change.

#### Gene List Analysis

_scIVA_ provides analysis tools for a user uploaded list of multiple genes. Features include reactome pathway analysis, with p-value cutoff for enrichment tests which displays interactive genetic pathways. The user can also choose to cluster by selected genes, with options to choose the number of clusters, and whether to scale by row, with results displayed as a heatmap with dendrogram to illustrate the arrangements of clustering.

## Running the web-tool
The following packages must be installed
```R
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
```
Each packages needs to be loaded from the library
```R
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
```

Once the packages have been installed, the app can be ran with the following line.
```R
runGitHub("scIVA", "IMB-Computational-Genomics-Lab")
```
