#' @description To filter cell outliers
#' @param to be added
#'
outlierRemoval <- reactive({
  expression.matrix <- as.data.frame(dataExpression())
  ## Subset the data
  totalCells <- ncol(expression.matrix)

  #Checking cells
  LibSizes <- colSums(expression.matrix)
  LibSizes <- as.numeric(as.vector(LibSizes))
  nDetectedGenes <- apply(expression.matrix, 2, function(x){sum(x>0)})
  nDetectedGenes <- as.numeric(as.vector(nDetectedGenes))
  ##threshold from slider
  mad_cutoff <- input$mad_cutoff

  lowerMAD1 <- median(nDetectedGenes) - mad_cutoff*mad(nDetectedGenes)
  higherMAD1 <- median(nDetectedGenes) + mad_cutoff*mad(nDetectedGenes)
  Cells_removed_MADgenes <- which((nDetectedGenes < lowerMAD1) | (nDetectedGenes > higherMAD1))

  lowerMAD2 <- median(LibSizes) - mad_cutoff*mad(LibSizes)
  higherMAD2 <- median(LibSizes) +  mad_cutoff*mad(LibSizes)
  Cells_removed_MAD_libsizes <- which((LibSizes < lowerMAD2)  | (LibSizes >  higherMAD2))

  #Checking genes
  pcExprCells <- apply(expression.matrix, 1, function(x){sum(x>0)})/totalCells*100
  pcExprCells <- as.numeric(as.vector(pcExprCells))
  lowerMAD3 <- median(pcExprCells) - mad_cutoff*mad(pcExprCells)
  higherMAD3 <- median(pcExprCells) + mad_cutoff*mad(pcExprCells)

  Genes_removed_MAD_pcExprs <- which((pcExprCells < lowerMAD3) | (pcExprCells >  higherMAD3))

  ##percent expression threshold from slider
  gene_lower_than_threshold <- which(pcExprCells < input$gene_cutoff)

  #final data to keep
  cells_to_remove <- unique(c(Cells_removed_MADgenes,Cells_removed_MAD_libsizes))
  genes_to_remove <- gene_lower_than_threshold
  data_kept <- expression.matrix[-genes_to_remove, -cells_to_remove]
  cluster_all <-dataCluster()
  cluster_kept <- cluster_all[-cells_to_remove]
  # output dataframe
  combined.data <- list(OutlierCellsLibSizes = length(Cells_removed_MAD_libsizes),
                        OutlierCellsDetectedGenes = length(Cells_removed_MADgenes),
                        OutlierGenesMADExprs = length(Genes_removed_MAD_pcExprs),
                        OutlierGenesPercentExprs = length(gene_lower_than_threshold))
  combined.df <- as.data.frame(combined.data)
  filtered.df <- data_kept
  list_output <-list("summary"=combined.df, "data" = filtered.df, "cluster"= cluster_kept)
})

output$outlierRemoved <- renderTable({
  as.matrix(outlierRemoval()$summary)
},bordered = TRUE)

#' @description To normalise the filtered dataset
#' @param to be added

normalisation <- reactive({
  expression.matrix <- as.data.frame(outlierRemoval()$data)
  if(input$normOption == "CPM"){
    dat_norm <-apply(expression.matrix, 2, function(x){(x/sum(x))*1000000})
  }else if (input$normOption == "Quantile"){
    #Need to cite quantile method in the preprocessCore package
    dat_norm <-normalize.quantiles(as.matrix(expression.matrix))
    dat_norm <-as.data.frame(dat_norm)
  }else if(input$normOption == "RLE"){
    #Need to cite RLE method in the ascend package
    CalcNormFactor <- function(x, geo.means) {
      x.geo.means <- cbind(x, geo.means)
      x.geo.means <- x.geo.means[(x.geo.means[, 1] > 0), ]
      non.zero.median <- median(apply(x.geo.means, 1, function(y) {
        y <- as.vector(y)
        y[1]/y[2]
      }))
      return(non.zero.median)
    }

    CalcGeoMeans <- function(x) {
      x <- x[x > 0]
      x <- exp(mean(log(x)))
      return(x)
    }
    geo.means <- apply(expression.matrix, 1, CalcGeoMeans)

    norm.factor <- apply(expression.matrix, 2, function(x) CalcNormFactor(x, geo.means))

    dat_norm <- t(t(expression.matrix)/norm.factor)
  }else{
    dat_norm <- expression.matrix
  }
})


output$downloadNormData = downloadHandler(
  #s = input$x1_rows_all
  filename = function() {
    paste("norm_output_data_", Sys.time(),".csv", sep="")
  },
  content = function(file) {
    write.csv(normalisation(), file)
  }
)

output$downloadClusterData = downloadHandler(
  #s = input$x1_rows_all
  filename = function() {
    paste("filtered_cluster_data_", Sys.time(),".csv", sep="")
  },
  content = function(file) {
    write.csv(outlierRemoval()$cluster, file,row.names = FALSE)
  }
)
normalisedPlot <-reactive({
  datNorm <-normalisation()
  libSizes <-colSums(datNorm)
  libSizesdf <-as.data.frame(libSizes)
  p <- qplot(libSizesdf, geom="histogram")
  p
})


output$libSizeExprsPostNorm <- renderPlotly(
  normalisedPlot()
)
