#' @description generate heatmap plot
#' @param to be added
#'
output$heatmap2 <- renderPlotly({
  DE <- dataExpressionGeneList()
  #totalReadsPerGenes <- rowSums(DE)
  DE <-log2(DE+1)
  k_row_input <-input$heatmap2Input
  if (input$Scaling == 'N') {
    p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL) #, scale="row"
  } else {
    DE <- scale(DE, center = TRUE, scale = TRUE)
    p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL, scale="row")
  }

  plotHeight <- as.character(120+nrow(dataExpressionGeneList())*15)

  p2 <- p %>% layout(height = plotHeight, autosize = TRUE,
                     margin = list(l = 250,r = 0,b = 25,t = 75,pad = 0)
  )
  p2
})

#' @description generate enriched Reactome pathway plot
#' @param to be added
#'

output$reactome <- renderForceNetwork({
  Genes <- dataGeneList()[,1]
  Genes <- gsub("_.*","",Genes)
  convert_to_gene_ID = bitr(Genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = "org.Hs.eg.db")
  converted_genes = convert_to_gene_ID$ENTREZID
  #totalReadsPerGenes <- rowSums(DE)
  p_cutoff <-input$p_cutoff
  x <- enrichPathway(gene=converted_genes,pvalueCutoff= p_cutoff, readable=TRUE)
  p <- enrichMap(x, layout=igraph::layout.kamada.kawai, vertex.label.cex = 1)
  k <- igraph_to_networkD3(p)
  k$nodes$group <- as.factor(matrix(1,length(k$nodes$name),1))
  iD3 <- forceNetwork(Links = k$links, Nodes = k$nodes,Source = "source", Target = "target",
                      Value = "value", NodeID = "name", Group = "group",
                      linkDistance = JS("function(d){return d.value * 15}"),
                      fontSize = 18, charge = -20, bounded = TRUE)
  return(iD3)
})
