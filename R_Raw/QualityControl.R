#' @description generate heatmap plot
#' @param to be added
#'
output$heatmap <- renderPlotly({

  if (input$clusterOrderType == 'G') {
    init <- Sys.time()
    maExpression <- as.matrix(dataExpression())
    maDCN <- as.matrix(dataClusterNames())
    DE <- matrix(0,nrow(maExpression),nrow(maDCN))
    rownames(DE) <- rownames(maExpression)

    for  (i in 1:nrow(maDCN)) { # Use apply
      DE[,i] <- rowMeans(maExpression[,dataCluster() == maDCN[i,1]])
    }
    DE <- as.data.frame(DE)
    print(Sys.time() - init)

  } else {
    DE <- dataExpression()
  }

  if (input$geneOrderType == 'VM') {
    dfExpressionOrdered <- DE[order(as.data.frame(apply(DE, 1, varMean))),] # rowMeans
    heatTitle <- c("Gene Expression Heatmap<br>Ascending Variance/Mean Ordered")
  } else if (input$geneOrderType == 'M') {
    dfExpressionOrdered <- DE[order(as.data.frame(rowMeans(DE))),] # rowMeans
    heatTitle <- c("Gene Expression Heatmap<br>Ascending Mean Ordered")
  } else if (input$geneOrderType == "V") {
    dfExpressionOrdered <- DE[order(as.data.frame(apply(DE, 1, var))),]
    heatTitle <- c("Gene Expression Heatmap<br>Ascending Variance Ordered")
  } else {
    dfExpressionOrdered <- as.data.frame(DE)
    heatTitle <- c("Gene Expression Heatmap<br> ")
  }

  if (is.null(dataGeneList())) {
    numRow <- nrow(dfExpressionOrdered)
    dfExpressionOrdered <- dfExpressionOrdered[(numRow-51):numRow,]
    plotHeight <- as.character(870)
  } else {
    plotHeight <- as.character(120+nrow(dfExpressionOrdered)*15)
  }

  genenames.text <- rep.col(rownames(dfExpressionOrdered), ncol(dfExpressionOrdered))

  if (input$clusterOrderType == 'G') {
    clusternames.text <- rep.row(maDCN, nrow(dfExpressionOrdered))
  } else {
    clusternames.text <- rep.row(as.matrix(dataCluster()), nrow(dfExpressionOrdered))
  }

  combined.text <- matrix(paste('Gene: ', genenames.text,
                                '<br> Cluster: ', clusternames.text,
                                '<br> Expression: ', as.matrix(dfExpressionOrdered)),
                          nrow(dfExpressionOrdered), ncol(dfExpressionOrdered))

  h <- plot_ly(y = rownames(dfExpressionOrdered),
               z = log2(as.matrix(dfExpressionOrdered)+1),
               type = "heatmap",
               colors = colorRamp(c("#E5E5E5", "#FF0000")),
               hoverinfo = 'text',
               text = combined.text) %>%
    layout(
      title = heatTitle,
      height = plotHeight, autosize = TRUE,
      margin = list(l = 250,r = 0,b = 25,t = 75,pad = 0))
  h
})

#' @description generate library size beeswarm plot
#' @param to be added
#'

libSizeExprsGenesFull <- reactive({
  palette <- clusterPalette()
  expression.matrix <- as.data.frame(dataExpression())
  cell.information <- dataGeneExpressionCluster()$Cluster
  batch.names <- unique(cell.information)
  ## Subset the data
  total.counts <- colSums(expression.matrix)
  total.features <- apply(expression.matrix, 2, function(x){sum(x>0)})
  ## Tidy dataframe
  combined.data <- list(TotalCounts = total.counts, FeatureCounts = total.features, BatchInfo = cell.information)
  combined.df <- as.data.frame(combined.data)


  # ggplot
  gradient.ramp <- ggplot2::scale_colour_gradient(low="#001b7f", high="#f1d351")
  control.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = factor(BatchInfo), y = TotalCounts, colour = FeatureCounts))
  control.plot <- control.plot + ggplot2::geom_violin(size = 1, scale = "width", colour = NA) + ggbeeswarm::geom_quasirandom(shape = 16, size=5, alpha=0.5, dodge.width=0.5, groupOnX = T)
  control.plot <- control.plot + gradient.ramp + ggplot2::scale_x_discrete(limits = batch.names) + ggplot2::xlab("Sample") + ggplot2::ylab("Total mapped reads per cell")
  control.plot <- control.plot + ggplot2::ggtitle("Total mapped reads and genes per sample")
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(control.plot)  %>% layout(margin = m)
  p
})


libSizeExprsGenesFull <- reactive({
  palette <- clusterPalette()
  expression.matrix <- as.data.frame(dataExpression())
  cell.information <- dataCluster()[,1]
  batch.names <- unique(cell.information)
  ## Subset the data
  total.counts <- colSums(expression.matrix)
  total.features <- apply(expression.matrix, 2, function(x){sum(x>0)})
  ## Tidy dataframe
  combined.data <- list(TotalCounts = total.counts, FeatureCounts = total.features, BatchInfo = cell.information)
  combined.df <- as.data.frame(combined.data)

  # ggplot
  gradient.ramp <- ggplot2::scale_colour_gradient(low="#001b7f", high="#f1d351")
  control.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = factor(BatchInfo), y = TotalCounts, colour = FeatureCounts))
  control.plot <- control.plot + ggplot2::geom_violin(size = 1, scale = "width", colour = NA) + ggbeeswarm::geom_quasirandom(shape = 16, size=5, alpha=0.5, dodge.width=0.5, groupOnX = T)
  control.plot <- control.plot + gradient.ramp + ggplot2::scale_x_discrete(limits = batch.names) + ggplot2::xlab("Sample") + ggplot2::ylab("Total mapped reads per cell")
  control.plot <- control.plot + ggplot2::ggtitle("Total mapped reads and genes per sample")
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(control.plot)  %>% layout(margin = m)
  p
})

output$QClibSizeExprsGenesFull <- renderPlotly(libSizeExprsGenesFull())

GeneFull <- reactive({
  expression.matrix <- as.data.frame(dataExpression())
  expression.gene <- dataGeneExpressionCluster()$Gene
  cell.information <- dataGeneExpressionCluster()$Cluster
  batch.names <- unique(cell.information)
  ## Subset the data
  total.counts <- colSums(expression.matrix)
  gene.percent <- expression.gene/total.counts*100
  ## Tidy dataframe
  combined.data <- list(genePercent = gene.percent , GeneLevel = expression.gene, BatchInfo = cell.information)
  combined.df <- as.data.frame(combined.data)
  # ggplot
  gradient.ramp <- ggplot2::scale_colour_gradient(low="#001b7f", high="#f1d351")
  Gene.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = factor(BatchInfo), y = genePercent, colour = GeneLevel))
  Gene.plot <- Gene.plot + ggplot2::geom_violin(size = 1, scale = "width", colour = NA) +
    ggbeeswarm::geom_quasirandom(shape = 16, size=5, alpha=0.5, dodge.width=0.5, groupOnX = T)
  Gene.plot <- Gene.plot + gradient.ramp + ggplot2::scale_x_discrete(limits = batch.names) +
    ggplot2::xlab("Sample") + ggplot2::ylab("Percent")
  Gene.plot <- Gene.plot + ggplot2::ggtitle(paste0("Percent Expressed of the Selected Gene: ", input$name))
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(Gene.plot)  %>% layout(margin = m)
  p
})

output$QCGeneFull <- renderPlotly(GeneFull())

#' @description generate drop out plot
#' @param to be added
#'

DropOutFull <- reactive({
  expression.matrix <- as.data.frame(dataExpression())
  ## Subset the data
  totalCells <- ncol(expression.matrix)
  DropOut <- apply(expression.matrix, 1, function(x){sum(x>0)})/totalCells
  MeanExprs <- rowMeans(expression.matrix)
  MeanExprs <-log2(MeanExprs+1)
  ## Tidy dataframe
  combined.data <- list(PropExprs = DropOut , MeanExprs = MeanExprs)
  combined.df <- as.data.frame(combined.data)
  combined.df <- combined.df[order(combined.df$MeanExprs, decreasing=TRUE ),]
  # ggplot
  DropOut.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = MeanExprs, y = PropExprs))
  DropOut.plot <-  DropOut.plot + ggplot2::geom_point() + guides(colour=FALSE)
  DropOut.plot <-  DropOut.plot + ggplot2::ggtitle("Proportion of Cells Expressed by Mean Expression") +xlab("Log2 mean expression") +
    ylab("Proportion of Cells Expressed")
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(DropOut.plot)  %>% layout(margin = m)
  p
})

output$DropOutFull <- renderPlotly(DropOutFull())

