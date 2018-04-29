#' @description generate Sunburst plot
#' @param to be added
#'
densityPositive <- reactive({
  palette <- clusterPalette()
  it <- thresholdCheck(input$threshold)
  dataDensityPositive <- as.data.frame(dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,])

  p <- ggplot(dataDensityPositive, aes(log2(Gene+1), fill=factor(Cluster))) +
    geom_density(aes( y=..scaled.. ), alpha=0.5) +
    theme_bw() + ylab('Scaled Density') + xlab('log2(Expression+1)') +
    scale_fill_manual(name="SubPop", values=c(palette),limits=t(dataClusterNames())) +
    labs(title = paste0("Gene Expression of Positive Cells:\n", input$name)) +
    theme(plot.title = element_text(hjust = 0.5))
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(p)  %>% layout(margin = m)
  p
})

output$densityPositive <- renderPlotly(densityPositive())

densityFull <- reactive({
  palette <- clusterPalette()
  dataDensityFull <- as.data.frame(dataGeneExpressionCluster())

  p <- ggplot(dataDensityFull, aes(log2(Gene+1), fill=factor(Cluster))) +
    geom_density(aes( y=..scaled.. ), alpha=0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    ylab('Scaled Density') + xlab('log2(Expression+1)') +
    scale_fill_manual(name="SubPop", values=c(palette),limits=t(dataClusterNames())) +
    ggtitle(paste0("Gene Expression of All Cells:\n", input$name))
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(p)  %>% layout(margin = m)
  p
})

output$densityFull <- renderPlotly(densityFull())

densityPositiveUnscaled <- reactive({
  palette <- clusterPalette()
  it <- thresholdCheck(input$threshold)
  dataDensityPositive <- as.data.frame(dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,])

  p <- ggplot(dataDensityPositive, aes(log2(Gene+1), fill=as.factor(Cluster))) +
    geom_density(alpha=0.5) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    ylab('Density') + xlab('log2(Expression+1)') +
    scale_fill_manual(name="SubPop",  values=c(palette),limits=t(dataClusterNames())) +
    ggtitle(paste0("Gene Expression of Positive Cells:\n", input$name))
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(p)  %>% layout(margin = m)
  p
})

output$densityPositiveUnscaled <- renderPlotly(densityPositiveUnscaled())

densityFullUnscaled <- reactive({
  palette <- clusterPalette()
  dataDensityFull <- dataGeneExpressionCluster()

  p <- ggplot(dataDensityFull, aes(log2(Gene+1), fill = factor(Cluster))) +
    geom_density(alpha=0.5)+
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) +
    ylab('Density') + xlab('log2(Expression+1)') +
    scale_fill_manual(name="SubPop", values=c(palette),limits=t(dataClusterNames())) +
    ggtitle(paste0("Gene Expression of All Cells:\n", input$name))
  m <- list(l = 80, b = 40, t = 95)
  p <- ggplotly(p)  %>% layout(margin = m)
  p
})

output$densityFullUnscaled <- renderPlotly(densityFullUnscaled())
