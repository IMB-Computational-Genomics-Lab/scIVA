
shinyServer(function(input, output, session){

  ####First Page inputs/outputs####

  options(shiny.maxRequestSize=10000*1024^2) #10 GB

  dataGeneList <- reactive({
    infileGene <- input$geneList
    if (is.null(infileGene)){
      return(NULL)
    }
    dGL <- as.data.frame(read.csv(input$geneList$datapath,
                                  header = input$headerGeneList,
                                  sep = ",",
                                  quote = '"'))
    return(dGL)
  })

  dataClusterList <- reactive({
    infileCluster <- input$clusterList
    if (is.null(infileCluster)) {
      return(NULL)
    }
    dCL <- as.data.frame(read.csv(input$clusterList$datapath,
                                  header = input$headerClusterList,
                                  sep = ",",
                                  quote = '"'))
    if (ncol(dCL) > 1) {
      dCL <- as.data.frame(do.call(paste, c(dCL, sep = " ")))
    }
    return(dCL)
  })

  output$tableGeneList <- renderTable({
    dataGeneList()
  },bordered = TRUE)

  output$tableClusterList <- renderTable({
    dataClusterList()
  },bordered = TRUE)

  dataExpression <- reactive({
    req(input$expression)
    if(input$transposeExpression == TRUE){
      dfExpression <- as.data.frame(t(fread(input$expression$datapath,
                                            header = "auto",
                                            sep = input$sepExpression,
                                            quote = input$quoteExpression)))
    } else {
      dfExpression <- fread(input$expression$datapath,
                            header = "auto",
                            sep = input$sepExpression,
                            quote = input$quoteExpression,
                            data.table = FALSE)
    }


    row.names(dfExpression) <- dfExpression[,1]
    dfExpression[,1] <- NULL
    #dfExpressionNew <- data.frame(dfExpression[,-1],row.names = dfExpression[,1])
    dfExpressionNew <- dfExpression

    if(!is.null(dataClusterList())){
      sampleClusterIndex <- which(dataClusterUpload()[,1] %in% dataClusterList()[,1])
      dfExpressionNew <- dfExpressionNew[,sampleClusterIndex]
    }

    # dfNames <- row.names(dfExpressionNew)

    # updateSelectizeInput(session, 'name',choices = sort(dfNames), server = TRUE)
    return(round(dfExpressionNew,2))
  })

  dataExpressionGeneList <- reactive({
    if(!is.null(dataGeneList())){
      sampleGeneIndex <- which(rownames(dataExpression()) %in% dataGeneList()[,1])
      dfExpressionNew <- dataExpression()[sampleGeneIndex,]
    }
    return(dfExpressionNew)
  })

  observe({
    input$updateNameList

    if(input$updateNameList == TRUE && !is.null(dataGeneList()) == TRUE) {
      dfNames <- row.names(dataExpressionGeneList())
    } else {
      dfNames <- row.names(dataExpression())
    }
    updateSelectizeInput(session, 'name',choices = sort(dfNames), server = TRUE)
  })

  dataClusterUpload <- reactive({
    req(input$cluster)
    if(input$transposeCluster == TRUE){
      dfClusterUpload <- as.data.frame(t(read.csv(input$cluster$datapath,
                                                  header = input$headerCluster,
                                                  sep = input$sepCluster,
                                                  quote = input$quoteCluster)))
    } else {
      dfClusterUpload <- as.data.frame(read.csv(input$cluster$datapath,
                                                header = input$headerCluster,
                                                sep = input$sepCluster,
                                                quote = input$quoteCluster))
    }

    if (ncol(dfClusterUpload) > 1) {
      dfClusterUpload <- as.data.frame(do.call(paste, c(dfClusterUpload, sep = " ")))
    }

    colnames(dfClusterUpload) <- c("ClusterNames")
    return(dfClusterUpload)
  })

  dataCluster <- reactive({
    dfCluster <- dataClusterUpload()
    if(!is.null(dataClusterList())){
      sampleClusterIndex <- which(dfCluster[,1] %in% dataClusterList()[,1])
      dfCluster <- as.data.frame(dfCluster[sampleClusterIndex,])
    }
    colnames(dfCluster) <- c("ClusterNames")
    return(as.data.frame(dfCluster))
  })

  dataClusterNames <- reactive({
    CN <- as.data.frame(unique(dataCluster()))
    colnames(CN) <- c("UniqueClusterNames")
    return(CN)
  })

  output$tableExpression <- renderTable({
    dataExpression()[1:5,1:5]
  },rownames = TRUE,bordered = TRUE)

  output$tableCluster <- renderTable({
    dataCluster <- as.data.frame(dataCluster())
    head(dataCluster)
  },bordered = TRUE)

  output$tableClusterNames <- renderTable({
    dataClusterNames()
  },bordered = TRUE)

  clusterPalette <- reactive({
    brewer.pal(max(dim(dataClusterNames())),"Set3")
  })

  dataGeneExpressionCluster <- reactive({
    namesExpression <- row.names(dataExpression())
    gene_idx <- as.numeric(which(namesExpression==input$name))
    dataGeneExpression_t <- as.data.frame(t(dataExpression()[gene_idx,]))
    dataCluster_new <- dataCluster()
    dataGeneExpression_t$Cluster <- dataCluster_new$ClusterNames
    colnames(dataGeneExpression_t) <- c('Gene','Cluster')
    return(dataGeneExpression_t)
  })

  output$tableClusterDataFull <- DT::renderDataTable({
    DT::datatable(dataGeneExpressionCluster(),options = list(pageLength = 20))
  })

  output$tableClusterDataPos <- DT::renderDataTable({
    it <- thresholdCheck(input$threshold)
    dat_pos <-dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,]
    DT::datatable(dat_pos,options = list(pageLength = 20))
  })

  output$noGenes <- renderTable({
    noGenesTable <- matrix(0,1,1)
    colnames(noGenesTable) <- "No. of Genes"
    noGenesTable[1,1] <- dim(dataExpression())[1]
    return(noGenesTable)
  }, digits = 0, bordered = T)

  output$noCells <- renderTable({
    noCellsTable <- matrix(0,1,1)
    colnames(noCellsTable) <- "No. of Cells"
    noCellsTable[1,1] <- dim(dataExpression())[2]
    return(noCellsTable)
  }, digits = 0, bordered = T)

  output$noClusters <- renderTable({
    noClustersTable <- matrix(0,1,1)
    colnames(noClustersTable) <- "No. of Cells with Cluster Info."
    noClustersTable[1,1] <- dim(dataCluster())[1]
    return(noClustersTable)
  }, digits = 0, bordered = T)

  output$noUnique <- renderTable({
    noUniqueTable <- matrix(0,1,1)
    colnames(noUniqueTable) <- "No. of Unique Clusters"
    noUniqueTable[1,1] <- dim(dataClusterNames())[1]
    return(noUniqueTable)
  }, digits = 0, bordered = T)

  output$noList <- renderTable({
    noListTable <- matrix(0,1,1)
    colnames(noListTable) <- "No. of Genes in List"
    noListTable <- dim(dataGeneList())[1]
    return(noListTable)
  }, digits = 0, bordered = T)

  ####TestCases####

  output$checkNo <- reactive({
    as.character(isTRUE(dim(dataCluster())[1] == dim(dataExpression())[2]))
  })

  output$conditionDataEntry <- reactive({
    as.character(class(dataExpression()) == "data.frame" & class(dataClusterUpload()) == "data.frame")
  })

  output$conditionNameEntry <- reactive({
    as.character(any(input$name == row.names(dataExpression())))
  })

  output$conditionListEntry <- reactive({
    as.character(class(dataGeneList()) == "data.frame")
  })
  ####DensityPlots####

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

  ####SummaryData####

  SData <- reactive({
    SD <- as.data.frame(matrix(0,5,max(dim(dataClusterNames()))))
    it <- thresholdCheck(input$threshold)

    for (i in 1:max(dim(dataClusterNames()))) {
      dCN <- as.character(dataClusterNames()[i,1])
      summary_output <- summary_data_big(dCN, dataGeneExpressionCluster(),it)

      SD[1,i] <- summary_output[[1]]
      SD[2,i] <- summary_output[[2]]
      SD[3,i] <- summary_output[[3]]
      SD[4,i] <- summary_output[[4]]
      SD[5,i] <- summary_output[[5]]
      colnames(SD)[i] <- paste0(dCN)
    }
    rownames(SD) <- c("Cell Count", "Positive Cells", "Percent Positive Cells", "Mean Expr. All", "Mean Expr. Positive")
    return(SD)
  })

  output$summaryData <- renderTable({
    SData()
  },rownames = T, digits = 2)

  ####SunburstPlot####

  SBdf <- reactive({
    SB <- as.data.frame(matrix(0,2*max(dim(dataClusterNames())),4))
    it <- thresholdCheck(input$threshold)

    for (i in 1:max(dim(dataClusterNames()))) {
      dCN <- as.character(dataClusterNames()[i,1])

      SB[2*i-1,1] <- "Total"
      SB[2*i,1] <- "Total"
      SB[2*i-1,2] <- paste0(dCN)
      SB[2*i,2] <- paste0(dCN)
      SB[2*i-1,3] <- "On"
      SB[2*i,3] <- "Off"
      sunburst_output <- summary_data_sunburst(dCN, dataGeneExpressionCluster(),it)
      SB[2*i-1,4] <- as.integer(sunburst_output[[2]])
      SB[2*i,4] <- as.integer(sunburst_output[[1]]-sunburst_output[[2]])
    }
    colnames(SB) <- c('DataSet','Cluster','State','Count')
    return(SB)
  })

  output$sunburstData <- renderTable({
    SBdf()
  })

  output$sunburstPlot <- renderSunburst({
    tmSB <- treemap(SBdf(), index=c("DataSet","Cluster","State"), vSize="Count",
                    vColor="State", type="index")

    tmSB_new <- tmSB$tm
    tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
    tmSB_new$color[which(tmSB_new$DataSet == 'Total')] <- '#00a6a7'
    for (i in seq_len(nrow(dataClusterNames()))) {
      dCN <- as.character(dataClusterNames()[i,1])
      tmSB_new$color[which(tmSB_new$Cluster == paste0(dCN))] <- clusterPalette()[i]
    }

    tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
    tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'

    tmSB_nest <- d3_nest(
      tmSB_new[,c("DataSet","Cluster","State", "vSize", "color")],
      value_cols = c("vSize", "color"))

    sunburst(data = tmSB_nest, valueField = "vSize", count = TRUE,
             colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
             withD3 = TRUE
    )

  })

  selection <- reactive({
    input$sunburst_mouseover
  })

  output$selection <- renderText(selection())

  ####KS Tables####

  ksTableFull <- reactive({
    n <- nrow(dataClusterNames())
    sampleInd <- combn(1:n,2)
    sampleNames <- combn(t(dataClusterNames()),2)
    ksTable <- matrix(NA,n,n+1)
    dat_full <- dataGeneExpressionCluster()

    for (i in seq_len(ncol(sampleInd))){
      m = length(dat_full[dat_full$Cluster==sampleNames[1,i],]$Gene)
      k = length(dat_full[dat_full$Cluster==sampleNames[2,i],]$Gene)

      if (m<3 | k<3){
        ksTable[sampleInd[1,i],sampleInd[2,i]] <- NA
        ksTable[sampleInd[2,i],sampleInd[1,i]] <- NA
      } else {
        sampleKS <- ks.test(dat_full[dat_full$Cluster==sampleNames[1,i],]$Gene,
                            dat_full[dat_full$Cluster==sampleNames[2,i],]$Gene)
        if (input$ksTypeFull == "P"){
          ksTable[sampleInd[1,i],sampleInd[2,i]] <- sampleKS$p.value
          ksTable[sampleInd[2,i],sampleInd[1,i]] <- sampleKS$p.value
        } else if (input$ksTypeFull == "T"){
          ksTable[sampleInd[1,i],sampleInd[2,i]] <- sqrt((m*k)/(m+k))*sampleKS$statistic
          ksTable[sampleInd[2,i],sampleInd[1,i]] <- sqrt((m*k)/(m+k))*sampleKS$statistic
        }
      }
    }

    for (i in 1:n){
      ksTable[i,ncol(ksTable)] <- length(dat_full[dat_full$Cluster==as.character(dataClusterNames()[i,1]),]$Gene)
    }

    colnames(ksTable) <- c(t(dataClusterNames()),"Count")
    rownames(ksTable) <- t(dataClusterNames())
    return((ksTable))
  })

  output$ksTableFull <- renderTable({
    ksTableFull()[seq_len(nrow(dataClusterNames())), seq_len(nrow(dataClusterNames()))]
  },spacing = "m",align = 'r',rownames = TRUE, digits = -1,na = "",bordered = TRUE)

  output$ksTableFullCount <- renderTable({
    trender <- as.data.frame(ksTableFull()[,ncol(ksTableFull())])
    colnames(trender) <- c("Counts")
    return(trender)
  },spacing = "l",align = 'r',rownames = TRUE,digits = 0,na = "",bordered = TRUE)

  ksTablePositive <- reactive({
    n <- nrow(dataClusterNames())
    sampleInd <- combn(seq_len(n), 2)
    sampleNames <- combn(t(dataClusterNames()),2)
    ksTable <- matrix(NA,n,n+1)
    it <- thresholdCheck(input$threshold)
    dat_pos <-dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,]


    for (i in 1:dim(sampleInd)[2]){
      m <- length(dat_pos[dat_pos$Cluster==sampleNames[1,i],]$Gene)
      k <- length(dat_pos[dat_pos$Cluster==sampleNames[2,i],]$Gene)

      if (m<3 | k<3){
        ksTable[sampleInd[1,i],sampleInd[2,i]] <- NA
        ksTable[sampleInd[2,i],sampleInd[1,i]] <- NA
      } else {
        sampleKS <- ks.test(dat_pos[dat_pos$Cluster==sampleNames[1,i],]$Gene,
                            dat_pos[dat_pos$Cluster==sampleNames[2,i],]$Gene)
        if (input$ksTypePositive == "P"){
          ksTable[sampleInd[1,i],sampleInd[2,i]] <- sampleKS$p.value
          ksTable[sampleInd[2,i],sampleInd[1,i]] <- sampleKS$p.value
        } else if(input$ksTypePositive == "T"){
          ksTable[sampleInd[1,i],sampleInd[2,i]] <- sqrt((m*k)/(m+k))*sampleKS$statistic
          ksTable[sampleInd[2,i],sampleInd[1,i]] <- sqrt((m*k)/(m+k))*sampleKS$statistic
        }
      }
    }
    it <- thresholdCheck(input$threshold)
    for (i in 1:n){
      ksTable[i,ncol(ksTable)] <- length(dat_pos[dat_pos$Cluster==as.character(dataClusterNames()[i,1]),]$Gene>it)
    }

    colnames(ksTable) <- c(t(dataClusterNames()), "Count")
    rownames(ksTable) <- t(dataClusterNames())
    return(ksTable)
  })

  output$ksTablePositive <- renderTable({
    ksTablePositive()[seq_len(nrow(dataClusterNames())), seq_len(nrow(dataClusterNames()))]
  },spacing = "m",align = 'r',rownames = TRUE,digits = -1,na = "",bordered = TRUE)

  output$ksTablePositiveCount <- renderTable({
    trender <- as.data.frame(ksTablePositive()[,ncol(ksTablePositive())])
    colnames(trender) <- c("Counts")
    return(trender)
  },spacing = "l",align = 'r',rownames = TRUE,digits = 0,na = "",bordered = TRUE)

  ####KW Tables####

  kwTableFull <- reactive({
    kruskalData <- kruskal.test(dataGeneExpressionCluster()$Gene ~ dataGeneExpressionCluster()$Cluster)
    kw <- matrix(0,1,3)
    kw[1,] <- c(kruskalData$p.value,kruskalData$statistic,kruskalData$parameter)
    colnames(kw) <- c("p.value","Chi-squared","Df")
    return(kw)
  })

  output$kwTableFull <- renderTable({
    t(kwTableFull()[1,1])
  },spacing = "m",align = 'r',digits = -1,bordered = TRUE)

  output$kwTableFullOther <- renderTable({
    t(kwTableFull()[1,2:3])
  },spacing = "m",align = 'r',bordered = TRUE)

  kwTablePos <- reactive({
    it <- thresholdCheck(input$threshold)
    dat_pos <-dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,]
    kruskalData <- kruskal.test(dat_pos$Gene ~ dat_pos$Cluster)
    kw <- matrix(0,1,3)
    kw[1,] <- c(kruskalData$p.value,kruskalData$statistic,kruskalData$parameter)
    colnames(kw) <- c("p.value","Chi-squared","Df")
    return(kw)
  })

  output$kwTablePos <- renderTable({
    t(kwTablePos()[1,1])
  },spacing = "m",align = 'r',digits = -1,bordered = TRUE)

  output$kwTablePosOther <- renderTable({
    t(kwTablePos()[1,2:3])
  },spacing = "m",align = 'r',bordered = TRUE)

  output$heatmap <- renderPlotly({

    if (input$clusterOrderType == 'G') {
      #init <- Sys.time()
      maExpression <- as.matrix(dataExpression())
      maDCN <- as.matrix(dataClusterNames())
      DE <- matrix(0,nrow(maExpression),nrow(maDCN))
      rownames(DE) <- rownames(maExpression)

      for  (i in 1:nrow(maDCN)) { # Use apply
        DE[,i] <- rowMeans(maExpression[,dataCluster() == maDCN[i,1]])
      }
      DE <- as.data.frame(DE)
      #print(Sys.time() - init)

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

  output$tableMeanVar <- DT::renderDataTable({
    dat <- dataExpression()
    tableMeanVar <- NULL
    #tableMeanVar$geneNames <- dat[,1]
    tableMeanVar$geneMean <- apply(dat[,2:ncol(dat)],1,mean)
    tableMeanVar$geneVar <- apply(dat[,2:ncol(dat)],1,var)
    tableMeanVardf <- as.data.frame(tableMeanVar)

    DT::datatable(tableMeanVardf[order(-tableMeanVardf$geneMean),],options = list(pageLength = 20))
  })

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

  output$heatmap2 <- renderPlotly({
    DE <- dataExpressionGeneList()
    #totalReadsPerGenes <- rowSums(DE)
    DE <- log2(DE+1)
    k_row_input <-input$heatmap2Input
    if (input$Scaling == 'N') {
      # p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL) #, scale="row"
      p <- heatmaply(DE, k_row = k_row_input, Colv = NULL, showticklabels = c(F,T),
                     label_names = c('Gene','Cell','Exprs'),labRow = rownames(DE),labCol = colnames(DE),
                     margins = c(0,250,0,0), colors = c('grey90','red'))#,scale = "row")
    } else {
      DE <- scale(DE, center = TRUE, scale = TRUE)
      #p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL, scale="row")
      p <- heatmaply(DE, k_row = k_row_input, Colv = NULL, showticklabels = c(F,T),
                     label_names = c('Gene','Cell','Exprs'),labRow = rownames(DE),labCol = colnames(DE),
                     margins = c(0,250,0,0), colors = c('grey90','red'))#,scale = "row")
    }

    plotHeight <- as.character(120+nrow(dataExpressionGeneList())*15)

    p2 <- p %>% layout(height = plotHeight, autosize = TRUE,
                       margin = list(l = 250,r = 0,b = 25,t = 75,pad = 0)
    )
    p2
  })


  #Gene list analysis-------------------------------------------------------------

  output$reactome <- renderForceNetwork({
    Genes <- dataGeneList()[,1]
    Genes <- gsub("_.*","",Genes)
    convert_to_gene_ID = bitr(Genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = "org.Hs.eg.db")
    converted_genes = convert_to_gene_ID$ENTREZID
    #totalReadsPerGenes <- rowSums(DE)
    p_cutoff <-input$p_cutoff
    x <- enrichPathway(gene=converted_genes,pvalueCutoff= p_cutoff, readable=TRUE)
    #p <- enrichMap(x, layout=igraph::layout.kamada.kawai, vertex.label.cex = 1)
    p <- emapplot.enrichResult(x)
    k <- igraph_to_networkD3(p)
    k$nodes$group <- as.factor(matrix(1,length(k$nodes$name),1))
    iD3 <- forceNetwork(Links = k$links, Nodes = k$nodes,Source = "source", Target = "target",
                        Value = "value", NodeID = "name", Group = "group",
                        linkDistance = JS("function(d){return d.value * 15}"),
                        fontSize = 18, charge = -20, bounded = TRUE) #,linkColour = "FF530D"
    return(iD3)
    #return(list("iD3"=iD3))# "p_cnet" = p_cnet))
  })


  output$cnet <- renderPlot({
    Genes <- dataGeneList()[,1]
    Genes <- gsub("_.*","",Genes)
    convert_to_gene_ID = bitr(Genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = "org.Hs.eg.db")
    converted_genes = convert_to_gene_ID$ENTREZID
    #totalReadsPerGenes <- rowSums(DE)
    p_cutoff <-input$p_cutoff
    x <- enrichPathway(gene=converted_genes,pvalueCutoff= p_cutoff, readable=TRUE)
    p <- cnetplot(x, showCategory=20, categorySize="pvalue", layout="nicely")
    #return(list("iD3"=iD3))# "p_cnet" = p_cnet))
    p
  })

  outputOptions(output, "conditionDataEntry", suspendWhenHidden = FALSE)
  outputOptions(output, "conditionNameEntry", suspendWhenHidden = FALSE)
  outputOptions(output, "conditionListEntry", suspendWhenHidden = FALSE)
  outputOptions(output, "checkNo", suspendWhenHidden = FALSE)


  ####LRT Tables####
  # function to run mcdavid et al. DE test
  DifferentialLRT <- function(x, y, xmin = 0) {
    lrtX <- bimodLikData(x = x)
    lrtY <- bimodLikData(x = y)
    lrtZ <- bimodLikData(x = c(x, y))
    lrt_diff <- 2 * (lrtX + lrtY - lrtZ)
    return(pchisq(q = lrt_diff, df = 3, lower.tail = F))
  }

  MinMax <- function(data, min, max) {
    data2 <- data
    data2[data2 > max] <- max
    data2[data2 < min] <- min
    return(data2)
  }

  bimodLikData <- function(x, xmin = 0) {
    x1 <- x[x <= xmin]
    x2 <- x[x > xmin]
    xal <- MinMax(
      data = length(x = x2) / length(x = x),
      min = 1e-5,
      max = (1 - 1e-5)
    )
    likA <- length(x = x1) * log(x = 1 - xal)
    if (length(x = x2) < 2) {
      mysd <- 1
    } else {
      mysd <- sd(x = x2)
    }
    likB <- length(x = x2) *
      log(x = xal) +
      sum(dnorm(x = x2, mean = mean(x = x2), sd = mysd, log = TRUE))
    return(likA + likB)
  }

  LRTTableFull <- reactive({
    n <- nrow(dataClusterNames())
    sampleInd <- combn(1:n,2)
    sampleNames <- combn(t(dataClusterNames()),2)
    lrTable <- matrix(NA,n,n)
    dat_full <- dataGeneExpressionCluster()

    for (i in seq_len(ncol(sampleInd))){
      m = length(dat_full[dat_full$Cluster==sampleNames[1,i],]$Gene)
      k = length(dat_full[dat_full$Cluster==sampleNames[2,i],]$Gene)

      if (m<3 | k<3){
        lrTable[sampleInd[1,i],sampleInd[2,i]] <- NA
        lrTable[sampleInd[2,i],sampleInd[1,i]] <- NA
      } else {
        x_vec <- dat_full[dat_full$Cluster==sampleNames[1,i],]$Gene
        y_vec <- dat_full[dat_full$Cluster==sampleNames[2,i],]$Gene
        if (input$lrTypeFull == "P"){

          sampleLR <- DifferentialLRT(x_vec,y_vec)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- sampleLR
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- sampleLR
        } else if (input$lrTypeFull == "F"){

          FC <- round(mean(x_vec)/mean(y_vec),digits=3)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- FC
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- 1/FC
        } else if (input$lrTypeFull == "L") {
          FC <- round(mean(x_vec)/mean(y_vec),digits=3)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- log2(FC)
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- log2(1/FC)
        }
      }
    }

    colnames(lrTable) <- c(t(dataClusterNames()))
    rownames(lrTable) <- t(dataClusterNames())
    return(lrTable)
  })

  output$LRTTableFull <- renderTable({
    LRTTableFull()
  },spacing = "m",align = 'r',rownames = TRUE, digits = -1, na = "",bordered = TRUE) #, digits = -1

  LRTTablePos <- reactive({
    it <- thresholdCheck(input$threshold)
    n <- nrow(dataClusterNames())
    sampleInd <- combn(1:n,2)
    sampleNames <- combn(t(dataClusterNames()),2)
    lrTable <- matrix(NA,n,n)
    dat_pos <- dataGeneExpressionCluster()[dataGeneExpressionCluster()$Gene>it,]

    for (i in seq_len(ncol(sampleInd))){
      m = length(dat_pos[dat_pos$Cluster==sampleNames[1,i],]$Gene)
      k = length(dat_pos[dat_pos$Cluster==sampleNames[2,i],]$Gene)

      if (m<3 | k<3){
        lrTable[sampleInd[1,i],sampleInd[2,i]] <- NA
        lrTable[sampleInd[2,i],sampleInd[1,i]] <- NA
      } else {
        x_vec <- dat_pos[dat_pos$Cluster==sampleNames[1,i],]$Gene
        y_vec <- dat_pos[dat_pos$Cluster==sampleNames[2,i],]$Gene
        if (input$lrTypePositive == "P"){

          sampleLR <- DifferentialLRT(x_vec,y_vec)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- sampleLR
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- sampleLR
        } else if (input$lrTypePositive == "F"){

          FC <- round(mean(x_vec)/mean(y_vec),digits=3)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- FC
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- 1/FC
        } else if (input$lrTypePositive == "L"){
          FC <- round(mean(x_vec)/mean(y_vec),digits=3)
          lrTable[sampleInd[1,i],sampleInd[2,i]] <- log2(FC)
          lrTable[sampleInd[2,i],sampleInd[1,i]] <- log2(1/FC)
        }
      }
    }

    colnames(lrTable) <- c(t(dataClusterNames()))
    rownames(lrTable) <- t(dataClusterNames())
    return(lrTable)
  })

  output$LRTTablePos <- renderTable({
    LRTTablePos()
  }, spacing = "m",align = 'r',rownames = TRUE, digits = -1, na = "",bordered = TRUE) #,spacing = "m",align = 'r'

  ###QUAN QC PLots####

  libSizeExprsGenesFull <- reactive({
    expression.matrix <- as.data.frame(dataExpression())
    cell.information <- dataCluster()[,1]
    batch.names <- unique(cell.information)
    ## Subset the data
    total.counts <- log1p(colSums(expression.matrix))
    total.features <- apply(expression.matrix, 2, function(x){sum(x>0)})
    ## Tidy dataframe
    combined.data <- list(TotalCounts = total.counts, FeatureCounts = total.features, BatchInfo = cell.information)
    combined.df <- as.data.frame(combined.data)

    # ggplot
    gradient.ramp <- ggplot2::scale_colour_gradient(low="#001b7f", high="#f1d351")
    control.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = factor(BatchInfo), y = TotalCounts, colour = FeatureCounts))
    # control.plot <- control.plot + ggplot2::geom_violin(size = 1, scale = "width", colour = NA)
    control.plot <- control.plot + ggbeeswarm::geom_quasirandom(shape = 16, size=5, alpha=0.5, dodge.width=0.5, groupOnX = T)
    control.plot <- control.plot + gradient.ramp + ggplot2::scale_x_discrete(limits = batch.names) + ggplot2::xlab("Sample") + ggplot2::ylab("Total mapped reads per cell (log)")
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
    combined.data <- list(genePercent = gene.percent , ExprsLevel = expression.gene, BatchInfo = cell.information)
    combined.df <- as.data.frame(combined.data)
    # ggplot
    gradient.ramp <- ggplot2::scale_colour_gradient(low="#001b7f", high="#f1d351")
    Gene.plot <- ggplot2::ggplot(combined.df, ggplot2::aes(x = factor(BatchInfo), y = genePercent, colour = ExprsLevel))
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

  ####QC Plots Drop out####
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

  #removing outliers (QC tab)
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

  #Normalisation (QC tab)
  normalisation <- reactive({
    expression.matrix <- as.data.frame(outlierRemoval()$data)
    if(input$normOption == "CPM"){
      dat_norm <-apply(expression.matrix, 2, function(x){(x/sum(x))*1000000})
    }else if (input$normOption == "Quantile"){
      #the function quantile_normalisation is based on DaveTang code
      quantile_normalisation <- function(df){
        df_rank <- apply(df,2,rank,ties.method="min")
        df_sorted <- data.frame(apply(df, 2, sort))
        df_mean <- apply(df_sorted, 1, mean)
        index_to_mean <- function(ip_index, ip_mean){
          return(ip_mean[ip_index])
        }
        df_final <- apply(df_rank, 2, index_to_mean, ip_mean=df_mean)
        rownames(df_final) <- rownames(df)
        return(df_final)
      }
      dat_norm <-quantile_normalisation(as.matrix(expression.matrix))
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

  ####Render Text####

  ####
  #output$ <- renderUI({HTML("<h4>",,"</h4>")})
  #uiOutput("")
  ####

  output$tableMeanVarTitle <- renderUI({HTML("<h4>Ordered Table of Uploaded Expression Matrix</h4>")})

  output$expressionTitle <- renderUI({req(input$expression)
    HTML("<h4>Uploaded Expression Matrix</h4>")})

  output$clusterTitle <- renderUI({req(input$cluster)
    HTML("<h4>Uploaded Cluster Matrix</h4>")})

  output$uniqueTitle <- renderUI({req(input$cluster)
    HTML("<h4>Unique Cluster Names</h4>")})

  output$geneListTitle <- renderUI({req(input$geneList)
    HTML("<h4>Uploaded Gene List</h4>")})

  output$clusterListTitle <- renderUI({req(input$clusterList)
    HTML("<h4>Uploaded Cluster List</h4>")})

  output$countTitle <- renderUI({HTML("<h4>Count Table: ", input$name,"</h4>")})

  output$sunburstTitle <- renderUI({HTML("<h4>Sunburst Plot: ", input$name, "</h4>")})

  output$summaryTitle <- renderUI({HTML("<h4>Summary Table: ", input$name, "</h4>")})

  output$fullDataTitle <- renderUI({HTML("<h5>Full Data:<br>", input$name, "</h4>")})

  output$posDataTitle <- renderUI({
    it <- thresholdCheck(input$threshold)
    HTML("<h5>Positive Data, lower threshold at ", it,":<br>", input$name, "</h4>")})

}
)
