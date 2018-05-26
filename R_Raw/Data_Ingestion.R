#' @description To subset data
#' @param to be added

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
                                          header = input$headerExpression,
                                          sep = input$sepExpression,
                                          quote = input$quoteExpression)))
  } else {
    dfExpression <- fread(input$expression$datapath,
                          header = input$headerExpression,
                          sep = input$sepExpression,
                          quote = input$quoteExpression,
                          data.table = FALSE)
  }


  row.names(dfExpression) <- dfExpression[,1]
  dfExpression[,1] <- NULL
  #dfExpressionNew <- data.frame(dfExpression[,-1],row.names = dfExpression[,1])
  dfExpressionNew <- dfExpression

  # if(!is.null(dataGeneList())){
  #   sampleGeneIndex <- which(rownames(dfExpressionNew) %in% dataGeneList()[,1])
  #   dfExpressionNew <- dfExpressionNew[sampleGeneIndex,]
  # }

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

output$conditionDataEntry <- reactive({
  as.character(class(dataExpression()) == "data.frame" & class(dataClusterUpload()) == "data.frame")
})

output$conditionNameEntry <- reactive({
  as.character(any(input$name == row.names(dataExpression())))
})

output$conditionListEntry <- reactive({
  as.character(class(dataGeneList()) == "data.frame")
})
