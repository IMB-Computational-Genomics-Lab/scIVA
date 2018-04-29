#' @description To perform a KS test at gene level
#' @param to be added
#'

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


#' @description To perform a KW test at gene level
#' @param to be added
#'
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

#' @description perform diffferential expression analysis by likelihood ratio test
#' @param to be added
#'
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
