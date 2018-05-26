#' @description generate Sunburst plot
#' @param to be added

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
