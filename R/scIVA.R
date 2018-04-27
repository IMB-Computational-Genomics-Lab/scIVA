# install.packages('shiny')
# install.packages('plotly')
# install.packages('RColorBrewer')
# install.packages('sunburstR')
# install.packages('readr')
# install.packages('readxl')
# install.packages('d3r')
# install.packages('treemap')
# install.packages('randomcoloR')
# install.packages('DT')
# install.packages('shinythemes')
# install.packages('webshot')
# install.packages('shinyBS')
# install.packages('heatmaply')
# install.packages('ReactomePA')
# source("https://bioconductor.org/biocLite.R")
# biocLite("ReactomePA")
# install.packages('org.Hs.eg.db')
# biocLite("org.Hs.eg.db")
# install.packages('clusterProfiler')
# biocLite("clusterProfiler")
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
library('shinyBS')
library('data.table')
library('heatmaply')
library('ReactomePA')
library('org.Hs.eg.db')
library('clusterProfiler')
library('networkD3')
library('preprocessCore')
# rm(list=ls())

# summary_data_big <- function(cluster, dat_gene, threshold){
#   cellCount <- length(cluster)

summary_data_big <- function(stepCluster, dGE, threshold){
  cellCount <- length(which(dGE$Cluster==stepCluster))
  pos_idx <- which((dGE$Cluster==stepCluster) & (dGE$Gene>threshold))
  posCount <- length(pos_idx)
  posPercent <- posCount/cellCount*100
  dGEC <- dGE$Gene[dGE$Cluster==stepCluster]
  meanExprs <- mean(dGEC)
  meanPositive <- mean(dGEC[dGEC>threshold])
  return(list(cellCount,posCount,round(posPercent,3),round(meanExprs,3),round(meanPositive,3)))
  cluster_exprs=NULL
}

summary_data_sunburst <- function(stepCluster, dGE, threshold){
  cellCount <- length(which(dGE$Cluster==stepCluster))
  pos_idx <- which((dGE$Cluster==stepCluster) & (dGE$Gene>threshold))
  posCount <- length(pos_idx)
  return(list(cellCount,posCount))
}

rep.row<-function(x,n) {
  matrix(rep(x,each=n),nrow=n)
}

rep.col<-function(x,n) {
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

thresholdCheck <- function(x){
  if (x < 0) {
    return(0)
  } else {
    return(x)
  }
}

varMean <- function(x) {
  if (mean(x) > 0) {
    y <- var(x)/mean(x)
  } else {
    y<-0
  }
  return(y)
}

# dataExpression <- read.csv("/Volumes/TSB USB DRV/scIVA/Expression_sample_data.csv")
# dataCluster <- read.csv("/Volumes/TSB USB DRV/scIVA/Cluster_sample_data.csv")
# geneNames <- dataExpression[,1]

ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("scIVA: Single Cell Interactive Visualisation and Analysis"),
  tabsetPanel(type = "tabs",
              tabPanel("Upload data",
                       fluidRow(
                         column(3,
                                wellPanel(
                                  fileInput("expression","Upload Expression Matrix",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),

                                  checkboxInput("transposeExpression", "Transpose Expression", FALSE),
                                  checkboxInput("headerExpression", "Header", TRUE),
                                  radioButtons("sepExpression", "Separator",
                                               choices = c("Comma" = ",",
                                                           "Semicolon" = ";",
                                                           "Tab" = "\t"),
                                               selected = ","),
                                  radioButtons("quoteExpression", "Quote",
                                               choices = c("None" = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"')
                                )
                         ),
                         column(9,
                                uiOutput("expressionTitle"),
                                tableOutput("tableExpression"),
                                tableOutput("noGenes"),
                                tableOutput("noCells")
                         )
                       ),

                       tags$hr(),

                       fluidRow(
                         column(3,
                                wellPanel(
                                  fileInput("cluster","Upload Cluster Matrix",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),

                                  checkboxInput("transposeCluster", "Transpose Cluster", FALSE),
                                  checkboxInput("headerCluster", "Header", TRUE),
                                  radioButtons("sepCluster", "Separator",
                                               choices = c("Comma" = ",",
                                                           "Semicolon" = ";",
                                                           "Tab" = "\t"),
                                               selected = ","),
                                  radioButtons("quoteCluster", "Quote",
                                               choices = c("None" = "",
                                                           "Double Quote" = '"',
                                                           "Single Quote" = "'"),
                                               selected = '"')
                                )
                         ),
                         column(width = 4,
                                uiOutput("clusterTitle"),
                                tableOutput("tableCluster"),
                                tableOutput("noClusters")
                         ),
                         column(width = 4,
                                uiOutput("uniqueTitle"),
                                tableOutput("tableClusterNames"),
                                tableOutput("noUnique")
                         )
                       ),

                       tags$hr(),

                       fluidRow(
                         column(width = 3,
                                wellPanel(
                                  fileInput("geneList","Upload gene list to subset data by:",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  checkboxInput("headerGeneList", "Header", TRUE)
                                )
                         ),
                         column(width = 4,
                                uiOutput("geneListTitle"),
                                tableOutput("tableGeneList")
                         ),
                         column(width = 4,
                                tableOutput("noList")
                         )
                       ),

                       tags$hr(),

                       fluidRow(
                         column(width = 3,
                                wellPanel(
                                  fileInput("clusterList","Upload cell list to subset data by:",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),
                                  checkboxInput("headerClusterList", "Header", TRUE)
                                )
                         ),
                         column(width = 6,
                                uiOutput("clusterListTitle"),
                                tableOutput("tableClusterList")
                         )
                       )
              ),
              tabPanel("Quality Control",
                       conditionalPanel(condition = "output.conditionDataEntry != 'TRUE'",
                                        h4('Please load the Expression and Cluster matrices on the "Upload Data" tab.')),
                       conditionalPanel(condition = "output.conditionDataEntry == 'TRUE'",
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Mapped Reads",
                                                             tags$br(),
                                                             fluidRow(
                                                               plotlyOutput("QClibSizeExprsGenesFull")
                                                             )
                                                    ),
                                                    tabPanel("Dropout rate",
                                                             tags$br(),
                                                             fluidRow(
                                                               plotlyOutput("DropOutFull")
                                                             )),
                                                    tabPanel("Heatmap",
                                                             tags$br(),
                                                             conditionalPanel(condition = "output.conditionDataEntry != 'TRUE'",
                                                                              h4('Please load the Expression and Cluster matrices on the "Upload Data" tab.')),
                                                             conditionalPanel(condition = "output.conditionDataEntry == 'TRUE'",
                                                                              fluidRow(
                                                                                column(4,
                                                                                       wellPanel(
                                                                                         radioButtons('geneOrderType', 'Choose ordering type',
                                                                                                      choices = c('None' = 'N',
                                                                                                                  'Variance' = 'V',
                                                                                                                  'Mean'= 'M',
                                                                                                                  'Variance/Mean' = 'VM'),
                                                                                                      selected = c('None' = 'N'))
                                                                                       )
                                                                                ),
                                                                                column(4,
                                                                                       wellPanel(
                                                                                         radioButtons('clusterOrderType', 'Choose to Group by Clusters',
                                                                                                      choices = c('None' = 'N', 'Group' = 'G'),
                                                                                                      selected = c('None' = 'N'))
                                                                                       ))
                                                                              ),
                                                                              fluidRow(
                                                                                column(12,
                                                                                       plotlyOutput("heatmap")
                                                                                )
                                                                              )
                                                             )
                                                    ),
                                                    tabPanel("Normalisation & Outlier Removal",
                                                             fluidRow(
                                                               h4('Outlier Removal'),
                                                               column(6,
                                                                      sliderInput("mad_cutoff", "Set Median Absolute Deviation Threshold:",
                                                                                  min = 0.1, max = 5,
                                                                                  value = 3)
                                                               ),
                                                               column(6,
                                                                      sliderInput("gene_cutoff", "Set Minimum Percent of Expressing Cells Threshold:",
                                                                                  min = 0.1, max = 50,
                                                                                  value = 1)
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(width = 12,
                                                                      tableOutput("outlierRemoved")
                                                               )
                                                             ),
                                                             tags$hr(),
                                                             fluidRow(
                                                               h4('Normalisation'),
                                                               radioButtons('normOption', 'Choose a type of normalisation:',
                                                                            choices = c('None' = 'N',
                                                                                        'By CPM' = 'CPM',
                                                                                        'By Quantile' = 'Quantile',
                                                                                        'By RLE' = 'RLE'),
                                                                            selected = c('None' = 'N')
                                                               ),
                                                               plotlyOutput("libSizeExprsPostNorm"),


                                                             # Add download buttons here
                                                             h4('Download Normalised Dataset as .csv'),
                                                             downloadButton("downloadNormData", "DownloadData"),
                                                             downloadButton("downloadClusterData", "DownloadCluster")
                                                             )
                                                    )
                                        )
                       )
              ),
              tabPanel("Single Gene Analysis",
                       conditionalPanel(condition = "output.conditionDataEntry != 'TRUE'",
                                        h4('Please load the Expression and Cluster matrices on the "Upload Data" tab.')),
                       conditionalPanel(condition = "output.conditionDataEntry == 'TRUE'",
                                        sidebarLayout(
                                          sidebarPanel(width = 3,
                                                       selectizeInput('name',
                                                                      label="Select a gene to display its expression:",
                                                                      choices = NULL,
                                                                      multiple = T,
                                                                      options = list(maxItems = 1, placeholder = NULL)
                                                       ),
                                                       numericInput("threshold", "Lower threshold for Positive Cells:", value = 0),
                                                       conditionalPanel(condition = "output.conditionListEntry == 'TRUE'",
                                                                        checkboxInput('updateNameList', 'Search Within Gene List', value = FALSE)
                                                       )
                                          ),
                                          mainPanel(
                                            conditionalPanel(condition = "output.conditionNameEntry != 'TRUE'",
                                                             h4('Please choose a gene from the dropdown menu.')),
                                            conditionalPanel(condition = "output.conditionNameEntry == 'TRUE'",
                                                             tabsetPanel(type = "tabs",
                                                                         tabPanel("Visualisation",
                                                                                  fluidRow(
                                                                                    column(width = 12, align = "center",
                                                                                           uiOutput("sunburstTitle"))
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 12,
                                                                                           sunburstOutput("sunburstPlot"))
                                                                                  ),
                                                                                  tags$hr(),
                                                                                  fluidRow(
                                                                                    plotlyOutput("QCGeneFull")
                                                                                  ),
                                                                                  tags$hr(),
                                                                                  fluidRow(
                                                                                    plotlyOutput("densityFullUnscaled")
                                                                                  ),
                                                                                  tags$br(),

                                                                                  fluidRow(column(12,h4("Kruskal-Wallis Test"))),
                                                                                  fluidRow(
                                                                                    column(width = 4,
                                                                                           tableOutput("kwTableFull")
                                                                                    ),
                                                                                    column(width = 8,
                                                                                           tableOutput("kwTableFullOther")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 12,
                                                                                           h4("Kolmogorov-Smirnov Test"),
                                                                                           tableOutput("ksTableFull")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           tableOutput("ksTableFullCount")
                                                                                    ),
                                                                                    column(width = 6,
                                                                                           radioButtons("ksTypeFull",
                                                                                                        "Choose K-S Test Output:",
                                                                                                        choices = c("p-value" = "P",
                                                                                                                    "Test Statistic" = "T"),
                                                                                                        selected = "P")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 12,
                                                                                           h4("Likelihood Ratio Differential Expression Test Full Dataset"),
                                                                                           tableOutput("LRTTableFull")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 6
                                                                                    ),
                                                                                    column(width = 6,
                                                                                           radioButtons("lrTypeFull",
                                                                                                        "Choose LR DE Test Output:",
                                                                                                        choices = c("Fold Change (Row/Column)" = "F",
                                                                                                                    "log2(Fold Change)" = "L",
                                                                                                                    "p-value" = "P"),
                                                                                                        selected = "F")
                                                                                    )
                                                                                  ),
                                                                                  tags$hr(),
                                                                                  fluidRow(
                                                                                    plotlyOutput("densityPositiveUnscaled")
                                                                                  ),
                                                                                  tags$br(),
                                                                                  fluidRow(column(12,h4("Kruskal-Wallis Test"))),
                                                                                  fluidRow(
                                                                                    column(width = 4,
                                                                                           tableOutput("kwTablePos")
                                                                                    ),
                                                                                    column(width = 8,
                                                                                           tableOutput("kwTablePosOther")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 12,
                                                                                           h4("Kolmogorov-Smirnov Test"),
                                                                                           tableOutput("ksTablePositive")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           tableOutput("ksTablePositiveCount")
                                                                                    ),
                                                                                    column(width = 6,
                                                                                           radioButtons("ksTypePositive",
                                                                                                        "Choose K-S Test Output:",
                                                                                                        choices = c("p-value" = "P",
                                                                                                                    "Test Statistic" = "T"),
                                                                                                        selected = "P")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 12,
                                                                                           h4("Likelihood Ratio Differential Expression Test Positive Cells"),
                                                                                           tableOutput("LRTTablePos")
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 6
                                                                                    ),
                                                                                    column(width = 6,
                                                                                           radioButtons("lrTypePositive",
                                                                                                        "Choose LR DE Test Output:",
                                                                                                        choices = c("Fold Change" = "F",
                                                                                                                    "log2(Fold Change)" = "L",
                                                                                                                    "p-value" = "P"),
                                                                                                        selected = "F")
                                                                                    )
                                                                                  )
                                                                         ),
                                                                         tabPanel("Data",
                                                                                  tags$br(),
                                                                                  conditionalPanel(condition = "output.conditionNameEntry != 'TRUE'",
                                                                                                   h4('Please choose a gene from the Visualisation tab.')),
                                                                                  conditionalPanel(condition = "output.conditionNameEntry == 'TRUE'",
                                                                                                   fluidRow(
                                                                                                     uiOutput("summaryTitle"),
                                                                                                     tableOutput("summaryData")
                                                                                                   ),
                                                                                                   fluidRow(
                                                                                                     column(6,
                                                                                                            uiOutput("fullDataTitle"),
                                                                                                            DT::dataTableOutput("tableClusterDataFull")
                                                                                                     ),
                                                                                                     column(6,
                                                                                                            uiOutput("posDataTitle"),
                                                                                                            DT::dataTableOutput("tableClusterDataPos")
                                                                                                     )
                                                                                                   )
                                                                                  )
                                                                         )
                                                             )
                                            )
                                          )
                                        )
                       )
              ),
              tabPanel("Gene List Analysis",
                       conditionalPanel(condition = "output.conditionDataEntry != 'TRUE'",
                                        h4('Please load the Expression and Cluster matrices on the "Upload Data" tab.')),
                       conditionalPanel(condition = "output.conditionDataEntry == 'TRUE'",
                                        conditionalPanel(condition = "output.conditionListEntry != 'TRUE'",
                                                         h4('Please load a Gene List on the "Upload Data" tab.')),
                                        conditionalPanel(condition = "output.conditionListEntry == 'TRUE'",
                                                         fluidRow(
                                                           column(width = 12,
                                                                  h4('Reactome pathway analysis of the gene list')
                                                           ),
                                                           tags$br(),
                                                           column(width = 6,
                                                                  sliderInput("p_cutoff", "p value cutoff for the enrichment test:",
                                                                              min = 0.0001, max = 1,
                                                                              value = 0.15)
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(12,
                                                                  forceNetworkOutput("reactome")
                                                           )
                                                         ),
                                                         tags$hr(),
                                                         fluidRow(
                                                           column(width = 12,
                                                                  h4('Clustering the selected gene list')
                                                           ),
                                                           tags$br(),
                                                           column(width = 6,
                                                                  sliderInput("heatmap2Input", "Number of Clusters by Genes:",
                                                                              min = 1, max = 10,
                                                                              value = 1)
                                                           ),
                                                           column(width = 6,
                                                                  radioButtons('Scaling', 'Select scaling option by row:',
                                                                               choices = c('No Scaling' ='N', 'Scaling' = 'S'),
                                                                               selected = c('None' = 'N'))
                                                           )
                                                         ),
                                                         fluidRow(
                                                           column(12,
                                                                  plotlyOutput("heatmap2")
                                                           )
                                                         )
                                        )
                       )
              )
  )
)

server <- function(input, output, session){

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
    DE <-log2(DE+1)
    k_row_input <-input$heatmap2Input
    if (input$Scaling == 'N') {
      p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL) #, scale="row"
    } else {
      DE <- scale(DE, center = TRUE, scale = TRUE)
      p <- heatmaply(DE, k_row = k_row_input, labCol = NA, Colv = NULL, scale="row")
    }

    # if (is.null(dataGeneList())) {
    #   numRow <- nrow(dfExpressionOrdered)
    #   dfExpressionOrdered <- dfExpressionOrdered[(numRow-input$heatmapInput+1):numRow,]
    #   plotHeight <- as.character(120+input$heatmapInput*15)
    # } else {
    #   plotHeight <- as.character(120+nrow(dfExpressionOrdered)*15)
    # }

    plotHeight <- as.character(120+nrow(dataExpressionGeneList())*15)

    p2 <- p %>% layout(height = plotHeight, autosize = TRUE,
                       margin = list(l = 250,r = 0,b = 25,t = 75,pad = 0)
    )
    p2
  })

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

  outputOptions(output, "conditionDataEntry", suspendWhenHidden = FALSE)
  outputOptions(output, "conditionNameEntry", suspendWhenHidden = FALSE)
  outputOptions(output, "conditionListEntry", suspendWhenHidden = FALSE)

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

  #start the test
  # LRTTableFull <- reactive({
  #   dat_full <- dataGeneExpressionCluster()$Gene
  #   cluster_full <- dataGeneExpressionCluster()$Cluster
  #   unique_cluster <-unique(cluster_full)
  #   LRT <- matrix(NA,1,3)
  #   colnames(LRT) <- c("p.value","FoldChange","Conditions")
  #   for (i in 1:round(length(unique_cluster)/2)){
  #     for(j in (i+1):length(unique_cluster)){
  #       x_vec <- dat_full[which(cluster_full == unique_cluster[i])]
  #       y_vec <- dat_full[which(cluster_full == unique_cluster[j])]
  #       LRTResult <-  DifferentialLRT(x_vec, y_vec)
  #       nameAdd <- paste0(as.character(unique_cluster[i]), "_vs_",
  #                         as.character(unique_cluster[j]))
  #       toAdd <- c(LRTResult,round(mean(x_vec)/mean(y_vec),digits=3), nameAdd)
  #       LRT <-rbind(LRT, toAdd)
  #     }
  #   }
  #   #LTR <-as.data.frame(LRT[-1,])
  #   row.names(LRT) <-LRT[,3]
  #   return(as.matrix(LRT[-1,-3]))
  # })

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


  ####Render Text####

  ####
  #output$ <- renderUI({HTML("<h4>",,"</h4>")})
  #uiOutput("")
  ####

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

  output$fullDataTitle <- renderUI({HTML("<h4>Full Data:<br>", input$name, "</h4>")})

  output$posDataTitle <- renderUI({
    it <- thresholdCheck(input$threshold)
    HTML("<h4>Positive Data, lower threshold at ", it,":<br>", input$name, "</h4>")})

}

shinyApp(ui, server)
