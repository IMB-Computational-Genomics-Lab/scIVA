
shinyUI(fluidPage(
  theme = shinytheme("cyborg"),
  tags$style(
    HTML('
         .well{
         background-color: #ffffff;
         }
         ')
  ),
  tags$style(
    HTML('
         .tbody{
         color: #000000;
         }
         ')
  ),
  tags$tbody(tags$style(HTML(
    "#tableClusterDataPos tr.odd {background-color:black}
    #tableClusterDataPos tr.even {background-color:black}
    #tableClusterDataFull tr.odd {background-color:black}
    #tableClusterDataFull tr.even {background-color:black}"
  ))),

  titlePanel("scIVA: Single Cell Interactive Visualisation and Analysis"),

  tabsetPanel(type = "tabs",
              tabPanel("Upload Data",
                       fluidRow(
                         column(3,
                                wellPanel(
                                  fileInput("expression","Upload Expression Matrix",
                                            multiple = FALSE,
                                            accept = c("text/csv",
                                                       "text/comma-separated-values,text/plain",
                                                       ".csv")),

                                  checkboxInput("transposeExpression", "Transpose Expression", FALSE),
                                  radioButtons("headerExpression", "Header",
                                               choices = c("Auto" = "auto",
                                                           "True" = "TRUE",
                                                           "False" = "FALSE"),
                                               selected = "auto"),
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
                                                               column(12,
                                                                      wellPanel(
                                                                        plotlyOutput("QClibSizeExprsGenesFull")
                                                                      )
                                                               )
                                                             )
                                                    ),
                                                    tabPanel("Dropout rate",
                                                             tags$br(),
                                                             fluidRow(
                                                               column(12,
                                                                      wellPanel(
                                                                        plotlyOutput("DropOutFull")
                                                                      )
                                                               )
                                                             )
                                                    ),
                                                    tabPanel("Mean & Variance",
                                                             tags$br(),
                                                             conditionalPanel(condition = "output.conditionDataEntry != 'TRUE'",
                                                                              h4('Please load the Expression and Cluster matrices on the "Upload Data" tab.')),
                                                             conditionalPanel(condition = "output.conditionDataEntry == 'TRUE'",
                                                                              uiOutput("tableMeanVarTitle"),
                                                                              DT::dataTableOutput("tableMeanVar")
                                                             )
                                                    ),
                                                    tabPanel("Normalisation & Outlier Removal",
                                                             fluidRow(
                                                               column(12,
                                                                      h4('Outlier Removal')
                                                               ),
                                                               column(6,
                                                                      wellPanel(
                                                                        sliderInput("mad_cutoff", "Set Median Absolute Deviation Threshold:",
                                                                                    min = 0.1, max = 5,
                                                                                    value = 3)
                                                                      )
                                                               ),
                                                               column(6,
                                                                      wellPanel(
                                                                        sliderInput("gene_cutoff", "Set Minimum Percent of Expressing Cells Threshold:",
                                                                                    min = 0.1, max = 50,
                                                                                    value = 1)
                                                                      )
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(12,
                                                                      tableOutput("outlierRemoved")
                                                               )
                                                             ),
                                                             tags$hr(),
                                                             fluidRow(
                                                               column(12,
                                                                      h4('Normalisation')
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(6,
                                                                      wellPanel(
                                                                        radioButtons('normOption', 'Choose a type of normalisation:',
                                                                                     choices = c('None' = 'N',
                                                                                                 'By CPM' = 'CPM',
                                                                                                 'By Quantile' = 'Quantile',
                                                                                                 'By RLE' = 'RLE'),
                                                                                     selected = c('None' = 'N')
                                                                        )
                                                                      )
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(12,
                                                                      wellPanel(
                                                                        plotlyOutput("libSizeExprsPostNorm")
                                                                      )
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(12,
                                                                      h4('Download Normalised Dataset as .csv'),
                                                                      downloadButton("downloadNormData", "DownloadData"),
                                                                      downloadButton("downloadClusterData", "DownloadCluster")
                                                               ),
                                                               tags$br()
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
                                                                                  wellPanel(
                                                                                    fluidRow(
                                                                                      column(width = 12,
                                                                                             sunburstOutput("sunburstPlot"))
                                                                                    )
                                                                                  ),
                                                                                  tags$hr(),
                                                                                  wellPanel(
                                                                                    fluidRow(
                                                                                      plotlyOutput("QCGeneFull")
                                                                                    )
                                                                                  ),
                                                                                  tags$hr(),
                                                                                  wellPanel(
                                                                                    fluidRow(
                                                                                      plotlyOutput("densityFullUnscaled")
                                                                                    )
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
                                                                                  wellPanel(
                                                                                    fluidRow(
                                                                                      plotlyOutput("densityPositiveUnscaled")
                                                                                    )
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
                                                         fluidRow(
                                                           column(12,
                                                                  plotOutput("cnet")
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
              ),
              tabPanel("About",
                       column(12,
                              h4("Upload Data"),
                              p("scIVA allows for flexible data uploading, with multiple formats accepted, including csv, tsv, and xlsx, each with or without quotations. It also provides options to transpose the data matrices and choose headers from the uploaded dataset. Following the initial upload, the user can subset data by clusters, or upload a gene list to subset by genes, giving greater control to the user. This then gives a preview of the uploaded data, with cell, gene and cluster counts."),
                              h4("Quality Control"),
                              p("scIVA gives users access to a range of quality control measures for cells, including an interactive Beeswarm plot to compare between clusters or experimental design conditions. Sequencing depth quality control shows mean expression with proportions of cells expressing the gene. Also included is a ranking system of genes by mean and variance across the whole dataset and across clusters. This option facilitates gene selection based on gene expression pattern. scIVA also allows for data to be normalized using different procedures, and gives users control to filter outlier genes from the dataset. The resulting data can be downloaded and re-uploaded for subsequent analysis."),
                              h4("Single Gene Visualisation"),
                              p("scIVA has a comprehensive range of interactive visualisation and statistical tests for examining changes of any selected gene between clusters. The key features of scIVA is to provide the ability to visualize the data in an intuitive and interactive way. The Sunburst plot generates a layered graph of the proportion of cells with/without expression of the genes, displaying counts and percentage representation. The Beeswarm plot graphs the proportion of expressed cells and cell-specific expression level by clusters. Multiple density plots are generated to show the distribution of gene expression between clusters for all and for zero-filtered cells. A Summary expression table is generated, which displays counts, percentages and means for each cluster across all cells and positively expressed cells."),
                              h4("Single Gene Analysis"),
                              p("scIVA provides summary statistics in data browsing table by cells and clusters. To first identify genes with potential expression changes, a Kruskal-Wallis test is performed for all clusters, followed by a Kolmogorov-Smirnov test for pair-wise statistical differences in the distributions of gene expression between clusters. A modified form of likelihood ratio test, taking into account zero-inflated distribution, is provided to quantitatively perform differential expression analysis at gene level between clusters and to estimate fold change."),
                              h4("Gene List Analysis"),
                              p("scIVA provides analysis tools for a user uploaded list of multiple genes. Features include reactome pathway analysis, with p-value cutoff for enrichment tests which displays interactive genetic pathways. The user can also choose to cluster by selected genes, with options to choose the number of clusters, and whether to scale by row, with results displayed as a heatmap with dendrogram to illustrate the arrangements of clustering.")
                       )
              )
  )
)
)

