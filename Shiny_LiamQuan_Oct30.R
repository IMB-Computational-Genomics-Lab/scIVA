# install.packages('shiny')
# install.packages('plotly')
# install.packages('RColorBrewer')
# install.packages('sunburstR')
# install.packages('readr')
# install.packages('readxl')
# install.packages('d3r')
# install.packages('treemap')
# 
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
#library('DT')
# 
rm(list=ls())
# 
# ####Loading data####
# 
# path='/Users/quan.nguyen/Documents/Powell_group_MacQuan/CardioDiff/Liam/CurrentShiny/CurrentShiny/'
path='/Volumes/TSB USB DRV/CurrentShiny/'
# path='D:/CurrentShiny/'
# # -----------------------More info about prepare datasets, don't run----------------------
# # ##Creating new name files to be uploaded
# expression_0 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0.RDS'))
# Names0 <- rownames(expression_0)
# saveRDS(Names0, paste0(path, 'Day0_GeneNames.RDS'))
# 
# expression_2 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2.RDS'))
# Names2 <- rownames(expression_2)
# saveRDS(Names2, paste0(path, 'Day2_GeneNames.RDS'))
# 
# expression_5 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5.RDS'))
# Names5 <- rownames(expression_5)
# saveRDS(Names5, paste0(path, 'Day5_GeneNames.RDS'))
# 
# expression_15 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15.RDS'))
# Names15 <- rownames(expression_15)
# saveRDS(Names15, paste0(path, 'Day15_GeneNames.RDS'))
# 
# expression_30 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30.RDS'))
# Names30 <- rownames(expression_30)
# saveRDS(Names30, paste0(path, 'Day30_GeneNames.RDS'))


##prepare 10pc small datasets
# dat3d_0 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day0.RDS'))
# expression_0 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0.RDS'))
# random_10pc <-sample(1:ncol(expression_0), round(ncol(expression_0)/10), replace = F)
# expression_0_10pc <- expression_0[,random_10pc]
# dat3d_0_10pc <-dat3d_0[random_10pc,]
# saveRDS(expression_0_10pc, paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0_10pc.RDS'))
# saveRDS(dat3d_0_10pc, paste0(path,'dat_tSNE_batch_cluster_Day0_10pc.RDS'))

# dat3d_2 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day2.RDS'))
# expression_2 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2.RDS'))
# random_10pc <-sample(1:ncol(expression_2), round(ncol(expression_2)/10), replace = F)
# expression_2_10pc <- expression_2[,random_10pc]
# dat3d_2_10pc <-dat3d_2[random_10pc,]
# saveRDS(expression_2_10pc, paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2_10pc.RDS'))
# saveRDS(dat3d_2_10pc, paste0(path,'dat_tSNE_batch_cluster_Day2_10pc.RDS'))

# dat3d_5 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day5.RDS'))
# expression_5 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5.RDS'))
# random_10pc <-sample(1:ncol(expression_5), round(ncol(expression_5)/10), replace = F)
# expression_5_10pc <- expression_5[,random_10pc]
# dat3d_5_10pc <-dat3d_5[random_10pc,]
# saveRDS(expression_5_10pc, paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5_10pc.RDS'))
# saveRDS(dat3d_5_10pc, paste0(path,'dat_tSNE_batch_cluster_Day5_10pc.RDS'))

# dat3d_15 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day15.RDS'))
# expression_15 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15.RDS'))
# random_10pc <-sample(1:ncol(expression_15), round(ncol(expression_15)/10), replace = F)
# expression_15_10pc <- expression_15[,random_10pc]
# dat3d_15_10pc <-dat3d_15[random_10pc,]
# saveRDS(expression_15_10pc, paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15_10pc.RDS'))
# saveRDS(dat3d_15_10pc, paste0(path,'dat_tSNE_batch_cluster_Day15_10pc.RDS'))

# dat3d_30 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day30.RDS'))
# expression_30 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30.RDS'))
# random_10pc <-sample(1:ncol(expression_30), round(ncol(expression_30)/10), replace = F)
# expression_30_10pc <- expression_30[,random_10pc]
# dat3d_30_10pc <-dat3d_30[random_10pc,]
# saveRDS(expression_30_10pc, paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30_10pc.RDS'))
# saveRDS(dat3d_30_10pc, paste0(path,'dat_tSNE_batch_cluster_Day30_10pc.RDS'))


# #Assign a vector of names for Names0 before running because the UI executes unique(Names0)
# # -----------------------End more info about prepare datasets, don't run----------------------

Names0 <-readRDS( paste0(path, 'Day0_GeneNames.RDS'))
Names2 <-readRDS( paste0(path, 'Day2_GeneNames.RDS'))
Names5 <-readRDS( paste0(path, 'Day5_GeneNames.RDS'))
Names15 <-readRDS( paste0(path, 'Day15_GeneNames.RDS'))
Names30 <-readRDS( paste0(path, 'Day30_GeneNames.RDS'))


summary_data_big <- function(id, cluster, dat_gene){
  cellCount <- length(cluster)
  pos_idx <- which(dat_gene[cluster]>0)
  posCount <- length(pos_idx)
  posPercent <- posCount/cellCount*100
  meanExprs <- mean(na.omit(dat_gene[cluster]))
  cluster_exprs <- dat_gene[cluster]
  MeanPositive <- mean(cluster_exprs[pos_idx])
  return(list(cellCount,posCount,round(posPercent,3),round(meanExprs,3),round(MeanPositive,3)))
  cluster_exprs=NULL
}

summary_data_sunburst <- function(id, cluster, dat_gene){
  cellCount <- length(cluster)
  pos_idx <- which(dat_gene[cluster]>0)
  posCount <- length(pos_idx)
  return(list(cellCount,posCount))
}

####UI####
ui2 <- fluidPage(
  titlePanel("Single Cell Expression Data"),
  
  sidebarLayout(
    sidebarPanel(width=3,
                 selectizeInput("name",
                                label="Select a gene to display its expression:",
                                choices = c('All Subpopulations',sort(unique(c(Names0,Names2,Names5,Names15,Names30)))),
                                multiple = T,
                                options = list(maxItems = 1, placeholder = 'Select a name'),
                                selected = "All Subpopulations")
    ),
    mainPanel(
      tabsetPanel(type ="tabs",
                  tabPanel("Day0",
                           
                           radioButtons("radio0","Data Size:",
                                        choices = list("Small 10% Data Set" = "checkbox0small",
                                                       "Full Data Set" = "checkbox0"),
                                        selected = "checkbox0small"
                           ),
                           
                           HTML('<style type="text/css">
                              .well { background-color: transparent }
                              .shiny-html-output { font-size: 16px; font-family: Times}
                              </style>'),
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"),
                           
                           #conditionalPanel(condition = "Names0.filter(function(Names0){return Names0 == input.name}) == 'TRUE'",
                           
                           column(12,
                                  #Sunburst and Density######
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            textOutput("sunburstname0"),
                                                            tags$style("#sunburstname0{color: red;
                                                                                 font-size: 16px;
                                                                                 font-family: Times
                                                                                 }"
                                                            ),
                                                            sunburstOutput("sunburst0") #width = "40%", height = "100%")
                                  )
                                  ),
                                  tags$div(tags$br(),tags$br(),tags$br()),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("density0", width = "100%",  height = "100%")
                                  )
                                  ),
                                  #Add some empty rows to separate plots from the tables 
                                  tags$div(tags$br()),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("fulldensity0", width = "100%",  height = "100%")
                                  )
                                  ),
                                  tags$div(tags$br(),tags$br())
                           ),
                           
                           
                           #Tables######
                           conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                            fluidRow(
                                              column(8, 
                                                     #wellPanel(
                                                     textOutput("summaryTablename0"),
                                                     #To edit the format
                                                     tags$style("#summaryTablename0{color: red;
                                                                         font-size: 16px;
                                                                         font-family: Times
                                                                         }"
                                                     ),                                                              
                                                     tableOutput("SummaryData0"),
                                                     downloadButton('downloadData0', 'Download')
                                              )
                                            )
                           
                           ),
                           #tSNE######
                           column(12,
                                  
                                  selectizeInput("subPop0","Select a subpopulation ID to view:",
                                                 choices = c('All',1,2,3,4),
                                                 multiple = T,
                                                 options = list(maxItems = 1, placeholder = 'Select a subpopulation'),
                                                 selected = 'All'),
                                  
                                  plotlyOutput("scatter3d0", height = "100%", width = "100%")
                           )
                           
                  ),
                  
                  #tabDay2####
                  tabPanel("Day2",
                           radioButtons("radio2","Data Size:",
                                        choices = list("Small 10% Data Set" = "checkbox2small",
                                                       "Full Data Set" = "checkbox2"),
                                        selected = "checkbox2small"
                           ),
                           HTML('<style type="text/css">
                                .well { background-color: transparent }
                                .shiny-html-output { font-size: 16px; font-family: Times}
                                </style>'),
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"),
                  column(12,
                         #Sunburst and Density######
                         fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                   textOutput("sunburstname2"),
                                                   #To edit the format #tags$head(
                                                   tags$style("#sunburstname2{color: red;
                                                              font-size: 16px;
                                                              font-family: Times
                                                              }"
                                                   ),
                                                   
                                                   sunburstOutput("sunburst2") #width = "40%", height = "100%")
                         )
                         ),
                         tags$div(tags$br(),tags$br(),tags$br()),
                         fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                   plotlyOutput("density2", width = "100%",  height = "100%")
                                                   )
                         ),
                         tags$div(tags$br(),tags$br(),tags$br()),
                         fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                   plotlyOutput("fulldensity2", width = "100%",  height = "100%")
                         )
                         ),
                         tags$div(tags$br(),tags$br(),tags$br())
                  ),
                  
                  #Tables######
                  conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                   fluidRow(
                                     column(8,
                                            textOutput("summaryTablename2"),
                                            tags$style("#summaryTablename2{color: red;
                                                       font-size: 16px;
                                                       font-family: Times
                                                       }"
                                            ),                                                              
                                            tableOutput("SummaryData2"),
                                            downloadButton('downloadData2', 'Download')
                                     )
                                   )
                  ),
                  
                  #tSNE######
                  column(12,
                         selectizeInput("subPop2","Select a subpopulation ID to view:",
                                        choices = c('All',1,2,3),
                                        multiple = T,
                                        options = list(maxItems = 1, placeholder = 'Select a subpopulation'),
                                        selected = 'All'),
                         plotlyOutput("scatter3d2", height = "100%", width = "100%")
                  )
                  ),
                  
                  #tabDay5####
                  tabPanel("Day5",
                           radioButtons("radio5","Data Size:",
                                        choices = list("Small 10% Data Set" = "checkbox5small",
                                                       "Full Data Set" = "checkbox5"),
                                        selected = "checkbox5small"
                           ),
                           
                           HTML('<style type="text/css">
                                .well { background-color: transparent }
                                .shiny-html-output { font-size: 16px; font-family: Times}
                                </style>'),                           
                           #use CSS to suppress warning (error occur if use this chunk at the start. To use this in every tab panel)
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"),
                           
                           column(12,
                                  #Sunburst and Density######
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            textOutput("sunburstname5"),
                                                            #To edit the format #tags$head(
                                                            tags$style("#sunburstname5{color: red;
                                                                       font-size: 16px;
                                                                       font-family: Times
                                                                       }"
                                                            ),
                                                            
                                                            sunburstOutput("sunburst5") #width = "40%", height = "100%")
                                                            )
                                  ),
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  ),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("density5", width = "100%",  height = "100%") 
                                                            #the pixel unit % is the percent of the containing  
                                  )
                                  ),
                                  #Add some empty rows to separate plots from the tables 
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  ),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("fulldensity5", width = "100%",  height = "100%")
                                  )
                                  ),
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  )
                                  ),
                           
                           
                           #Tables######
                           conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                            fluidRow(
                                              column(8, 
                                                     #wellPanel(
                                                     textOutput("summaryTablename5"),
                                                     #To edit the format
                                                     tags$style("#summaryTablename5{color: red;
                                                                font-size: 16px;
                                                                font-family: Times
                                                                }"
                                                     ),                                                              
                                                     tableOutput("SummaryData5"),
                                                     downloadButton('downloadData5', 'Download')
                                                     )
                                              )
                                            
                                            ),
                           
                           #tSNE######
                           column(12,
                                  
                                  selectizeInput("subPop5","Select a subpopulation ID to view:",
                                                 choices = c('All',1,2,3,4),
                                                 multiple = T,
                                                 options = list(maxItems = 1, placeholder = 'Select a subpopulation'),
                                                 selected = 'All'),
                                  
                                  plotlyOutput("scatter3d5", height = "100%", width = "100%")
                           )
                           
                  ),
                  #tabDay15####
                  tabPanel("Day15",
                           radioButtons("radio15","Data Size:",
                                        choices = list("Small 10% Data Set" = "checkbox15small",
                                                       "Full Data Set" = "checkbox15"),
                                        selected = "checkbox15small"
                           ),
                           
                           HTML('<style type="text/css">
                                .well { background-color: transparent }
                                .shiny-html-output { font-size: 16px; font-family: Times}
                                </style>'),                           
                           #use CSS to suppress warning (error occur if use this chunk at the start. To use this in every tab panel)
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"),
                           
                           column(12,
                                  #Sunburst and Density######
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            textOutput("sunburstname15"),
                                                            #To edit the format #tags$head(
                                                            tags$style("#sunburstname15{color: red;
                                                                       font-size: 16px;
                                                                       font-family: Times
                                                                       }"
                                                            ),
                                                            
                                                            sunburstOutput("sunburst15") #width = "40%", height = "100%")
                                                            )
                                  ),
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  ),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("density15", width = "100%",  height = "100%") 
                                                            #the pixel unit % is the percent of the containing  
                                  )
                                  ),
                                  #Add some empty rows to separate plots from the tables 
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  ),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("fulldensity15", width = "100%",  height = "100%")
                                  )
                                  ),
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  )
                                  ),
                           
                           
                           #Tables######
                           conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                            fluidRow(
                                              column(8, 
                                                     #wellPanel(
                                                     textOutput("summaryTablename15"),
                                                     #To edit the format
                                                     tags$style("#summaryTablename15{color: red;
                                                                font-size: 16px;
                                                                font-family: Times
                                                                }"
                                                     ),                                                              
                                                     tableOutput("SummaryData15"),
                                                     downloadButton('downloadData15', 'Download')
                                                     )
                                              )
                                            
                                            ),
                           
                           #tSNE######
                           column(12,
                                  
                                  selectizeInput("subPop15","Select a subpopulation ID to view:",
                                                 choices = c('All',1,2),
                                                 multiple = T,
                                                 options = list(maxItems = 1, placeholder = 'Select a subpopulation'),
                                                 selected = 'All'),
                                  
                                  plotlyOutput("scatter3d15", height = "100%", width = "100%")
                           )
                           
                           ),
                  #tabDay30####
                  tabPanel("Day30",
                           radioButtons("radio30","Data Size:",
                                        choices = list("Small 10% Data Set" = "checkbox30small",
                                                       "Full Data Set" = "checkbox30"),
                                        selected = "checkbox30small"
                           ),
                           
                           HTML('<style type="text/css">
                                .well { background-color: transparent }
                                .shiny-html-output { font-size: 16px; font-family: Times}
                                </style>'),                           
                           #use CSS to suppress warning (error occur if use this chunk at the start. To use this in every tab panel)
                           tags$style(type="text/css",
                                      ".shiny-output-error { visibility: hidden; }",
                                      ".shiny-output-error:before { visibility: hidden; }"),
                           
                           column(12,
                                  #Sunburst and Density######
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            textOutput("sunburstname30"),
                                                            #To edit the format #tags$head(
                                                            tags$style("#sunburstname30{color: red;
                                                                       font-size: 16px;
                                                                       font-family: Times
                                                                       }"
                                                            ),
                                                            
                                                            sunburstOutput("sunburst30") #width = "40%", height = "100%")
                                                            )
                                  ),
                                  tags$div(
                                    tags$br(),
                                    tags$br(),
                                    tags$br()
                                  ),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("density30", width = "100%",  height = "100%") 
                                                            #the pixel unit % is the percent of the containing  
                                  )
                                  ),
                                  #Add some empty rows to separate plots from the tables 
                                  tags$div(tags$br(),tags$br(),tags$br()),
                                  fluidRow(conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                                            plotlyOutput("fulldensity30", width = "100%",  height = "100%")
                                  )),
                                  tags$div(tags$br(),tags$br(),tags$br())
                                  ),
                           
                           
                           #Tables######
                           conditionalPanel(condition = "input.name != 'All Subpopulations'",
                                            fluidRow(
                                              column(8, 
                                                     #wellPanel(
                                                     textOutput("summaryTablename30"),
                                                     #To edit the format
                                                     tags$style("#summaryTablename30{color: red;
                                                                font-size: 16px;
                                                                font-family: Times
                                                                }"
                                                     ),                                                              
                                                     tableOutput("SummaryData30"),
                                                     downloadButton('downloadData30', 'Download')
                                                     )
                                              )
                                            
                                            ),
                           
                           #tSNE######
                           column(12,
                                  
                                  selectizeInput("subPop30","Select a subpopulation ID to view:",
                                                 choices = c('All',1,2),
                                                 multiple = T,
                                                 options = list(maxItems = 1, placeholder = 'Select a subpopulation'),
                                                 selected = 'All'),
                                  
                                  plotlyOutput("scatter3d30", height = "100%", width = "100%")
                           )
                           
                           ),
                  #tabAbout####
                  tabPanel("About/Help"
                  )
      )
    )
  )
)


server2 <- function(input, output, session){
  
  ######################
  # Start day 0
  ######################
  
  OutputAll_day0 <- function(dat3d, expression,SBE) {
    output$scatter3d0  <- renderPlotly({
      if (input$name == 'All Subpopulations'){
        if (input$subPop0 == 'All'){
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker = list(opacity = 1, size=5), hoverinfo = 'text',
                       text = ~paste('By Day subPop: ', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying All Subpopulations on the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else
        {
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop0,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker=list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('By Day subPop :', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying Subpopulation ', input$subPop0,' for the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
      else {
        if (input$subPop0 == 'All'){
          gene_idx <- which(Names0==input$name)
          Log2GeneExpression <- log2(expression[gene_idx,]+1)
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression,
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else {
          ind <- which(dat3d$clusters %in% input$subPop0)
          expression_SP <- expression[,ind]
          gene_idx <- which(Names0==input$name)
          Log2GeneExpression <- log2(expression_SP[gene_idx,]+1)
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop0,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression,
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression_SP[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name, ', Subpopulation ', input$subPop0),
                   showlegend = T, legend=list("SubPopulation", bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
    }
    )
    
    #Density####
    
    density0 <- reactive({
      gene_idx <- which(Names0==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      if(length(grep(input$name, Names0, ignore.case = T)) > 0){
        Exprs_gene$Cluster <- dat3d$clusters
        colnames(Exprs_gene) <- c('Gene','Cluster')
        p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>0,]), aes(log2(Gene+1))) + 
          geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5)+ 
          theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
          ylab('Scaled Density') + xlab('log2(Expression+1)') + 
          scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff','#aabd00'),limits=c('1','2','3','4')) +
          ggtitle(paste0("Gene Expression of Positive Cells ", input$name))
        m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
        p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m)
        
        
      } else if (length(grep(input$name, Names0, ignore.case = T)) == 0){
        p1 <- ggplot(data.frame(x=1:1,y=1:1)) + 
          theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) +
          ggtitle(paste0("No Expression on Day0 for Gene: ", input$name))
        m1 <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
        p1 <- ggplotly(p1) %>% layout(autosize = F, width = 600, height = 400,margin = m1)}
    }
    )
    
    fulldensity0 <- reactive({
      gene_idx <- which(Names0==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      if(length(grep(input$name, Names0, ignore.case = T)) > 0){
        Exprs_gene$Cluster <- dat3d$clusters
        colnames(Exprs_gene) <- c('Gene','Cluster')
        p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>-1,]), aes(log2(Gene+1))) + 
          geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5)+ 
          theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
          ylab('Scaled Density') + xlab('log2(Expression+1)') + 
          scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff','#aabd00'),limits=c('1','2','3','4')) +
          ggtitle(paste0("Gene Expression of All Cells for: ", input$name))
        m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
        p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m)
        
        
      } else if (length(grep(input$name, Names0, ignore.case = T)) == 0){
        p1 <- ggplot(data.frame(x=1:10,y=1:10)) + 
          theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) +
          ggtitle(paste0("No Expression on Day 0 for Gene: ", input$name))
        m1 <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
        p1 <- ggplotly(p1) %>% layout(autosize = F, width = 600, height = 400,margin = m1)}

    })
    
    output$density0 <- renderPlotly(density0())
    
    output$fulldensity0 <- renderPlotly(fulldensity0())
    
    ClusterSummary <- reactive({
      gene_idx <- which(Names0==input$name)
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      cluster4 <- which(dat3d$clusters==4)
      DayAll <- c(cluster1,cluster2,cluster3,cluster4)
      dat_gene <- expression[gene_idx,]
      
      all_output <- summary_data_big(gene_idx, DayAll, dat_gene)
      cluster1_output <- summary_data_big(gene_idx, cluster1, dat_gene)
      cluster2_output <- summary_data_big(gene_idx, cluster2, dat_gene)
      cluster3_output <- summary_data_big(gene_idx, cluster3, dat_gene)
      cluster4_output <- summary_data_big(gene_idx, cluster4, dat_gene)
      
      data.frame(
        ClusterData = c("Cell Count","Positive Cells","Percent Positive Cells","Mean Expression All","Mean Expression Positive"),
        AllClusters = as.character(c(all_output[[1]],all_output[[2]],all_output[[3]],all_output[[4]],all_output[[5]])),
        Cluster1 = as.character(c(cluster1_output[[1]],cluster1_output[[2]],cluster1_output[[3]],cluster1_output[[4]],cluster1_output[[5]])),
        Cluster2 = as.character(c(cluster2_output[[1]],cluster2_output[[2]],cluster2_output[[3]],cluster2_output[[4]],cluster2_output[[5]])),
        Cluster3 = as.character(c(cluster3_output[[1]],cluster3_output[[2]],cluster3_output[[3]],cluster3_output[[4]],cluster3_output[[5]])),
        Cluster4 = as.character(c(cluster4_output[[1]],cluster4_output[[2]],cluster4_output[[3]],cluster4_output[[4]],cluster4_output[[5]]))
      )
    })
    # Summary Table 
    
    output$SummaryData0 <-renderTable({
      ClusterSummary()
    })
    
    
    output$summaryTablename0 <- renderText(paste0("Table 1. Expression in Positive vs Negative Cells for Gene: ", input$name))
    
    output$sunburstname0  <- renderText(paste0("Percent Positive vs Negative Cells for Gene: ", input$name))
    #and Sunburst####
    
    sunburstIN <- reactive({
      gene_idx <- which(Names0==input$name)
      DayAll <- as.numeric(rownames(dat3d))
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      cluster4 <- which(dat3d$clusters==4)
      dat_gene <- expression[gene_idx,]
      
      sunburst1_output <- summary_data_sunburst(gene_idx, cluster1, dat_gene)
      sunburst2_output <- summary_data_sunburst(gene_idx, cluster2, dat_gene)
      sunburst3_output <- summary_data_sunburst(gene_idx, cluster3, dat_gene)
      sunburst4_output <- summary_data_sunburst(gene_idx, cluster4, dat_gene)
      
      SBE[1,4] <- as.integer(sunburst1_output[[2]])
      SBE[2,4] <- as.integer(sunburst1_output[[1]]-sunburst1_output[[2]])
      SBE[3,4] <- as.integer(sunburst2_output[[2]])
      SBE[4,4] <- as.integer(sunburst2_output[[1]]-sunburst2_output[[2]])
      SBE[5,4] <- as.integer(sunburst3_output[[2]])
      SBE[6,4] <- as.integer(sunburst3_output[[1]]-sunburst3_output[[2]])
      SBE[7,4] <- as.integer(sunburst4_output[[2]])
      SBE[8,4] <- as.integer(sunburst4_output[[1]]-sunburst4_output[[2]])
      SBE
    })
    
    output$SunburstData0 <- renderTable({
      sunburstIN()
    }) 
    
    output$sunburst0 <- renderSunburst({
      tmSB <- treemap(sunburstIN(),
                      index=c("Day","SubPopulation","State"),
                      vSize="Count",
                      vColor="State",
                      type="index")
      
      tmSB_new <- tmSB$tm
      tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
      tmSB_new$color[which(tmSB_new$Day == 'Day')] <- '#00a6a7'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C1')] <- '#ff1c66'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C2')] <- '#02d4a1'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C3')] <- '#7354ff'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C4')] <- '#aabd00'
      tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
      tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'
      
      tmSB_nest <- d3_nest(
        tmSB_new[,c("Day","SubPopulation","State", "vSize", "color")],
        value_cols = c("vSize", "color"))
      
      add_shiny(sunburst(
        data = tmSB_nest,
        valueField = "vSize",
        count = TRUE,
        colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
        withD3 = TRUE
      ))
      
    })
    
    selection <- reactive({
      input$sunburst_mouseover
    })
    
    output$selection <- renderText(selection())
    
    #DownloadButtons####
    
    output$downloadData0 <- downloadHandler(
      filename = function(){ 
        paste(input$name, '_day','.csv', sep='') 
      },
      content = function(file){
        write.csv(ClusterSummary(), file)
      }
    )
  }
  
  Day0file<-list(paste0(path,'dat_tSNE_batch_cluster_Day0.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0.RDS'),
                 paste0(path, "Data0.xlsx")) #sunburstin table name to be changed
  dat3d_0 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day0.RDS'))
  #to display progress bar
  observeEvent(input$radio0,{
    if(input$radio0 == "checkbox0"){
      withProgress(message = 'Reading Large Data for Day 0!', value = 0, {
        
        for(i in 1:length(Day0file)){
          incProgress(1/length(Day0file), detail = paste("File #", i, " of ", length(Day0file))) #e.g. 1 of 3 
          dat3d_0 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day0.RDS'))
          expression_0 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0.RDS'))
          SBE0 <- read_excel(paste0(path, "Data0.xlsx"))
          #output all here
          OutputAll_day0(dat3d = dat3d_0, expression = expression_0,SBE = SBE0)
        } } ) }
  } )
  
  Day0file_small<-list(paste0(path,'dat_tSNE_batch_cluster_Day0_10pc.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0_10pc.RDS'),
                       paste0(path, "Data0.xlsx"))
  
  observeEvent(input$radio0,{
    if(input$radio0 == "checkbox0small"){
      withProgress(message = 'Reading Data for Day 0!', value = 0, {
        
        for(i in 1:length(Day0file)){
          incProgress(1/length(Day0file_small), detail = paste("File #", i, " of ", length(Day0file_small))) #e.g. 1 of 3 
          dat3d_0 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day0_10pc.RDS'))
          expression_0 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day0_10pc.RDS'))
          SBE0 <- read_excel(paste0(path, "Data0.xlsx"))
          #output all here
          OutputAll_day0(dat3d = dat3d_0, expression = expression_0, SBE = SBE0)
        } } ) }
  } )
  
  ######################
  # Start day 2
  ######################
  
  
  OutputAll_day2 <- function(dat3d, expression,SBE) {
    output$scatter3d2  <- renderPlotly({
      if (input$name == 'All Subpopulations'){
        if (input$subPop2 == 'All'){
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker = list(opacity = 1, size=5), hoverinfo = 'text',
                       text = ~paste('By Day subPop: ', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying All Subpopulations on the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else
        {
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop2,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker=list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('By Day subPop :', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying Subpopulation ', input$subPop2,' for the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
      else {
        if (input$subPop2 == 'All'){
          gene_idx <- which(Names2==input$name)
          Log2GeneExpression <- log2(expression[gene_idx,]+1)
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression, #~log2(expression[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else {
          ind <- which(dat3d$clusters %in% input$subPop2)
          expression_day <- expression[,ind]
          gene_idx <- which(Names2==input$name)
          
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop2,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~log2(expression_day[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression_day[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name, ', Subpopulation ', input$subPop2),
                   showlegend = T, legend=list("SubPopulation", bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
    }
    )
    #Density####
    
    density2 <- reactive({
      gene_idx  <- which(Names2==input$name)
      Exprs_gene <-as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <-c('Gene', 'Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>0,]), aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff'),limits=c('1','2','3')) +
        ggtitle(paste0("Gene Expression of Positive Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    })
    
    fulldensity2 <- reactive({
      gene_idx <- which(Names2==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <- c('Gene','Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>-1,]), aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff'),limits=c('1','2','3')) +
        ggtitle(paste0("Gene Expression of All Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    })
    
    output$density2 <- renderPlotly(density2())
    
    output$fulldensity2 <- renderPlotly(fulldensity2())
    
    ClusterSummary <- reactive({
      gene_idx <- which(Names2==input$name)
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      DayAll <- c(cluster1,cluster2,cluster3)
      dat_gene <- expression[gene_idx,]
      
      all_output <- summary_data_big(gene_idx, DayAll, dat_gene)
      cluster1_output <- summary_data_big(gene_idx, cluster1, dat_gene)
      cluster2_output <- summary_data_big(gene_idx, cluster2, dat_gene)
      cluster3_output <- summary_data_big(gene_idx, cluster3, dat_gene)
      
      data.frame(
        ClusterData = c("Cell Count","Positive Cells","Percent Positive Cells","Mean Expression All","Mean Expression Positive"),
        AllClusters = as.character(c(all_output[[1]],all_output[[2]],all_output[[3]],all_output[[4]],all_output[[5]])),
        Cluster1 = as.character(c(cluster1_output[[1]],cluster1_output[[2]],cluster1_output[[3]],cluster1_output[[4]],cluster1_output[[5]])),
        Cluster2 = as.character(c(cluster2_output[[1]],cluster2_output[[2]],cluster2_output[[3]],cluster2_output[[4]],cluster2_output[[5]])),
        Cluster3 = as.character(c(cluster3_output[[1]],cluster3_output[[2]],cluster3_output[[3]],cluster3_output[[4]],cluster3_output[[5]]))
        )
    })
    # Summary Table 
    
    output$SummaryData2 <-renderTable({
      ClusterSummary()
    })
    
    
    output$summaryTablename2 <- renderText(paste0("Table 1. Expression in Positive vs Negative Cells for Gene: ", input$name))
    
    output$sunburstname2  <- renderText(paste0("Percent Positive vs Negative Cells for Gene: ", input$name))
    #and Sunburst####
    
    sunburstIN <- reactive({
      gene_idx <- which(Names2==input$name)
      DayAll <- as.numeric(rownames(dat3d))
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      dat_gene <- expression[gene_idx,]
      
      sunburst1_output <- summary_data_sunburst(gene_idx, cluster1, dat_gene)
      sunburst2_output <- summary_data_sunburst(gene_idx, cluster2, dat_gene)
      sunburst3_output <- summary_data_sunburst(gene_idx, cluster3, dat_gene)
      
      SBE[1,4] <- as.integer(sunburst1_output[[2]])
      SBE[2,4] <- as.integer(sunburst1_output[[1]]-sunburst1_output[[2]])
      SBE[3,4] <- as.integer(sunburst2_output[[2]])
      SBE[4,4] <- as.integer(sunburst2_output[[1]]-sunburst2_output[[2]])
      SBE[5,4] <- as.integer(sunburst3_output[[2]])
      SBE[6,4] <- as.integer(sunburst3_output[[1]]-sunburst3_output[[2]])
      SBE
    })
    
    output$SunburstData2 <- renderTable({
      sunburstIN()
    }) 
    
    output$sunburst2 <- renderSunburst({
      tmSB <- treemap(sunburstIN(),
                      index=c("Day","SubPopulation","State"),
                      vSize="Count",
                      vColor="State",
                      type="index")
      
      tmSB_new <- tmSB$tm
      tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
      tmSB_new$color[which(tmSB_new$Day == 'Day')] <- '#00a6a7'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C1')] <- '#ff1c66'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C2')] <- '#02d4a1'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C3')] <- '#7354ff'
      tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
      tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'
      
      tmSB_nest <- d3_nest(
        tmSB_new[,c("Day","SubPopulation","State", "vSize", "color")],
        value_cols = c("vSize", "color"))
      
      add_shiny(sunburst(
        data = tmSB_nest,
        valueField = "vSize",
        count = TRUE,
        colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
        withD3 = TRUE
      ))
      
    })
    
    selection <- reactive({
      input$sunburst_mouseover
    })
    
    output$selection <- renderText(selection())
    
    #DownloadButtons####
    
    output$downloadData2 <- downloadHandler(
      filename = function(){ 
        paste(input$name, '_day','.csv', sep='') 
      },
      content = function(file){
        write.csv(ClusterSummary(), file)
      }
    )
  }
  
  Day2file<-list(paste0(path,'dat_tSNE_batch_cluster_Day2.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2.RDS'),
                 paste0(path, "Data2.xlsx"))
  
  
  observeEvent(input$radio2,{
    if(input$radio2 == "checkbox2"){
      withProgress(message = 'Reading Full Data for Day 2!', value = 0, {
        for(i in 1:length(Day2file)){
          incProgress(1/length(Day2file), detail = paste("File #", i, " of ", length(Day2file))) #e.g. 1 of 3
          dat3d_2 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day2.RDS'))
          expression_2 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2.RDS'))
          SBE2 <- read_excel(paste0(path, "Data2.xlsx"))
          OutputAll_day2(dat3d=dat3d_2, expression=expression_2,SBE=SBE2)
        }
      }
      )
    }
  })
  
  Day2file_small<-list(paste0(path,'dat_tSNE_batch_cluster_Day2_10pc.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2_10pc.RDS'),
                       paste0(path, "Data2.xlsx")) #sunburstin table name to be changed
  
  
  observeEvent(input$radio2,{
    if(input$radio2 == "checkbox2small"){
      withProgress(message = 'Reading Small Data for Day 2!', value = 0, {
        for(i in 1:length(Day2file_small)){
          incProgress(1/length(Day2file_small), detail = paste("File #", i, " of ", length(Day2file_small))) #e.g. 1 of 3
          dat3d_2 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day2_10pc.RDS'))
          expression_2 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day2_10pc.RDS'))
          SBE2 <- read_excel(paste0(path, "Data2.xlsx"))
          OutputAll_day2(dat3d=dat3d_2, expression=expression_2,SBE=SBE2)
        }
      }
      )
    }
  } )
  
  ######################
  # Start day 5
  ######################
  
  OutputAll_day5 <- function(dat3d, expression,SBE) {
    output$scatter3d5  <- renderPlotly({
      if (input$name == 'All Subpopulations'){
        if (input$subPop5 == 'All'){
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker = list(opacity = 1, size=5), hoverinfo = 'text',
                       text = ~paste('By Day subPop: ', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying All Subpopulations on the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else
        {
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop5,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker=list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('By Day subPop :', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying Subpopulation ', input$subPop5,' for the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
      else {
        if (input$subPop5 == 'All'){
          gene_idx <- which(Names5==input$name)
          Log2GeneExpression <- log2(expression[gene_idx,]+1)
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression, #~log2(expression[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else {
          ind <- which(dat3d$clusters %in% input$subPop5)
          expression_day <- expression[,ind]
          gene_idx <- which(Names5==input$name)
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop5,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~log2(expression_day[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression_day[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name, ', Subpopulation ', input$subPop5),
                   showlegend = T, legend=list("SubPopulation", bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
    }
    )
    
    #Density####
    
    density5 <- reactive({
      gene_idx  <- which(Names5==input$name)
      Exprs_gene <-as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <-c('Gene', 'Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>0,]) , aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff','#aabd00'),limits=c('1','2','3','4')) +
        ggtitle(paste0("Gene Expression of Positive Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    }
    )
    
    fulldensity5 <- reactive({
      gene_idx <- which(Names5==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <- c('Gene','Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>-1,]), aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1','#7354ff','#aabd00'),limits=c('1','2','3','4')) +
        ggtitle(paste0("Gene Expression of All Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    })
    
    output$density5 <- renderPlotly(density5())
    
    output$fulldensity5 <- renderPlotly(fulldensity5())
    
    ClusterSummary <- reactive({
      gene_idx <- which(Names5==input$name)
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      cluster4 <- which(dat3d$clusters==4)
      DayAll <- c(cluster1,cluster2,cluster3,cluster4)
      dat_gene <- expression[gene_idx,]
      
      all_output <- summary_data_big(gene_idx, DayAll, dat_gene)
      cluster1_output <- summary_data_big(gene_idx, cluster1, dat_gene)
      cluster2_output <- summary_data_big(gene_idx, cluster2, dat_gene)
      cluster3_output <- summary_data_big(gene_idx, cluster3, dat_gene)
      cluster4_output <- summary_data_big(gene_idx, cluster4, dat_gene)
      
      data.frame(
        ClusterData = c("Cell Count","Positive Cells","Percent Positive Cells","Mean Expression All","Mean Expression Positive"),
        AllClusters = as.character(c(all_output[[1]],all_output[[2]],all_output[[3]],all_output[[4]],all_output[[5]])),
        Cluster1 = as.character(c(cluster1_output[[1]],cluster1_output[[2]],cluster1_output[[3]],cluster1_output[[4]],cluster1_output[[5]])),
        Cluster2 = as.character(c(cluster2_output[[1]],cluster2_output[[2]],cluster2_output[[3]],cluster2_output[[4]],cluster2_output[[5]])),
        Cluster3 = as.character(c(cluster3_output[[1]],cluster3_output[[2]],cluster3_output[[3]],cluster3_output[[4]],cluster3_output[[5]])),
        Cluster4 = as.character(c(cluster4_output[[1]],cluster4_output[[2]],cluster4_output[[3]],cluster4_output[[4]],cluster4_output[[5]]))
      )
    })
    # Summary Table 
    
    output$SummaryData5 <-renderTable({
      ClusterSummary()
    })
    
    
    output$summaryTablename5 <- renderText(paste0("Table 1. Expression in Positive vs Negative Cells for Gene: ", input$name))
    
    output$sunburstname5  <- renderText(paste0("Percent Positive vs Negative Cells for Gene: ", input$name))
    #and Sunburst####
    
    sunburstIN <- reactive({
      gene_idx <- which(Names5==input$name)
      DayAll <- as.numeric(rownames(dat3d))
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      cluster3 <- which(dat3d$clusters==3)
      cluster4 <- which(dat3d$clusters==4)
      dat_gene <- expression[gene_idx,]
      
      sunburst1_output <- summary_data_sunburst(gene_idx, cluster1, dat_gene)
      sunburst2_output <- summary_data_sunburst(gene_idx, cluster2, dat_gene)
      sunburst3_output <- summary_data_sunburst(gene_idx, cluster3, dat_gene)
      sunburst4_output <- summary_data_sunburst(gene_idx, cluster4, dat_gene)
      
      SBE[1,4] <- as.integer(sunburst1_output[[2]])
      SBE[2,4] <- as.integer(sunburst1_output[[1]]-sunburst1_output[[2]])
      SBE[3,4] <- as.integer(sunburst2_output[[2]])
      SBE[4,4] <- as.integer(sunburst2_output[[1]]-sunburst2_output[[2]])
      SBE[5,4] <- as.integer(sunburst3_output[[2]])
      SBE[6,4] <- as.integer(sunburst3_output[[1]]-sunburst3_output[[2]])
      SBE[7,4] <- as.integer(sunburst4_output[[2]])
      SBE[8,4] <- as.integer(sunburst4_output[[1]]-sunburst4_output[[2]])
      SBE
    })
    
    output$SunburstData5 <- renderTable({
      sunburstIN()
    }) 
    
    output$sunburst5 <- renderSunburst({
      tmSB <- treemap(sunburstIN(),
                      index=c("Day","SubPopulation","State"),
                      vSize="Count",
                      vColor="State",
                      type="index")
      
      tmSB_new <- tmSB$tm
      tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
      tmSB_new$color[which(tmSB_new$Day == 'Day')] <- '#00a6a7'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C1')] <- '#ff1c66'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C2')] <- '#02d4a1'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C3')] <- '#7354ff'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C4')] <- '#aabd00'
      tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
      tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'
      
      tmSB_nest <- d3_nest(
        tmSB_new[,c("Day","SubPopulation","State", "vSize", "color")],
        value_cols = c("vSize", "color"))
      
      add_shiny(sunburst(
        data = tmSB_nest,
        valueField = "vSize",
        count = TRUE,
        colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
        withD3 = TRUE
      ))
      
    })
    
    selection <- reactive({
      input$sunburst_mouseover
    })
    
    output$selection <- renderText(selection())
    
    #DownloadButtons####
    
    output$downloadData5 <- downloadHandler(
      filename = function(){ 
        paste(input$name, '_day5','.csv', sep='') 
      },
      content = function(file){
        write.csv(ClusterSummary(), file)
      }
    )
  }
  
  Day5file<-list(paste0(path,'dat_tSNE_batch_cluster_Day5.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5.RDS'),
                 paste0(path, "Data5.xlsx"))
  dat3d_5 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day5.RDS'))
  #to display progress bar
  observeEvent(input$radio5,{
    if(input$radio5 == "checkbox5"){
      withProgress(message = 'Reading Large Data for Day 5!', value = 0, {
        
        for(i in 1:length(Day5file)){
          incProgress(1/length(Day5file), detail = paste("File #", i, " of ", length(Day5file))) #e.g. 1 of 3 
          dat3d_5 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day5.RDS'))
          expression_5 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5.RDS'))
          SBE5 <- read_excel(paste0(path, "Data5.xlsx"))
          #output all here
          OutputAll_day5(dat3d = dat3d_5, expression = expression_5,SBE = SBE5)
        } } ) }
  } )
  
  Day5file_small<-list(paste0(path,'dat_tSNE_batch_cluster_Day5_10pc.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5_10pc.RDS'),
                       paste0(path, "Data5.xlsx"))
  
  observeEvent(input$radio5,{
    if(input$radio5 == "checkbox5small"){
      withProgress(message = 'Reading Data for Day 5!', value = 0, {
        
        for(i in 1:length(Day5file)){
          incProgress(1/length(Day5file_small), detail = paste("File #", i, " of ", length(Day5file_small))) #e.g. 1 of 3 
          dat3d_5 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day5_10pc.RDS'))
          expression_5 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day5_10pc.RDS'))
          SBE5 <- read_excel(paste0(path, "Data5.xlsx"))
          #output all here
          OutputAll_day5(dat3d = dat3d_5, expression = expression_5, SBE = SBE5)
        } } ) }
  } )
  
  
  ######################
  # Start day 15
  ######################
  
  OutputAll_day15 <- function(dat3d, expression,SBE) {
    output$scatter3d15  <- renderPlotly({
      if (input$name == 'All Subpopulations'){
        if (input$subPop15 == 'All'){
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker = list(opacity = 1, size=5), hoverinfo = 'text',
                       text = ~paste('By Day subPop: ', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying All Subpopulations on the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else
        {
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop15,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker=list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('By Day subPop :', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying Subpopulation ', input$subPop15,' for the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
      else {
        if (input$subPop15 == 'All'){
          gene_idx <- which(Names15==input$name)
          Log2GeneExpression <- log2(expression[gene_idx,]+1)
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression, #~log2(expression[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else {
          ind <- which(dat3d$clusters %in% input$subPop15)
          expression_day <- expression[,ind]
          gene_idx <- which(Names15==input$name)
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop15,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~log2(expression_day[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression_day[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name, ', Subpopulation ', input$subPop15),
                   showlegend = T, legend=list("SubPopulation", bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
    }
    )
    
    #Density####
    
    density15 <- reactive({
      gene_idx  <- which(Names15==input$name)
      Exprs_gene <-as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <-c('Gene', 'Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>0,]) , aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1'),limits=c('1','2')) +
        ggtitle(paste0("Gene Expression of Positive Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    }
    )
    
    fulldensity15 <- reactive({
      gene_idx <- which(Names15==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <- c('Gene','Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>-1,]), aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1'),limits=c('1','2')) +
        ggtitle(paste0("Gene Expression of All Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    })
    
    output$density15 <- renderPlotly(density15())
    
    output$fulldensity15 <- renderPlotly(fulldensity15())
    
    ClusterSummary <- reactive({
      gene_idx <- which(Names15==input$name)
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      DayAll <- c(cluster1,cluster2)
      dat_gene <- expression[gene_idx,]
      
      all_output <- summary_data_big(gene_idx, DayAll, dat_gene)
      cluster1_output <- summary_data_big(gene_idx, cluster1, dat_gene)
      cluster2_output <- summary_data_big(gene_idx, cluster2, dat_gene)
      
      data.frame(
        ClusterData = c("Cell Count","Positive Cells","Percent Positive Cells","Mean Expression All","Mean Expression Positive"),
        AllClusters = as.character(c(all_output[[1]],all_output[[2]],all_output[[3]],all_output[[4]],all_output[[5]])),
        Cluster1 = as.character(c(cluster1_output[[1]],cluster1_output[[2]],cluster1_output[[3]],cluster1_output[[4]],cluster1_output[[5]])),
        Cluster2 = as.character(c(cluster2_output[[1]],cluster2_output[[2]],cluster2_output[[3]],cluster2_output[[4]],cluster2_output[[5]]))
        )
    })
    # Summary Table 
    
    output$SummaryData15 <-renderTable({
      ClusterSummary()
    })
    
    
    output$summaryTablename15 <- renderText(paste0("Table 1. Expression in Positive vs Negative Cells for Gene: ", input$name))
    
    output$sunburstname15  <- renderText(paste0("Percent Positive vs Negative Cells for Gene: ", input$name))
    #and Sunburst####
    
    sunburstIN <- reactive({
      gene_idx <- which(Names15==input$name)
      DayAll <- as.numeric(rownames(dat3d))
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      dat_gene <- expression[gene_idx,]
      
      sunburst1_output <- summary_data_sunburst(gene_idx, cluster1, dat_gene)
      sunburst2_output <- summary_data_sunburst(gene_idx, cluster2, dat_gene)
      
      SBE[1,4] <- as.integer(sunburst1_output[[2]])
      SBE[2,4] <- as.integer(sunburst1_output[[1]]-sunburst1_output[[2]])
      SBE[3,4] <- as.integer(sunburst2_output[[2]])
      SBE[4,4] <- as.integer(sunburst2_output[[1]]-sunburst2_output[[2]])
      SBE
    })
    
    output$SunburstData15 <- renderTable({
      sunburstIN()
    }) 
    
    output$sunburst15 <- renderSunburst({
      tmSB <- treemap(sunburstIN(),
                      index=c("Day","SubPopulation","State"),
                      vSize="Count",
                      vColor="State",
                      type="index")
      
      tmSB_new <- tmSB$tm
      tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
      tmSB_new$color[which(tmSB_new$Day == 'Day')] <- '#00a6a7'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C1')] <- '#ff1c66'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C2')] <- '#02d4a1'
      tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
      tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'
      
      tmSB_nest <- d3_nest(
        tmSB_new[,c("Day","SubPopulation","State", "vSize", "color")],
        value_cols = c("vSize", "color"))
      
      add_shiny(sunburst(
        data = tmSB_nest,
        valueField = "vSize",
        count = TRUE,
        colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
        withD3 = TRUE
      ))
      
    })
    
    selection <- reactive({
      input$sunburst_mouseover
    })
    
    output$selection <- renderText(selection())
    
    #DownloadButtons####
    
    output$downloadData15 <- downloadHandler(
      filename = function(){ 
        paste(input$name, '_day5','.csv', sep='') 
      },
      content = function(file){
        write.csv(ClusterSummary(), file)
      }
    )
  }
  
  Day15file<-list(paste0(path,'dat_tSNE_batch_cluster_Day15.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15.RDS'),
                 paste0(path, "Data15.xlsx"))
  dat3d_15 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day15.RDS'))
  #to display progress bar
  observeEvent(input$radio15,{
    if(input$radio15 == "checkbox15"){
      withProgress(message = 'Reading Large Data for Day 15!', value = 0, {
        
        for(i in 1:length(Day15file)){
          incProgress(1/length(Day15file), detail = paste("File #", i, " of ", length(Day15file))) #e.g. 1 of 3 
          dat3d_15 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day15.RDS'))
          expression_15 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15.RDS'))
          SBE15 <- read_excel(paste0(path, "Data15.xlsx"))
          #output all here
          OutputAll_day15(dat3d = dat3d_15, expression = expression_15,SBE = SBE15)
        } } ) }
  } )
  
  Day15file_small<-list(paste0(path,'dat_tSNE_batch_cluster_Day15_10pc.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15_10pc.RDS'),
                       paste0(path, "Data15.xlsx"))
  
  observeEvent(input$radio15,{
    if(input$radio15 == "checkbox15small"){
      withProgress(message = 'Reading Data for Day 15!', value = 0, {
        
        for(i in 1:length(Day15file)){
          incProgress(1/length(Day15file_small), detail = paste("File #", i, " of ", length(Day15file_small))) #e.g. 1 of 3 
          dat3d_15 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day15_10pc.RDS'))
          expression_15 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day15_10pc.RDS'))
          SBE15 <- read_excel(paste0(path, "Data15.xlsx"))
          #output all here
          OutputAll_day15(dat3d = dat3d_15, expression = expression_15, SBE = SBE15)
        } } ) }
  } )
  
  ######################
  # Start day 30
  ######################
  
  OutputAll_day30 <- function(dat3d, expression,SBE) {
    output$scatter3d30  <- renderPlotly({
      if (input$name == 'All Subpopulations'){
        if (input$subPop30 == 'All'){
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker = list(opacity = 1, size=5), hoverinfo = 'text',
                       text = ~paste('By Day subPop: ', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying All Subpopulations on the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else
        {
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop30,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~factor(clusters), colors= c('#273f5d','#4d76ca','#8bb2db','#6ad0dc'),
                       marker=list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('By Day subPop :', clusters, '<br> ', batches)) %>%
            layout(title = paste0('Displaying Subpopulation ', input$subPop30,' for the Selected Day'),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
      else {
        if (input$subPop30 == 'All'){
          gene_idx <- which(Names30==input$name)
          Log2GeneExpression <- log2(expression[gene_idx,]+1)
          p <- plot_ly(dat3d, x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~Log2GeneExpression, #~log2(expression[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name),
                   showlegend = T, legend=list(bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
        else {
          ind <- which(dat3d$clusters %in% input$subPop30)
          expression_day <- expression[,ind]
          gene_idx <- which(Names30==input$name)
          p <- plot_ly(dat3d[dat3d$clusters == input$subPop30,], x = ~tSNE1, y = ~tSNE2,  z= ~tSNE3, type="scatter3d", mode = 'markers',
                       color = ~log2(expression_day[gene_idx,]+1),
                       colors = colorRamp(c('lightgrey','#fdd49e','#fdbb84','#fc8d59','#ef6548','red')),
                       marker = list(opacity = 1, size=5),hoverinfo = 'text',
                       text = ~paste('Subpopulation:', clusters, '<br> ', batches,'<br> Exprs:', expression_day[gene_idx[1],])) %>%
            layout(title = paste0('Displaying gene ', input$name, ', Subpopulation ', input$subPop30),
                   showlegend = T, legend=list("SubPopulation", bgcolor='#E2E2E2',bordercolor='#FFFFFF',borderwidth=2))
        }
      }
    }
    )
    
    #Density####
    
    density30 <- reactive({
      gene_idx  <- which(Names30==input$name)
      Exprs_gene <-as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <-c('Gene', 'Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>0,]) , aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1'),limits=c('1','2')) +
        ggtitle(paste0("Gene Expression of Positive Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    }
    )
    
    fulldensity30 <- reactive({
      gene_idx <- which(Names30==input$name)
      Exprs_gene <- as.data.frame(expression[gene_idx,])
      Exprs_gene$Cluster <- dat3d$clusters
      colnames(Exprs_gene) <- c('Gene','Cluster')
      p <- ggplot(data=(Exprs_gene[Exprs_gene$Gene>-1,]), aes(log2(Gene+1))) + 
        geom_density(aes( y=..scaled.., fill=as.factor(Cluster)), alpha=0.5) + 
        theme_bw() + theme(text = element_text(family = "Times", colour = "red" ,size=10)) + 
        ylab('Scaled Density') + xlab('log2(Expression+1)') + 
        scale_fill_manual(name="Subpop", values=c('#ff1c66','#02d4a1'),limits=c('1','2')) +
        ggtitle(paste0("Gene Expression of All Cells for: ", input$name) )
      m <- list(l = 100, r = 0, b = 100, t = 50, pad = 4)
      p <- ggplotly(p)  %>% layout(autosize = F, width = 600, height = 400, margin = m) 
    })
    
    output$density30 <- renderPlotly(density30())
    
    output$fulldensity30 <- renderPlotly(fulldensity30())
    
    ClusterSummary <- reactive({
      gene_idx <- which(Names30==input$name)
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      DayAll <- c(cluster1,cluster2)
      dat_gene <- expression[gene_idx,]
      
      all_output <- summary_data_big(gene_idx, DayAll, dat_gene)
      cluster1_output <- summary_data_big(gene_idx, cluster1, dat_gene)
      cluster2_output <- summary_data_big(gene_idx, cluster2, dat_gene)
      
      data.frame(
        ClusterData = c("Cell Count","Positive Cells","Percent Positive Cells","Mean Expression All","Mean Expression Positive"),
        AllClusters = as.character(c(all_output[[1]],all_output[[2]],all_output[[3]],all_output[[4]],all_output[[5]])),
        Cluster1 = as.character(c(cluster1_output[[1]],cluster1_output[[2]],cluster1_output[[3]],cluster1_output[[4]],cluster1_output[[5]])),
        Cluster2 = as.character(c(cluster2_output[[1]],cluster2_output[[2]],cluster2_output[[3]],cluster2_output[[4]],cluster2_output[[5]]))
      )
    })
    # Summary Table 
    
    output$SummaryData30 <-renderTable({
      ClusterSummary()
    })
    
    
    output$summaryTablename30 <- renderText(paste0("Table 1. Expression in Positive vs Negative Cells for Gene: ", input$name))
    
    output$sunburstname30  <- renderText(paste0("Percent Positive vs Negative Cells for Gene: ", input$name))
    #and Sunburst####
    
    sunburstIN <- reactive({
      gene_idx <- which(Names30==input$name)
      DayAll <- as.numeric(rownames(dat3d))
      cluster1 <- which(dat3d$clusters==1)
      cluster2 <- which(dat3d$clusters==2)
      dat_gene <- expression[gene_idx,]
      
      sunburst1_output <- summary_data_sunburst(gene_idx, cluster1, dat_gene)
      sunburst2_output <- summary_data_sunburst(gene_idx, cluster2, dat_gene)
      
      SBE[1,4] <- as.integer(sunburst1_output[[2]])
      SBE[2,4] <- as.integer(sunburst1_output[[1]]-sunburst1_output[[2]])
      SBE[3,4] <- as.integer(sunburst2_output[[2]])
      SBE[4,4] <- as.integer(sunburst2_output[[1]]-sunburst2_output[[2]])
      SBE
    })
    
    output$SunburstData30 <- renderTable({
      sunburstIN()
    }) 
    
    output$sunburst30 <- renderSunburst({
      tmSB <- treemap(sunburstIN(),
                      index=c("Day","SubPopulation","State"),
                      vSize="Count",
                      vColor="State",
                      type="index")
      
      tmSB_new <- tmSB$tm
      tmSB_new$vSize[which(is.na(tmSB_new$State))] <- 0
      tmSB_new$color[which(tmSB_new$Day == 'Day')] <- '#00a6a7'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C1')] <- '#ff1c66'
      tmSB_new$color[which(tmSB_new$SubPopulation == 'C2')] <- '#02d4a1'
      tmSB_new$color[which(tmSB_new$State == 'On')] <- '#f5001a'
      tmSB_new$color[which(tmSB_new$State == 'Off')] <- '#d5d4d2'
      
      tmSB_nest <- d3_nest(
        tmSB_new[,c("Day","SubPopulation","State", "vSize", "color")],
        value_cols = c("vSize", "color"))
      
      add_shiny(sunburst(
        data = tmSB_nest,
        valueField = "vSize",
        count = TRUE,
        colors = htmlwidgets::JS("function(d){return d3.select(this).datum().data.color;}"),
        withD3 = TRUE
      ))
      
    })
    
    selection <- reactive({
      input$sunburst_mouseover
    })
    
    output$selection <- renderText(selection())
    
    #DownloadButtons####
    
    output$downloadData30 <- downloadHandler(
      filename = function(){ 
        paste(input$name, '_day30','.csv', sep='') 
      },
      content = function(file){
        write.csv(ClusterSummary(), file)
      }
    )
  }
  
  Day30file<-list(paste0(path,'dat_tSNE_batch_cluster_Day30.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30.RDS'),
                  paste0(path, "Data30.xlsx"))
  dat3d_30 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day30.RDS'))
  #to display progress bar
  observeEvent(input$radio30,{
    if(input$radio30 == "checkbox30"){
      withProgress(message = 'Reading Large Data for Day 30!', value = 0, {
        
        for(i in 1:length(Day30file)){
          incProgress(1/length(Day30file), detail = paste("File #", i, " of ", length(Day30file))) #e.g. 1 of 3 
          dat3d_30 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day30.RDS'))
          expression_30 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30.RDS'))
          SBE30 <- read_excel(paste0(path, "Data30.xlsx"))
          #output all here
          OutputAll_day30(dat3d = dat3d_30, expression = expression_30,SBE = SBE30)
        } } ) }
  } )
  
  Day30file_small<-list(paste0(path,'dat_tSNE_batch_cluster_Day30_10pc.RDS'), paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30_10pc.RDS'),
                        paste0(path, "Data30.xlsx"))
  
  observeEvent(input$radio30,{
    if(input$radio30 == "checkbox30small"){
      withProgress(message = 'Reading Data for Day 30!', value = 0, {
        
        for(i in 1:length(Day30file)){
          incProgress(1/length(Day30file_small), detail = paste("File #", i, " of ", length(Day30file_small))) #e.g. 1 of 3 
          dat3d_30 <- readRDS(paste0(path,'dat_tSNE_batch_cluster_Day30_10pc.RDS'))
          expression_30 <- readRDS(paste0(path,'Exprs_DCVLnorm_unlog_minus1_pos_Day30_10pc.RDS'))
          SBE30 <- read_excel(paste0(path, "Data30.xlsx"))
          #output all here
          OutputAll_day30(dat3d = dat3d_30, expression = expression_30, SBE = SBE30)
        } } ) }
  } )
}



shinyApp(ui = ui2, server = server2)

