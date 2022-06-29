#' @title Assocation Rules Visualization Shiny App
#' @description Launches a Shiny App that provides an interactive interface to the visualizations of the \code{arulesViz} package.
#' The app allows users to mine rules based on all or just subsets of features, sort by criteria (lift, support, confidence) and visualize
#' using network graph, grouped bubble and scatter plots. \cr
#' Users filter rules to target only those with a certain variable on the RHS or LHS of the rule.
#' Rule mining is computed using the \link{apriori} algorithm from \code{arules}.
#' 
#' @param dataset data.frame, this is the dataset that association rules will be mined from.  Each row is treated as a transaction.  Seems to work 
#' OK when a the S4 transactions class from \code{arules} is used, however this is not thoroughly tested.
#' @param bin logical, \code{TRUE} will automatically discretize/bin numerical data into categorical features that can be used for association analysis.
#' @param vars integer, how many variables to include in initial rule mining
#' @param supp numeric, the support parameter for initializing visualization.  Useful when it is known that a high support is needed to not crash computationally.
#' @param conf numeric, the confidence parameter for initializing visualization.  Similarly useful when it is known that a high confidence is needed to not crash computationally.
#' @seealso \code{arulesViz}, \code{arules}
#' @return Shiny App
#' @import shiny arulesViz arules
#' @export
#' 


# dependencies:
devtools::source_url('https://raw.githubusercontent.com/brooksandrew/Rsenal/master/R/rules2df.R')
devtools::source_url('https://raw.githubusercontent.com/brooksandrew/Rsenal/master/R/bin.R')

arulesApp <- function (tr, bin=T, vars=1, supp=0.001, conf=0.5) {
  
  
  ## calling Shiny App
  shinyApp(ui = shinyUI(pageWithSidebar(
    
    headerPanel("Association Rules"),
    
    sidebarPanel(
      
      conditionalPanel(
        condition = "input.samp=='Sample'",
        numericInput("nrule", 'Number of Rules', 5), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab=='graph'",
        radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
      ),
      
      
      conditionalPanel(
        condition = "input.mytab=='grouped'",
        sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=10), br()
      ),
      
      conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')", 
        radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
        sliderInput("supp", "Support:", min = 0.000001, max =0.1, value = supp , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0.45, max = 0.75, value = conf , step = 1/100), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(), 
        numericInput("maxL", "Max. items per set::", 3), br(),
    
        downloadButton('downloadData', 'Download Rules as CSV')
      )
      
    ),
    
    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                  tabPanel('Graph', value='graph', visNetworkOutput("graphPlot", width='100%', height='1000px')),
                  tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                  tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                  tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                  tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                  tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                  tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
      )
    )
    
  )),
  
  server = function(input, output) {
    
    
    
    ## Extracting and Defining arules
    rules <- reactive({
     #tr <- as(dataset[,input$cols], 'transactions')
      arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
      
     
      ar <- arAll
      quality(ar)$conviction <- interestMeasure(ar, measure='conviction', transactions=tr)
      quality(ar)$hyperConfidence <- interestMeasure(ar, measure='hyperConfidence', transactions=tr)
      quality(ar)$cosine <- interestMeasure(ar, measure='cosine', transactions=tr)
      quality(ar)$chiSquare <- interestMeasure(ar, measure='chiSquare', transactions=tr)
      quality(ar)$coverage <- interestMeasure(ar, measure='coverage', transactions=tr)
      quality(ar)$doc <- interestMeasure(ar, measure='doc', transactions=tr)
      quality(ar)$gini <- interestMeasure(ar, measure='gini', transactions=tr)
      quality(ar)$hyperLift <- interestMeasure(ar, measure='hyperLift', transactions=tr)
      ar
    })
    
    # Rule length
    nR <- reactive({
      nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
    })
    
    ## Grouped Plot #########################
    output$groupedPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
    }, height=800, width=800)
    
    ## Graph Plot ##########################
    output$graphPlot <- renderVisNetwork({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='graph', engine='htmlwidget',width="100%",height = "1200px",control=list(type=input$graphType))
    })
    
    ## Scatter Plot ##########################
    output$scatterPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
    }, height=800, width=800)
    
    ## Parallel Coordinates Plot ###################
    output$paracoordPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
    }, height=800, width=800)
    
    ## Matrix Plot ###################
    output$matrixPlot <- renderPlot({
      ar <- rules()
      plot(sort(ar, by=input$sort)[1:nR()], method='matrix')
    }, height=800, width=800)
    
    ## Item Frequency Plot ##########################
    output$itemFreqPlot <- renderPlot({
     # trans <- as(dataset[,input$cols], 'transactions')
      itemFrequencyPlot(tr,topN=25,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")
    }, height=800, width=800)
    
    ## Rules Data Table ##########################
    output$rulesDataTable <- renderDataTable({
      ar <- rules()
      rulesdt <- rules2df(ar)
      rulesdt
    })
    
    ## Rules Printed ########################
    output$rulesTable <- renderPrint({
      #hack to disply results... make sure this match line above!!
      #ar <- apriori(dataset[,input$cols], parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))
      ar <- rules()
      inspect(sort(ar, by=input$sort))
    })
    
    ## Download data to csv ########################
    output$downloadData <- downloadHandler(
      filename = 'arules_data.csv',
      content = function(file) {
        write.csv(rules2df(rules()), file)
      }
    )
    
    
  },
  options = list(height = 800, width = 1600)
  )
}









