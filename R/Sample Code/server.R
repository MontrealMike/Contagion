#' @name server.R
#' 
#' @author Michael Bleau
#' 
#' @changes
#'  2018-09-12 (MRB)
#'   - addition of Container Analysis section
#'   - major rewrite of how certain reports are displayed to eliminate warning messages (bugs really)
#'  2018-01-31 (MRB)
#'   - add Management Reporting section
#'  2019-05-23 (MRB)
#'   - refactor Management Reporting section 
rm(list=ls())

library(shiny)
require(shinycssloaders)
source("R/ShinyHelperFunctions.R")
source("R/UtilityFunctions.R")

options(shiny.error = browser,
        shiny.reactlog = TRUE,
        DT.options = list(pageLength = 400))

shinyServer(function(input, output, session) {
  
  # Detail report -----------------------------------------------------------------------------------------
  # when run report is pressed
  drTable <- eventReactive(input$drRunReport, {
    source("R/DetailReport.R")
    GetDetailReport(
      nodeSelection = input$drHierarchySelection, 
      level = input$drLevelSelection,
      geoSelection = input$drGeoSelection,
      timeSelection = input$drTimeSelection,
      weighting = input$drWeightingSelection)
  })
  output$detailReport <- DT::renderDataTable(drTable())
  
  # Attest report ---------------
  # when run report is pressed
  arTable <-eventReactive(input$arRunReport, {
    source("R/AttestReport.R")
    r <- GetAttestReport( 
      nodeSelection = input$arHierarchySelection, 
      level = input$arLevelSelection,
      geoSelection = input$arGeoSelection,
      timeSelection = input$arTimeSelection,
      weighting = input$arWeightingSelection)
  })
  output$attestReport <- DT::renderDataTable(arTable())
  
  # Summary Program Comparison report -----------------------------------------------------------------------------------------
  # when run report is pressed
  pcrTable <- eventReactive(input$pcrRunReport, {
    source("R/ProgramComparisonReport.R")
    GetProgramComparisonReport(
      nodeID = input$pcrHierarchySelection, 
      level = input$pcrLevelSelection,
      timeSelection = input$pcrTimeSelection,
      weighting = input$pcrWeightingSelection,
      statisticID = input$pcrStatisticSelection,
      variableName = input$pcrVariableSelection,
      includeOES = FALSE)   # 2018-10-12 (MRB) OES removed from system  
  })
  output$programComparisonReport <- DT::renderDataTable(pcrTable())
  
  # Detail Program Comparison report and chart --------------------------------------------------------
  
  # This report contains 2 columns and a dynamically established number of rows. 
  # There is a row for each selected program and an optional summary row (the first row).
  # The first column of each row presents the data in tabular form, the second, graphically
  
  observe({
    # Node list depends on product hierarchy selection
    # 2018-08-23 (MRB) changed leaf = from TRUE to FALSE
    nodeList <- NodeSelectionList(input$prctHierarchySelection, skip = FALSE, leaf = FALSE)
    updateSelectizeInput(session, "prctNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  prctStatus <- reactiveValues()
  
  # when the run report button is pressed, all the tables and charts are produced
  prctChartTable <- eventReactive(input$prctRunReport, {
    req(input$prctNodeSelection)
    req(input$prctProgramIDs)
    
    # if this report has already been run, remove the existing dynamic UI component
    if (!is.null(prctStatus$UICreated)) {
      removeUI(selector = "#prctTableChartDiv")
      prctStatus$UICreated <- NULL
    }
    
    source("R/ProgramComparisonChartTable.R")
    chartTable <- GetPrctChartTable(nodeID = input$prctNodeSelection,
                                    programList = input$prctProgramIDs,
                                    includeSummary = input$prctIncludeSummary,
                                    weighting = input$prctWeightingSelection,
                                    statisticID = input$prctStatisticSelection,
                                    variableName = input$prctVariableSelection,
                                    timeParms = list(timeUnit = input$prctTimeUnitSelection,
                                                     startQuarterID = input$prctQtrStart,
                                                     quarterCount = input$prctQtrCount,
                                                     startHalfID = input$prctHalfStart,
                                                     halfCount = input$prctHalfCount,
                                                     yearRange = input$prctYrRange))

    # when the data to be presented changes, the UI components,
    # which are a list of 2 column rows are generated
    # each component is given a name: prctTable_i and pcrtChart_i where i is the row number.
    fluidRowCount <- chartTable$tableCount
    fluidRowHeight <- 20 + chartTable$tableRowCount * 35 #px
    myTagList <- div(id = "prctTableChartDiv")
    for (i in 1:fluidRowCount) {
      myTagList <- tagAppendChild(myTagList, fluidRow(
        column(width = 6, tableOutput(paste("prctTable", i, sep = "_"))),
        column(width = 4, plotOutput(paste("prctChart", i, sep = "_"), height = fluidRowHeight))
      ))
    }
  
    # Insert the dynamically created UI component
    insertUI(selector = "#prctRows", ui = myTagList)
    prctStatus$UICreated <- TRUE
    
    # the components are rendered.  
    # Note the use of the local() function, which I barely understand having discovered it in other code.
    # It is critical to making all this work.
    for (i in 1:fluidRowCount) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        tablename <- paste("prctTable", my_i, sep = "_")
        output[[tablename]] <- function() { chartTable$displayObjects[[my_i]]$tables }
        
        chartname <- paste("prctChart", my_i, sep="_")
        output[[chartname]] <- renderPlot({ chartTable$displayObjects[[my_i]]$charts })
      })
    }
    return(chartTable)
  })
  
  output$prctCaption <- renderUI(prctChartTable()$caption)
  
  # react to save table button
  output$prctSaveReport <- downloadHandler(
    filename = paste0("DetailProcComp-", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(filename) {
      write.csv(prctChartTable()$exportData, filename, na = "")
    }
  )
  
  # Processor Product report ------------------------------------------------------------------------------------
  pdReport <- eventReactive(input$pdRunReport, {
    source("R/ProcessorDetailReport.R")
    GetProcessorDetailReport(
      processorSelection = input$pdProcessorSelection,
      nodeSelection = input$pdHierarchySelection, 
      level = input$pdLevelSelection,
      geoSelection = input$pdGeoSelection,
      timeSelection = input$pdTimeSelection)
  })
  output$pdReport <- DT::renderDataTable({pdReport()})
  
  # Summary Processor Comparison report ------------------------------------------------------------------------------------
  # reactively update the UserInterface - Processor comparison charts
  observe({
    # Node list depends on product hierarchy selection
    # 2018-08-23 (MRB) changed LEAF from FALSE to TRUE
    nodeList <- NodeSelectionList(input$pcHierarchySelection, skip = FALSE, leaf = TRUE)
    updateSelectizeInput(session, "pcNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })  
  
  pcReport <- eventReactive(input$pcRunReport, {
    req(input$pcNodeSelection)  # don't do anything until node selection is ready
    source("R/ProcessorComparisonReport.R")
    GetProcessorComparisonReport( 
      nodeID = input$pcNodeSelection,
      geoSelection = input$pcGeoSelection,
      timeSelection = input$pcTimeSelection,
      activeOnly = input$pcActiveOnly)
  })
  output$pcReport <- DT::renderDataTable(pcReport())
  
  # Download button added by Alivingstone
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('SumProcComp-', Sys.Date(), '.csv', sep='')
    },
    content = function(filename) {
      write.csv(detailReport, filename)
    }
  )
  
  # Detail Processor Comparison report and chart --------------------------------------------------------
  observe({
    # Node list depends on product hierarchy selection
    # 2018-08-23 (MRB) changed leaf = from TRUE to FALSE
    nodeList <- NodeSelectionList(input$pccHierarchySelection, skip = FALSE, leaf = FALSE)
    updateSelectizeInput(session, "pccNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  
  pccStatus <- reactiveValues()
  pccChartTable <- eventReactive(input$pccRunReport, {
    req(input$pccNodeSelection)
    req(input$pccProgramIDs)

    # if this report has already been run, remove the existing dynamic UI component
    if (!is.null(pccStatus$UICreated)) {
      removeUI(selector = "#pccTableChartDiv")
      pccStatus$UICreated <- NULL
    }
    
    source("R/ProcessorComparisonChartTable.R")
    chartTable <- GetPccChartTable(nodeID = input$pccNodeSelection,
                                   processorParms = list(selectionType = input$pccProcessorSelectionType,
                                                         locationIDs = input$pccProcessorList),
                                   programList = input$pccProgramIDs,
                                   statisticID = input$pccStatisticSelection,
                                   variableName = input$pccVariableSelection,
                                   timeParms = list(timeUnit = input$pccTimeUnitSelection,
                                                    startQuarterID = input$pccQtrStart,
                                                    quarterCount = input$pccQtrCount,
                                                    startHalfID = input$pccHalfStart,
                                                    halfCount = input$pccHalfCount,
                                                    yearRange = input$pccYrRange))
    fluidRowCount <- chartTable$tableCount
    fluidRowHeight <- 20 + chartTable$tableRowCount * 35 #px
    myTagList <- div(id = "pccTableChartDiv")
    for (i in 1:fluidRowCount) {
      myTagList <- tagAppendChild(myTagList, fluidRow(
        column(width = 6, tableOutput(paste("pccTable", i, sep = "_"))),
        column(width = 4, plotOutput(paste("pccChart", i, sep = "_"), height = fluidRowHeight))
      ))
    }
  
    # Insert the dynamically created UI component
    insertUI(selector = "#pccRows", ui = myTagList)
    pccStatus$UICreated <- TRUE
    
    # populate the dynamic components
    for (i in 1:fluidRowCount) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        tablename <- paste("pccTable", my_i, sep = "_")
        output[[tablename]] <- function() {chartTable$displayObjects[[my_i]]$table}
        chartname <- paste("pccChart", my_i, sep="_")
        output[[chartname]] <- renderPlot(chartTable$displayObjects[[my_i]]$chart)
      })
    }
    return(chartTable)
  })
  
  output$pccCaption <- renderUI(pccChartTable()$caption)
  
  # react to save table button
  output$pccSaveReport <- downloadHandler(
    filename = paste0("ProcComp-", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(filename) {
      write.csv(pccChartTable()$exportData, filename, na = "")
    }
  )
  # Container Analysis report and chart --------------------------------------------------------
  caStatus <- reactiveValues()
  caChartTable <- eventReactive(input$caRunReport, {
    
    # if this report has already been run, remove the existing dynamic UI component
    if (!is.null(caStatus$UICreated)) {
      removeUI(selector = "#caTableChartDiv")
      caStatus$UICreated <- NULL
    }
    
    # get the data to be displayed
    source("R/ContainerAnalysis.R")
    chartTable <- GetCaChartTable(organizeBy = input$caOrganizationType,
                                  programList = input$caProgramIDs,
                                  includeSummary = input$caIncludeSummary,
                                  timeParms = list(timeUnit = input$caTimeUnitSelection,
                                                   startQuarterID = input$caQtrStart,
                                                   quarterCount = input$caQtrCount,
                                                   startHalfID = input$caHalfStart,
                                                   halfCount = input$caHalfCount,
                                                   yearRange = input$caYrRange),
                                  variable = input$caVariableSelection)
    
    # create the dynamic UI component which is a div containing a fluid row for each 
    # program or container type (depending on organizeBy setting)
    # each row contains two columns. The first with a table and the second with a chart.
    fluidRowCount <- chartTable$tableCount
    # the parameter below tries to size the row height according to the number of rows in the
    # table.  The fixed and variable components are set by trial and error
    fluidRowHeight <- 30 + chartTable$tableRowCount * 25 #px
    myTagList <- div(id = "caTableChartDiv")
    for (i in 1:fluidRowCount) {
      myTagList <- tagAppendChild(myTagList, fluidRow(
        column(width = 6, tableOutput(paste("caTable", i, sep = "_"))),  
        column(width = 4, plotOutput(paste("caChart", i, sep = "_"), height = fluidRowHeight))
      ))
    }
    
    # Insert the dynamically created UI component
    insertUI(selector = "#caRows", ui = myTagList)
    caStatus$UICreated <- TRUE
    
    # populate the dynamic components
    for (i in 1:fluidRowCount) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        tablename <- paste("caTable", my_i, sep = "_")
        output[[tablename]] <- function() {chartTable$displayObjects[[my_i]]$table}
        chartname <- paste("caChart", my_i, sep="_")
        output[[chartname]] <- renderPlot(chartTable$displayObjects[[my_i]]$chart)
      })
    }
    return(chartTable)
  })
  
  output$caCaption <- renderUI(caChartTable()$caption)
  
  # react to save table button
  output$caSaveReport <- downloadHandler(
      filename = paste0("ContainerAnalysis ", format(Sys.Date(), "%Y%m%d"), ".csv"),
      content = function(filename) {
        write.csv(caChartTable()$exportData, filename, na = "")
      }
    )

  # Brand Analysis report and chart --------------------------------------------------------
  observe({
    nodeList <- NodeSelectionList(input$barHierarchySelection, skip = FALSE, leaf = TRUE)
    updateSelectizeInput(session, "barNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  
  barStatus <- reactiveValues()
  barChartTable <- eventReactive(input$barRunReport, {
    req(input$barNodeSelection)
    req(input$barProgramIDs)
    
    # if this report has already been run, remove the existing dynamic UI component
    if (!is.null(barStatus$UICreated)) {
      removeUI(selector = "#barTableChartDiv")
      barStatus$UICreated <- NULL
    }
    
    source("R/BrandAnalysis.R")
    chartTable <- GetbarChartTable(nodeID = input$barNodeSelection,
                                   brandParms = list(selectionType = input$barBrandSelectionType,
                                                     brandIDs = input$barBrandList,
                                                     maxCount = input$barMaxCount,
                                                     summarization = input$barSummarizationType),
                                   programList = input$barProgramIDs,
                                   includeSummary = input$barIncludeSummary,
                                   statisticID = input$barStatisticSelection,
                                   variableName = input$barVariableSelection,
                                   timeParms = list(timeUnit = input$barTimeUnitSelection,
                                                    startQuarterID = input$barQtrStart,
                                                    quarterCount = input$barQtrCount,
                                                    startHalfID = input$barHalfStart,
                                                    halfCount = input$barHalfCount,
                                                    yearRange = input$barYrRange))
    req(chartTable)  # must not be NULL
    fluidRowCount <- chartTable$tableCount
    fluidRowHeight <- 20 + chartTable$tableRowCount * 35 #px
    myTagList <- div(id = "barTableChartDiv")
    for (i in 1:fluidRowCount) {
      myTagList <- tagAppendChild(myTagList, fluidRow(
        column(width = 6, tableOutput(paste("barTable", i, sep = "_"))),
        column(width = 4, plotOutput(paste("barChart", i, sep = "_"), height = fluidRowHeight))
      ))
    }
    
    # Insert the dynamically created UI component
    insertUI(selector = "#barRows", ui = myTagList)
    barStatus$UICreated <- TRUE
    
    # populate the dynamic components
    for (i in 1:fluidRowCount) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        tablename <- paste("barTable", my_i, sep = "_")
        output[[tablename]] <- function() {chartTable$displayObjects[[my_i]]$table}
        chartname <- paste("barChart", my_i, sep="_")
        output[[chartname]] <- renderPlot(chartTable$displayObjects[[my_i]]$chart)
      })
    }
    return(chartTable)
  })
  
  output$barCaption <- renderUI(barChartTable()$caption)
  
  # react to save table button
  output$barSaveReport <- downloadHandler(
    filename = paste0("BrandAnalysis-", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(filename) {
      write.csv(barChartTable()$exportData, filename, na = "")
    }
  )
  
  # Time series reports ------------------------------------------------------------------------------------
  # reactively update the UserInterface - Time Series
  observe({
    # Node list depends on product hierarchy
    nodeList <- NodeSelectionList(input$tsHierarchySelection, skip = FALSE, leaf = FALSE)
    updateSelectizeInput(session, "tsNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  
  tsTimeParms <- reactive({
    if (input$tsTimeUnitSelection == 0){
      timeParms <- list(timeUnit = 0,
                        startQuarterID = input$tsQtrStart,
                        quarterCount = input$tsQtrCount)
    } else if (input$tsTimeUnitSelection == 1) {
      timeParms <- list(timeUnit = 1, 
                        yearRange = input$tsYrRange)
    } else {
      timeParms <- NULL
    }
    return(timeParms)
  })
  
  # react to Run Report  
  tsPlot <- eventReactive(input$tsRunReport, {
    req(input$acNodeSelection) # do nothing until node selection is ready
    source("R/BasicCharts.R")
    tsplot <- tsLinePlot(nodeID = input$tsNodeSelection,
                         timeParms = tsTimeParms(),
                         weighting = input$tsWeightingSelection,
                         statisticID = input$tsStatisticSelection,
                         variableName = input$tsVariableSelection)
    return(tsplot)
  })
  
  output$tsPlot <- renderPlot({tsPlot()})
  
  # Area chart reports ------------------------------------------------------------------------------------
  
  # reactively update the UserInterface - Area charts
  observe({
    # Node list depends on product hierarchy selection
    acNodeList <- NodeSelectionList(input$acHierarchySelection, skip = FALSE, leaf = FALSE)
    updateSelectizeInput(session, "acNodeSelection", choices = acNodeList, options = list(maxItems = 1))
  })
  
  acTimeParms <- reactive({
    if (input$acTimeUnitSelection == 0){
      timeParms <- list(timeUnit = 0,
                        startQuarterID = input$acQtrStart,
                        quarterCount = input$acQtrCount)
    } else if (input$acTimeUnitSelection == 1) {
      timeParms <- list(timeUnit = 1, 
                        yearRange = input$acYrRange)
    } else {
      timeParms <- NULL
    }
    return(timeParms)
  })
  
  # react to Run Report  
  acPlot <- eventReactive(input$acRunReport, {
    req(input$acNodeSelection) # do nothing until node selection is ready
    source("R/BasicCharts.R")
    acAreaPlot(nodeID = input$acNodeSelection, 
               timeParms = acTimeParms(), 
               weighting = input$acWeightingSelection)
  })
  output$acPlot <- renderPlot({acPlot()})
  
    
  # Weight ratio by program ------------------------------------------------------------------------------------
  
  # When a new hierarchy is selected, change the list of available child nodes
  observe({
    wrpNodeList <- NodeSelectionList(input$wrpHierarchySelection, skip = FALSE, leaf = FALSE)
    updateSelectizeInput(session, "wrpNodeSelection", choices = wrpNodeList, options = list(maxItems = 1))
  })
  
  # react to button press to create the table and charts
  wrpTableCharts <- eventReactive(input$wrpRunReport, {
    # we need a node selection
    req(input$wrpNodeSelection)
    
    # we need at least one program
    req(input$wrpProgramIDs)
    
    # create the time parameter list
    wrpTimeParms <- list(timeUnit = input$wrpTimeUnitSelection,
                         startQuarterID = input$wrpQtrStart,
                         quarterCount = input$wrpQtrCount,
                         yearRange = input$wrpYrRange)
    
    # get a list of tables and plots
    source("R/WeightRatioChart.R")
    wrpList <- GetWrpAreaChart(
      nodeID = input$wrpNodeSelection,
      programList = input$wrpProgramIDs,
      timeParms = wrpTimeParms,
      weighting = input$wrpWeightingSelection,
      includeSummary = input$wrpIncludeSummary)
    return(wrpList)
  })
  
  output$wrpTable  <- reactive({
    req(input$wrpQtrCount)
    require(knitr)
    require(kableExtra)
    table <- wrpTableCharts()$table
    table$tableData %>% 
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = c("condensed", 
                                          "striped"),
                    font_size = 9) %>%
      column_spec(1, width = "20em") %>% 
      group_rows(index = table$rowGroups) %>%
      scroll_box()
  })
  
  # as best we can, match the height of the Charts column with that of the table column
  wrpPlotHeight <- function() {
    table <- wrpTableCharts()$table
    return(length(table$rowGroups) * 26 + nrow(table$tableData) * 24.5)
  }
  
  output$wrpCharts <- renderPlot(
    wrpTableCharts()[["charts"]],
    height = wrpPlotHeight
  )
  
  # react to save table button
  output$wrpSaveReport <- downloadHandler(
    filename = paste0("PctWt-", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(filename) {
      write.csv(wrpTableCharts()$table$rawData, filename, na = "")
    }
  )
  
  # Management reports --------------------------------------------------------------------
  htmlFile <- reactive({
    # set up progress reporting 
    progress <- shiny::Progress$new()
    progress$set(message = "Preparing ESA Management Report", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    # Create a closure to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/4 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 4
      }
      progress$set(value = value, detail = detail)
    }
    
    # Set up parameters to pass to Rmd document
    params <- list(time = input$pdfTimeSelection,
                   weighting = input$pdfWeightingSelection,
                   rootdir = normalizePath("."),
                   progress = updateProgress
    )

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmdFile <- "markdown/ManagementReport_001.Rmd"
    htmlFile <- GetTempFile("ManagementReport_001", ".html")
    rmarkdown::render(rmdFile, output_file = htmlFile,
                      output_format = "html_document",
                      output_dir = "tmp",
                      params = params,
                      clean = TRUE,
                      envir = new.env(parent = globalenv())
    )
    
    # return the file name
    return(htmlFile)
  })

  output$pdfGenerateReport <- downloadHandler(
    filename = "ESA Management Report.html",
    content =  function(file) {
      file.copy(htmlFile(), file)
    }
  )
  
  xlFilename <- reactive({
    # set up progress reporting 
    progress <- shiny::Progress$new()
    progress$set(message = "Preparing Excel workbook", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a closure to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # create the time parameter list
    xlTimeParms <- list(timeUnit = 0, # Quarterly units
                        startQuarterID = input$xlQtrStart,
                        quarterCount = input$xlQtrCount)
    source("R/ExcelExport_001.R")
    xlFilename <- GetTempFile("ExcelReport_001", ".xlsx")
    CreateExcelReport(filename = xlFilename,
                      timeParms = xlTimeParms,
                      weighting = input$xlWeightingSelection,
                      progress = updateProgress)
    return(xlFilename)    
  })
  
  output$xlGenerateReport <- downloadHandler(
    filename = "ESA Management Report.xlsx",
    content = function(file) {
      file.copy(xlFilename(), file)
    }
  )
  
  # Single stratum math check ---------------------------------------------------------------------------------- 
  
  # reactively update the UserInterface - Single Stratum  charts
  observe({
    # Node list depends on product hierarchy selection
    nodeList <- NodeSelectionList(input$ssHierarchySelection, skip = FALSE, leaf = TRUE)
    updateSelectizeInput(session, "ssNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  
  # I tried the code below, but it really uses up memory!!!!
  # # load the large nodeObs table only once when the tab is selected
  # ssNodeObs <- reactive({
  #   dummy <- input$ssMatchCheck
  #   return(GetDataObject("nodeObs"))
  # })
  
  ssDataTable <- eventReactive(input$ssRunReport, {
    req(input$ssNodeSelection)
    source("R/SamplingMathSingleStratumReport.R")
    reports <- GetSingleStratumReports(nodeSelection = input$ssNodeSelection,
                                       stratumSelection = input$ssStratumSelection,
                                       statSelection = input$ssStatisticSelection)
    return(reports)
  })
  
  ssContentDescription <- eventReactive(input$ssRunReport, {
    stratumDesc <- GetDataObject("stratum")[StratumID == input$ssStratumSelection, 
                                            paste0(ProgramShortname, " ",
                                                   SubprogramShortname, " - ",
                                                   PeriodName)]
    statDesc = GetDataObject("statistic")[StatisticID == input$ssStatisticSelection, NameE]
    nodeDesc = GetDataObject("node")[NodeID == input$ssNodeSelection, NodeName]
    desc <- paste(stratumDesc, nodeDesc, statDesc, sep = " - ")
  })
  
  output$ssContentDescription <- renderText(ssContentDescription())
  output$ssSummaryReport <- DT::renderDataTable(ssDataTable()$summaryTable)
  output$ssDetailReport <- DT::renderDataTable(ssDataTable()$detailTable)
  
  # Multi stratum math check ----------------------------------------------------------------
  observe({
    # Node list depends on product hierarchy selection
    nodeList <- NodeSelectionList(input$msHierarchySelection, skip = FALSE, leaf = TRUE)
    updateSelectizeInput(session, "msNodeSelection", choices = nodeList, options = list(maxItems = 1))
  })
  
  msStrataSelection <- eventReactive(input$msRunReport, {
    s <- switch(as.character(input$msStratumSelectionType),
                "0" = StratumSelection(input$msGeoSelection, input$msTimeSelection),
                "1" = list(desc = "Custom strata", strataID = input$msStratumSelection), 
                stop("invalid msStratumSelection type") # obviously this should never happen!
    )
    return(s)
  })
  
  msReportDescription <- eventReactive(input$msRunReport, {
    statDesc <- GetDataObject("statistic")[StatisticID == input$msStatisticSelection, NameE]
    nodeDesc <- GetDataObject("node")[NodeID == input$msNodeSelection, NodeName]
    desc <- paste(nodeDesc, statDesc, sep = " - ")
  })
  
  msDataTable <- eventReactive(input$msRunReport, {
    req(input$msNodeSelection)
    source("R/SamplingMathMultiStratumReport.R")
    GetMultiStratumReports(nodeSelection = input$msNodeSelection,
                           strataSelection = msStrataSelection()$strataID,
                           statSelection = input$msStatisticSelection,
                           weightingSelection = input$msWeightingSelection)
  })
  
  msVarianceFormula <- eventReactive(input$msRunReport, {
    list( src = "graphics/msVarianceFormula.png", 
          alt = "Multi Stratum Variance Formula",
          height = 60)
  })
  
  output$msContentDescription <- renderText(paste0(msStrataSelection()$desc,
                                                   " - ",
                                                   msReportDescription()))
  
  output$msVarianceFormula <- renderImage(msVarianceFormula(),deleteFile = FALSE)
  output$msSummaryReport <- DT::renderDataTable(msDataTable()$summaryTable)
  output$msDetailReport <- DT::renderDataTable(msDataTable()$detailTable)


  
  # Refresh ESA Data ----------------------------------------------------------
  GetEffectiveDate <- function() {
    lastLoadInfo <- GetDataLoadTiming()
    if (lastLoadInfo$consistencyFlag) {
      return(format(lastLoadInfo$officialTimeStamp))
    } else {
      return("Refresh failed")
    }
  }
  
  GetRefreshDuration <- function() {
    lastLoadInfo <- GetDataLoadTiming()
    if (lastLoadInfo$consistencyFlag) {
      return(format(lastLoadInfo$seconds))
    } else {
      return("NA")
    }
  }
  
  updatedRefreshData <- eventReactive(input$utilRefreshData, {

    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Refreshing data", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    updateProgress <- function(value, detail) {
      progress$set(value = value, detail = detail)
    }
    
    BuildStats(progress = updateProgress)
    list(effDate = GetEffectiveDate(), duration = GetRefreshDuration())
  })
  
  output$utilRefreshCurrentEffectiveDate <- renderText(GetEffectiveDate())
  output$utilRefreshLastDuration <- renderText(GetRefreshDuration())
  
  output$utilRefreshEffectiveDate <- renderText(updatedRefreshData()$effDate)
  output$utilRefreshDuration <- renderText(updatedRefreshData()$duration)
  
})

