# ProcessorComparisonChartTable.R
#
# Michael Bleau
# 2018-04-12
#
# Functions to generate Processor Comparison charts and tables for display in Shiny
# 
# 2018-08-23 (MRB)
#  - minor fixes to prevent NaN values to cause crashes
#  - replace 'digit' with 'accuracy' parameter when using scales library
# 2018-09-05 (MRB)
#  - moved functions to ShinyHelperFunctions.R since they turn out to be useful elsewhere 
#    (see IDLL functions)

#' @section Test

pccTest <- function(){
  rm(list = ls())
  source("R/UtilityFunctions.R")
  nodeID <- 456500
  # programList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8")
  programList <- c("P7")
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 8
  startHalfID = "H2-2018"
  halfCount = 8
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    startHalfID = startHalfID,
                    halfCount = halfCount,
                    yearRange = yearValues)
  statID <- 1  #wt ratio  
  # statID <- 2  #wt ratio 
  variableName <- "mean"
  # variableName <- "gr"
  
  processorParms <- list()
  # processorParms$selectionType <- "all"
  # processorParms$selectionType <- "allActive"
  processorParms$selectionType <- "specific"
  processorParms$locationIDs <- c(17, 28)
  
  pcc <- GetPccChartTable(nodeID = nodeID, 
                          processorParms = processorParms,
                          programList = programList, 
                          statisticID = statID,
                          variableName = variableName,
                          timeParms = timeParms)
  return(pcc)
}

#' @section main
source("R/UtilityFunctions.R")


#' @name GetPccChartTable
#' 
#' @desc Generate area charts and data tables for display with Shiny.  
#'       Both show time on the x axis and 
#'       the selected variable/statistic on the y axis.
#'       There is a chart and a table for each requested processorLocations
#'        
#' @param 
#' nodeID           the ID of the parent node.  The area plot will be for the nodes 1 level below
#' processorParms   a two item list describing the desired processor/locations
#'       selectionType "all"       (all processorLocations),
#'                     "allActive" all processorLocations with activity for the selected programs,
#'                     "specific"  user provided list of locations
#'       locationIDs   user provided list of IDs (only used if selectionType is specific)
#' programList      a list of the program ID's to include (each with a P prefix)
#' statisticID      the ID of the statistic to use
#' variableName     the name of the variable to use
#' timeParms        a list of parameters describing the time periods and units to cover
#' 
#' @return a five item list of objects
#'         caption           for display in shiny using renderUI and uiOutput
#'         displayObjects    a two item list.
#'               tables         a vector of chart kable tables - 1 per processor
#'               charts         a vector of plots - 1 per processor
#'         tableCount        number of tables in the tables vector (same as number of charts in the chart vector)
#'         tableRowCount     number of node items in each chart
#'         exportData        a simple table combining all the data contained in the tables vector for export
#'         

GetPccChartTable <- function(nodeID, 
                             processorParms, 
                             programList, 
                             statisticID,
                             variableName,
                             timeParms ) {
  
  #' @name MultiStrataCategories
  #' 
  #' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
  #' 
  #' @param 
  #' periods          period table as produced by the GetPeriod function
  #' subprogramList   list of subprograms to include in each multistratum
  #' 
  #' @details 
  #' DataTable columns:  MultiStratumHash, PeriodID, StratumID, QuarterID
  #' 
  #' @description 
  #' Yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R).
  #' A multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
  #' a multi stratum contains statistics for a given combination of strata
  #' this function creates a data structure which defines multistrata
  #' for each time unit in the datevectorlist parameter plus.  The multiStrata only include selected subprograms
  #'  
  MultiStrataCategories <- function(periods, subprogramList){
    
    # Get strata
    strata <- GetDataObject("stratum")[SubprogramID %in% subprogramList, .(StratumID, QuarterID)]
    
    # merge in periods
    ms <- merge(strata, periods, by = "QuarterID")
    
    # Create hash
    ms[ , MultiStratumHash := PeriodID]
    
    return(ms)
  }
  
  #' @name ReportData
  #' 
  #' @return a table of values to plot with columns: NodeID, SamplingLocationID, PeriodID and reportVariable
  #' 
  #' @param 
  #' nodes                  a table of nodes to plot (requires a NodeID column)
  #' multiStratumCategories the table of subprograms and quarters 
  #' processors             the list of processors to retain
  #' activeOnly             TRUE if only data for active processors is to be retained
  #' statisticID            the statisticID to select
  #' variableName           the variable to retain
  #' 
  #' @details 
  #' A column named "reportVariable" is created.  It is a copy of the column specified by the variableName parameter.
  #' 
  #' 
  ReportData <- function(nodes, multiStratumCategories, processors, activeOnly, statisticID, variableName){
    
    # get the multistratum statistics
    source("R/PrepareMultistratStats.R")
    reportDataTable <- MultiStratumTable(nodes$nodeList, multiStratumCategories, as.list(statisticID), "unweighted",
                                         locationID = TRUE)[SamplingLocationID %in% processors]
    
    # create hashTable, merge and lose the hash column
    hashTable <- unique(multiStratumCategories[,.(MultiStratumHash, PeriodID)])
    reportDataTable <- merge(reportDataTable, hashTable, by = "MultiStratumHash")
    reportDataTable[, MultiStratumHash := NULL]
    
    # remove inactive processors
    if (activeOnly) {
      activeProcessors <- reportDataTable[ , .(TotalUnits = sum(units, na.rm = TRUE)), by = SamplingLocationID][TotalUnits > 0, SamplingLocationID]
      reportDataTable <- reportDataTable[SamplingLocationID %in% activeProcessors]
    }
    
    # create the reportVariable column and only retain needed data
    reportDataTable[, reportVariable := get(variableName)][ , .(NodeID, SamplingLocationID, PeriodID, reportVariable)]
     
    # replace NaN values in report variable with 0s (or else shiny has a hissy fit when trying to plot)
    reportDataTable[is.nan(reportVariable), reportVariable := 0]
    
    return(reportDataTable)
  }
  
  #' @name ApplyLevels
  #' 
  #' @return A table of with the various ID fields converted to sorted factors
  #' 
  #' @param 
  #' reportData            the original table
  #' periods               as returned by the GetPeriods function (includes levels list)
  #' processorsLocations   as returned by the GetProcessorLocations function (includes levels list)
  #' nodes                 as returned by the GetNodes function (includes levels list)
  #' 
  #' @details 
  #' converting ID fields to factors allows table and charting functions to present with their names
  #' and in the appropriate sequence
  #'  
  ApplyLevels <- function(reportData, periods, processors, nodes) {
    
    # convert ID field to factors
    reportData$PeriodID <- factor(reportData$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
    reportData$SamplingLocationID <- factor(reportData$SamplingLocationID, levels = processors$locationLevels, labels = processors$locationLabels)
    reportData$NodeID <-  factor(reportData$NodeID, levels = nodes$nodeLevels, labels = nodes$nodeLabels)
    return(reportData)
  }
  
  
 
  #' @name BuildTable
  #' 
  #' @return a Shiny DT table for a single processor
  #'
  #' @param 
  #'    data           table to plot.  Must include the following columns: ProgramID, NodeID, PeriodID, reportVariable
  #'    displayParms   list of parameters for the statisticID, variableName as provided by the DTGetDisplayParameters function
  #'    statisticID    factor for the statistic
  #' 
  BuildTable <- function(data, displayParms, statisticID) {
    
    # create and format tableVariable
    data[ , tableVar := switch(displayParms$FormatType, 
                               "P" = scales::percent(reportVariable * displayParms$ConversionFactor),
                               "C" = scales::comma(reportVariable * displayParms$ConversionFactor, accuracy = displayParms$Accuracy))]
    if (nrow(data) == 0 ) {return(NULL)}
    
    data <- dcast(data, NodeID ~ PeriodID, value.var = "tableVar", drop = FALSE)
    
    require(knitr)
    require(kableExtra)
    
    colNames <- c(as.character(statisticID), colnames(data)[2:ncol(data)])
    options(knitr.kable.NA = "")
    htmlTable <- data %>%
      kable(format = "html", 
            linesep = "",
            col.names = colNames,
            align = c("l", rep("r", ncol(data) - 1))) %>%
      kable_styling(bootstrap_options = c("condensed", 
                                          "striped"),
                    font_size = 10) %>%
      column_spec(1, width = "20em") %>% 
      scroll_box()
    
    return(htmlTable) 
  }
  
 
  #' @name BuildChart
  #' 
  #' @return A table for single processor
  #' 
  #' @param 
  #'   data           Data table to plot.  Must include the following columns: ProgramID, NodeID, PeriodID, reportVariable.
  #'   displayParms   list of parameters for the statisticID, variableName as provided by the DTGetDisplayParameters function
  #' 
  #' @description 
  #' the table will have one facet (graph) per processor.  For statisticID = 1 (WtPct) and variableName "mean"
  #' the chart type is determined by the DisplayParms settting
  #' 
  BuildChart <- function(data, displayParms) {
    require(ggplot2)
    
    # create plotVariable
    data[ , plotVariable := reportVariable * displayParms$ConversionFactor]
    
    # plot
    p <- ggplot(data = data, aes(x = PeriodID, 
                                 y = plotVariable,
                                 group = NodeID, 
                                 colour = NodeID)) + 
      geom_line(size = 1.0) + 
      scale_color_brewer(name = "Product", palette = "Set2") + 
      scale_y_continuous(labels = switch(displayParms$FormatType, 
                                         "P" = scales::percent,
                                         "C" = scales::comma,
                                         scales::comma)) + 
      scale_x_discrete(drop = FALSE) +
      xlab("") + 
      ylab("") + 
      theme(legend.position = "right")
    
    return(p)
  } 
  
  BuildDisplayObjects <- function(data, statisticID, variableName) {
    displayParms <- DTGetDisplayParameters(statisticID, variableName)
    processorIDs <- unique(data[ , SamplingLocationID])
    displayObjects <- lapply(processorIDs, 
                             function(x) list(table = BuildTable(data[SamplingLocationID == x], displayParms, x),
                                              chart = BuildChart(data[SamplingLocationID == x], displayParms))
    )
    return(displayObjects)
  }
  
  GetExportData <- function(data, displayParms) {
    data[ , exportVariable := reportVariable * displayParms$ConversionFactor]
    exportData <- dcast(data, SamplingLocationID + NodeID ~ PeriodID, value.var = "exportVariable", drop = FALSE)
    return(exportData)
  }
  
  # main entry point -----------------------------------------------------------
  source("R/ShinyHelperFunctions.R")
  displayParms <- DTGetDisplayParameters(statisticID, variableName)
  periods <- GetPeriodsIDLL(timeParms)
  programs <- GetProgramsIDLL(programList)
  nodes <- GetNodesIDLL(nodeID)
  processorLocations <- GetProcessorLocationsIDLL(processorParms)
  
  msl <- MultiStrataCategories(periods$periodTable, programs$programTable[ ,SubprogramID])
  reportData <- ReportData(nodes = nodes, 
                           multiStratumCategories = msl,
                           processors = processorLocations$locationList,
                           statisticID = statisticID,
                           variableName = variableName,
                           activeOnly = processorLocations$activeOnly)
  levelledReportData <- ApplyLevels(reportData, periods, processorLocations, nodes)
  displayObjects <-BuildDisplayObjects(levelledReportData, 
                                statisticID,
                                variableName)
  caption <- DTGetReportCaption(mainTitle = "Processor Comparison Report",
                                nodeSelection = nodeID,
                                statisticID = statisticID,
                                variableName = variableName,
                                weighting = "unweighted",
                                tableCaption = FALSE)
  pccData <- list(caption = caption,
                  displayObjects = displayObjects,
                  tableCount = length(displayObjects),
                  tableRowCount = length(nodes$nodeLevels), 
                  exportData = GetExportData(levelledReportData, displayParms))

  return(pccData)
}