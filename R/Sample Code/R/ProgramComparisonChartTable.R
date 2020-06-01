# ProgramComparisonChartTable.R
#
# Michael Bleau
# 2018-04-23
#
# Functions to generate Program Comparison charts and tables for display in Shiny
# 
# 2018-08-23 (MRB)
# minor fixes to prevent NaN values to cause crashes
# replace 'digit' with 'accuracy' parameter when using scales library

#' @section Test

prctTest <- function(){
  nodeID <- 1000
  programList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
  # programList <- c("P9")
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 8
  halfStartID = "2015-H1"
  halfCount = 8
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    startHalfID = halfStartID,
                    halfCount = halfCount,
                    yearRange = yearValues)
  statID <- 1  #wt ratio  
  # statID <- 2  #wt ratio 
  variableName <- "mean"
  # variableName <- "units"
  includeSummary <- TRUE
  
  prct <- GetPrctChartTable(nodeID = nodeID, 
                            programList = programList, 
                            weighting = "forecast",
                            statisticID = statID,
                            variableName = variableName,
                            timeParms = timeParms,
                            includeSummary = includeSummary)
  return(prct)
}

#' @section main


#' @name GetPrctChartTable
#' 
#' @desc Generate area charts and data tables for display with Shiny.  
#'       Both show time on the x axis and 
#'       the selected variable/statistic on the y axis.
#'       There is a chart and a table for each requested program and,
#'       if requested, a further summary chart and table
#'        
#' 
#' @param 
#' nodeID           the ID of the parent node.  The area plot will be for the nodes 1 level below
#' programList      a list of the program ID's to include (each with a P prefix)
#' weighting        Forecast, Actual or Unweighted
#' statisticID      the ID of the statistic to use
#' variableName     the name of the variable to use
#' timeParms        a list of parameters describing the time periods and units to cover
#' includeSummary   TRUE is a summary is requested
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

GetPrctChartTable <- function(nodeID, 
                              processorParms, 
                              programList, 
                              weighting, 
                              statisticID,
                              variableName,
                              timeParms,
                              includeSummary) {
  

  
  #' @name MultiStrataCategories
  #' 
  #' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
  #' 
  #' @param 
  #' periods          period table as produced by the GetPeriod function
  #' programs         programs table as produced by the GetPrograms function
  #' 
  #' @details 
  #' DataTable columns:  MultiStratumHash, PeriodID, StratumID, QuarterID, ProgramID
  #' 
  #' @description 
  #' Yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R).
  #' A multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
  #' a multi stratum contains statistics for a given combination of strata
  #' this function creates a data structure which defines multistrata
  #' for each time unit in the datevectorlist parameter plus.  
  #' There is a multi stratum for each period program combination (that is found in the stratum table)
  #'  
  MultiStrataCategories <- function(periods, programs){
    
    # cartesian join of programs and periods
    pp <- Cartesian.dt(programs, periods)
    
    # Get strata
    strata <- GetDataObject("stratum")[,.(StratumID, SubprogramID, QuarterID)]
    strata <- GetDataObject("stratum")[,.(StratumID, SubprogramID, QuarterID)]
    
    # merge in program periods
    ms <- merge(strata, pp, by = c("SubprogramID", "QuarterID"))
    
    # Create hash
    ms[ , MultiStratumHash := paste(ProgramID, PeriodID, sep = "|")]
    
    return(ms)
  }
  
  #' @name ReportData
  #' 
  #' @return a table of values to plot with columns: NodeID, SamplingLocationID, PeriodID and reportVariable
  #' 
  #' @param 
  #' nodes                  a table of nodes to plot (requires a NodeID column)
  #' multiStratumCategories the table of subprograms and quarters 
  #' statisticID            the statisticID to select
  #' variableName           the variable to retain
  #' weightingType          "forecast", "actual" or "unweighted"
  #' 
  #' @details 
  #' A column named "reportVariable" is created.  It is a copy of the column specified by the variableName parameter.
  #' 
  #' 
  ReportData <- function(nodes, multiStratumCategories, statisticID, variableName, weightingType){
    
    # get the multistratum statistics
    source("R/PrepareMultistratStats.R")
    reportDataTable <- MultiStratumTable(nodes$nodeList, 
                                         multiStratumCategories, 
                                         as.list(statisticID), 
                                         weightingType)
    
    # create hashTable, merge and lose the hash column
    hashTable <- unique(multiStratumCategories[,.(MultiStratumHash, ProgramID, PeriodID)])
    reportDataTable <- merge(reportDataTable, hashTable, by = "MultiStratumHash")
    reportDataTable[, MultiStratumHash := NULL]
    
    # create the reportVariable column and only retain needed data
    reportDataTable[, reportVariable := get(variableName)][ , .(NodeID, ProgramID, PeriodID, reportVariable)]
    
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
  #' periods               as returned by the GetPeriodsIDLL function (includes levels list)
  #' processorsLocations   as returned by the GetProcessorLocationsIDLL function (includes levels list)
  #' nodes                 as returned by the GetNodesIDLL function (includes levels list)
  #' 
  #' @details 
  #' converting ID fields to factors allows table and charting functions to present with their names
  #' and in the appropriate sequence
  #'  
  ApplyLevels <- function(reportData, programs, periods, nodes) {
    
    # convert ID field to factors
    reportData$ProgramID <- ordered(reportData$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
    reportData$PeriodID <- ordered(reportData$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
    reportData$NodeID <-  ordered(reportData$NodeID, levels = nodes$nodeLevels, labels = nodes$nodeLabels)
    return(reportData)
  }
  
  #' @name BuildTable
  #' 
  #' @return a Shiny DT table for a single program
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
  #' @return A table for single program
  #' 
  #' @param 
  #'   data           Data table to plot.  Must include the following columns: ProgramID, NodeID, PeriodID, reportVariable.
  #'   displayParms   list of parameters for the statisticID, variableName as provided by the DTGetDisplayParameters function
  #' 
  #' @description 
  #' the chart type (area, line) is provided in displayParms
  #' 
  BuildChart <- function(data, displayParms) {
    require(ggplot2)
    
    # create plotVariable
    data[ , plotVariable := reportVariable * displayParms$ConversionFactor]
    
    # plot
    if (displayParms$ChartType == "A") {
      p <- ggplot(data = data, aes(x = PeriodID, y = plotVariable, fill = NodeID, group = NodeID)) + 
        geom_area() + 
        scale_fill_brewer(name = "Product", 
                          palette = "Set2")
    } else {
      p <- ggplot(data = data, aes(x = PeriodID, 
                                   y = plotVariable,
                                   group = NodeID, 
                                   colour = NodeID)) + 
        geom_line(size = 1.0) + 
        scale_color_brewer(name = "Product", 
                           palette = "Set2")
    }
    p <- p + 
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
  
  BuildDisplayObjects <- function(data, programLabels, statisticID, variableName) {
    displayParms <- DTGetDisplayParameters(statisticID, variableName)
    displayObjects <- lapply(programLabels, 
                             function(x) list(tables = BuildTable(data[ProgramID == x], displayParms, x),
                                              charts = BuildChart(data[ProgramID == x], displayParms))
    )
    return(displayObjects)
  }
  
  GetExportData <- function(data, displayParms) {
    data[ , exportVariable := reportVariable * displayParms$ConversionFactor]
    exportData <- dcast(data, ProgramID + NodeID ~ PeriodID, value.var = "exportVariable", drop = FALSE)
    return(exportData)
  }
  
  # main entry point -----------------------------------------------------------
  source("R/UtilityFunctions.R")
  source("R/ShinyHelperFunctions.R")
  displayParms <- DTGetDisplayParameters(statisticID, variableName)
  periods <- GetPeriodsIDLL(timeParms)
  programs <- GetProgramsIDLL(programList, includeSummary)
  nodes <- GetNodesIDLL(nodeID)

  msl <- MultiStrataCategories(periods$periodTable, programs$programTable)
  reportData <- ReportData(nodes = nodes, 
                           multiStratumCategories = msl,
                           statisticID = statisticID,
                           variableName = variableName,
                           weightingType = weighting)
  levelledReportData <- ApplyLevels(reportData, programs, periods, nodes)
  displayObjects <- BuildDisplayObjects(levelledReportData, 
                                        programs$programLabels,
                                        statisticID,
                                        variableName)
  caption <- DTGetReportCaption(mainTitle = "Program Comparison Report",
                                nodeSelection = nodeID,
                                statisticID = statisticID,
                                variableName = variableName,
                                tableCaption = FALSE)
  prctData <- list(caption = caption,
                  displayObjects = displayObjects,
                  tableCount = length(displayObjects),
                  tableRowCount = length(nodes$nodeLevels), 
                  exportData = GetExportData(levelledReportData, displayParms))
  
  return(prctData)
}