# WeightRatioChart.R
#
# Michael Bleau
# 2016-04-05
#
# Functions to generate Weight Percent Tables and Charts for Display in Shiny
# 
# Charts use the ggplot2 package - and can be viewed using the plot viewer or Shiny
# 
# Parameters: Node (within the hierarchy), PeriodType (Quarter or Year), PeriodStart, PeriodList and weighting type, statistic and variable
# 

#' @section Test

wrpTest <- function(){
  nodeID <- 1000
  programList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8")
  # programList <- c("P7", "P8")
  includeSummary = TRUE
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 12
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    yearRange = yearValues)
  statID <- 1  #wt ratio  #change 
  plt <- GetWrpAreaChart(nodeID, 
                         programList, 
                         timeParms, 
                         "forecast",
                         includeSummary)
  return(plt)
}

#' @section main
source("R/UtilityFunctions.R")


#' @name GetWrpAreaChart
#' 
#' @desc Generate area charts and data tables.  The charts show time on the x axis and PercentWeight on the y axis.
#'       There is a chart for each requested program and, optionally, a summary chart.
#'       There are 2 data tables: one for display along side the charts and another (containing the same data but designed for download)
#' 
#' @param 
#' nodeID:          the ID of the parent node.  The area plot will be for the nodes 1 level below
#' programList:     a list of the program ID's to include (each with a P prefix)
#' timeParms:       a list of parameters describing the time periods and units to cover
#' weighting:       Forecast, Actual or Unweighted
#' includeSummary:  if TRUE include a summary table as the first set of results
#' 
#' @return a two object list of objects for shiny to display. 
#'         dataTable         a list of three objects:
#'               tableData   a data table to be used in a Knitr::kable HTML table
#'               rowGroups   a list describing how the dataTable rows are to be grouped into programs.
#'                           the list is designed to be used by the group_rows parameter of the
#'                           kable_styling function
#'               rawData     a table suitable for downloading as a csv file
#'         chart             a multifaceted area chart with one facet per program
#'         
#' 

GetWrpAreaChart <- function(nodeID, programList, timeParms, weighting, includeSummary ) {
  
  #' @name GetPeriods
  #' 
  #' @desc Utility function to convert shiny date parameters into a
  #'       time period information 
  #' 
  #' @return a three item list
  #'  periodTable   table of quarters. 
  #'                PeriodID - the value on which periods should be grouped
  #'                QuarterID - the quarters that belong to each group
  #'  periodLevels  a sorted list of levels for the PeriodID field
  #'  periodLabels  a list of labels to assign to those levels
  #'
  #' @param a timeparms obejct with the following elements:
  #'   unitType:       0 for quarters and 1 for years
  #'   startQuarterID: ID of the first quarter to display (ignored if timeType = 1)
  #'   quarterCount:   number of quarters to display (ignored if timeType = 1)
  #'   yearValues:     2 element vector of first and last year values (not the IDs)
  #'                   to display (only used for timeType = 1)
  
  GetPeriods <- function(timeParms) {
    q <- GetDataObject("quarter")
    setkey(q, PeriodStartDate)
    if (timeParms$timeUnit == 0) {
      firstDate <- q[QuarterID == timeParms$startQuarterID, PeriodStartDate]
      q <- q[PeriodStartDate >= firstDate]
      qTable <- q[1:timeParms$quarterCount, .(PeriodID = QuarterID,
                                              PeriodName,
                                              PeriodStartDate, 
                                              QuarterID)]
      periodLevels <- qTable[order(PeriodStartDate), PeriodID]
      periodLabels <- qTable[order(PeriodStartDate), PeriodName]
      
    } 
    
    else if (timeParms$timeUnit == 1) {
      years <- timeParms$yearRange[1]:timeParms$yearRange[2]
      qTable <- q[Year %in% years, .(PeriodID = Year,
                                     QuarterID)]
      periodLevels <- sort(years)
      periodLabels <- sort(years)
      
    }
    
    return(list(periodTable = qTable[ , .(PeriodID, QuarterID)], periodLevels = periodLevels, periodLabels = periodLabels))
  }

  #' @name GetPrograms
  #' 
  #' @desc Utility function to convert shiny program parameters into a
  #'       program/subprogram information 
  #' 
  #' @return a two item list
  #'  programTable   table of quarters. 
  #'                 ProgramID - the value on which subprograms should be grouped
  #'                 SubprogramID - the subprograms that belong to the group
  #'  programLevels  a sorted list of labels for the ProgramID field
  #'  programLabels  a list of labels to assign to those levels
  #'
  #' @param 
  #'   programIDs      a list of programIDs with a "P" prefix (this is how they come from the UI)
  #'   includeSummary  if TRUE, a summary level program covering all selected programs is provided

  GetPrograms <- function(programIDs, includeSummary) {
    # strip prefix from program IDs
    programIDs <- sapply(programIDs, function(x) as.integer(substr(x, 2, 999)))
    
    programs <- GetDataObject("subprogram")[ProgramID %in% programIDs, .(ProgramID, ProgHdgE, ProgSortOrder, SubprogramID)]
    programLevels <- unique(programs[order(ProgSortOrder), ProgramID])
    programLabels <- unique(programs[order(ProgSortOrder), ProgHdgE])
    programs <- programs[, .(ProgramID, SubprogramID)]
    if (includeSummary) {
      programs <- rbind(programs[, .(ProgramID = 0, SubprogramID)], programs)
      programLevels <- c(0, programLevels)
      programLabels <- c("Summary", programLabels)
    }
    return(list(programTable = programs, programLevels = programLevels, programLabels = programLabels))
  }
  
  #' @name GetNodes
  #' 
  #' @desc Utility function to convert shiny program parameters into a
  #'       node information 
  #' 
  #' @return a two item list
  #'  nodeTable      list of NodeIDs (children of the parentNode)
  #'  nodeLevels     a sorted list of labels for the ProgramID field
  #'  nodeLabels     a list of labels to assign to those levels
  #'
  #' @param 
  #'   parentNodeID:       the ID of the parent node

  GetNodes <- function(parentNodeID) {
    source("R/ShinyHelperFunctions.R")
    nodes <- NodeSelection(nodeID, depth = 1, skip = TRUE)
    nodeLevels <- nodes[order(sortOrder), NodeID]
    nodeLabels <- nodes[order(sortOrder), shortname]
    return(list(nodeList = nodes[,NodeID], nodeLevels = nodeLevels, nodeLabels = nodeLabels))
  }
  
  
  #' @name MultiStrataCategories
  #' 
  #' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
  #' 
  #' @param 
  #' periods:          period table as prodiced by the GetPeriod function
  #' programIDs:       program table as prodiced by the GetProgram function
  #' 
  #' @details 
  #' DataTable columns: MultiStratumHash, ProgramID, PeriodID, SubprogramID, QuarterID
  #' 
  #' @description 
  #' Yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R).
  #' A multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
  #' a multi stratum contains statistics for a given combination of strata
  #' this function creates a data structure which defines multistrata
  #' for each program in the database and each time unit in the datevectorlist parameter plus
  #' If includeSummary = TRUE an additional multistratum for all selected programs and each time unit in the datevector list
  #' time period numbers assume time in ascending order
  #' program numbers start with Canada and then order programs according to the program SortOrder paramter
  #'  
  MultiStrataCategories <- function(periods, programs){
    
    # combine periods and programs (cartesian join)
    qp <- Cartesian.dt(periods$periodTable, programs$programTable)
    
    # Get strata
    strata <- GetDataObject("stratum")[, .(StratumID, SubprogramID, QuarterID)]
  
    # merge in quarters
    ms <- merge(strata, qp, by = c("SubprogramID", "QuarterID"))
    
    # Create hash
    ms[ , MultiStratumHash := paste0(PeriodID, "|", ProgramID)]
    

    return(ms)
  }
  
  #' @name ReportData
  #' 
  #' @return a table of values to plot - the data is in the format most conveniently returned by MultiStratumStats
  #' 
  #' @param 
  #' nodes:                  a table of nodes to plot (requires a NodeID column)
  #' multiStratumCategories: the table of subprograms and quarters 
  #' statisticID:            a list of statisticIDs
  #' weightingType:          "Forecast", "Actual" or "Unweighted"
  #' 
  ReportData <- function(nodes, multiStratumCategories, statisticID, weightingType){
    
    # get the multistratum statistics
    source("R/PrepareMultistratStats.R")
    reportDataTable <- MultiStratumTable(nodes$nodeList, multiStratumCategories, as.list(statisticID), weightingType)
    
    # create hashTable, merge and lose the hash column
    hashTable <- unique(multiStratumCategories[,.(MultiStratumHash, ProgramID, PeriodID)])
    reportDataTable <- merge(reportDataTable, hashTable, by = "MultiStratumHash", all.y = TRUE, allow.cartesian = TRUE)
    reportDataTable[, MultiStratumHash := NULL]
    
    # rename the mean column (kludgy)
    setnames(reportDataTable, "mean", "WeightPercent")
    
    return(reportDataTable)
  }
  
  #' @name ApplyLevels
  #' 
  #' @return A table of with the various ID fields converted to sorted factors
  #' 
  #' @param 
  #' reportData:       the original table
  #' periods:          as returned by the GetPeriods function (includes levels list)
  #' programs:         as returned by the GetPrograms function (includes levels list)
  #' nodes:            as returned by the GetNodes function (includes levels list)
  #' 
  #' @details 
  #' converting ID fields to factors allows table and charting functions to present with their names
  #' and in the appropriate sequence
  #'  
  ApplyLevels <- function(reportData, periods, programs, nodes) {
    
    # convert ID field to factors
    reportData$PeriodID <- factor(reportData$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
    reportData$ProgramID <- factor(reportData$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
    reportData$NodeID <-  factor(reportData$NodeID, levels = nodes$nodeLevels, labels = nodes$nodeLabels)
    return(reportData)
  }
  
  
  #' @name AreaPlotBuild
  #' 
  #' @return Multi faceted ggplot
  #' 
  #' @param data table to plot.  Must include the following columns: ProgramID, NodeID, TimePeriodNumber, WeightPercent
  #' 
  AreaPlotBuild <- function(data) {
    require(ggplot2)
    plotData <- data
    p <- ggplot(data = plotData, aes(x = PeriodID, y = WeightPercent, fill = NodeID, group = NodeID)) + 
      geom_area() + 
      scale_fill_brewer(name = "Product", 
                        palette = "Set2") + 
      scale_y_continuous(labels = scales::percent) + 
      xlab("Time Period") + 
      ylab("") + 
      facet_grid(ProgramID ~ .) + 
      theme(legend.position = "bottom")
    return(p)
  }
  
  #' @name DataTableBuild
  #' 
  #' @return list of three data tables:
  #'               tableData   a DT data table to be used in a Knitr::kable HTML table
  #'               rowGroups   a list describing how the dataTable rows are to be grouped into programs.
  #'                           the list is designed to be used by the group_rows parameter of the
  #'                           kable_styling function
  #'               rawData     a DT table suitable for downloading as a csv file
  #'
  #' @param data table to plot.  Must include the following columns: ProgramID, NodeID, PeriodID, WeightPercent
  #' 
  DataTableBuild <- function(data) {
    rawData <- dcast(data, ProgramID + NodeID ~ PeriodID, value.var = "WeightPercent")
    programRowCount <- rawData[, .N, by = ProgramID]
    rowGroups <- programRowCount[,N]
    names(rowGroups) <- programRowCount[,ProgramID]
    tableData <- copy(rawData)[, ProgramID := NULL]    # if not copy, then we we are just creating a reference
    
    return(list(tableData = tableData,
                rowGroups = rowGroups,
                rawData = rawData) 
           )
  }
  
  # main entry point -----------------------------------------------------------
  
  # print("You called master ?")
  
  # statID and depth could, in theory, be changed to other values - this is not tested
  statIDList = as.list(1)  # weight percent
  periods <- GetPeriods(timeParms)
  programs <- GetPrograms(programList, includeSummary)
  nodes <- GetNodes(nodeID)
  msl <- MultiStrataCategories(periods, programs)
  reportData <- ReportData(nodes, msl, statIDList, weighting)
  levelledReportData <- ApplyLevels(reportData, periods, programs, nodes)
  wrpData <- list(charts = AreaPlotBuild(levelledReportData),
                     table = DataTableBuild(levelledReportData))
  return(wrpData)
}