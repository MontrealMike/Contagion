# EPRASampling
#
# Michael Bleau
# 2016-03-02
#
# Functions to generate Time Series Chart
# 
# Charts use the ggplot2 package - and can be viewed using the plot viewer or Shiny
# 
# Parameters: Node (within the hierarchy), PeriodType (Quarter or Year), PeriodStart, PeriodList and weighting type, statistic and variable
# 
# The data rows are a series of time series  - one for each program and a total
#     each program in the row consists of multiple subprograms
# the columns are time units
#     each time unit in the column may be a quarter or a year
# the values depend on the Statistic and Variable parameters
#
# 2018-03-11
#    clean up bugs related to missing dates

#' @section Setup


#'  @section Test functions
tsTest <- function(){
  nodeID <- 12000
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 9
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    yearList = yearValues)
  statID <- 1  #wt ratio change
  variableName = "mean"
  weightingType = "forecast"
  plt <- tsLinePlot(nodeID = nodeID, 
                    timeParms = timeParms, 
                    weighting = weightingType,
                    statisticID = statID,
                    variableName = variableName
  )
  return(plt)
}

acTest <- function(){
  nodeID <- 1000
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 9
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    yearRange = yearValues)
  statID <- 1  #wt ratio  #change 
  plt <- acAreaPlot(nodeID, timeParms, "forecast")
  return(plt)
}

#' @section Chart specific functions
#' these are called from the Shiny server

#' @name tsLinePlot
#' 
#' @desc return a line plot of user selected statistic and variables
#' 
#' @param 
#' nodeID:          the ID of the parent node.  The area plot will be for the nodes 1 level below
#' timeParms:       a list of 4 objects:
#'                  timeType:       0 for quarters and 1 for years
#'                  startQuarterID: ID of the first quarter to display (ignored if timeType = 1)
#'                  quarterCount:   number of quarters to display (ignored if timeType = 1)
#'                  yearRange:      2 element vector of first and last year values (not the IDs)
#'                                  to display (only used for timeType = 1)
#' weighting:       "forecast", "actual" or "unweighted"
#' StatisticID:     StatisticID (or list of StatisticID's)
#' variableName:    Name of desired variable - choices are
#'                  ("units", "datalines", "gr", "mean", "var", "stdDev", "lowerCI", "upperCI")
#' 
#' @return multi faceted ggplot (line plot) with 1 facet per program + 1 facet for all programs
#' 

tsLinePlot <- function(nodeID, timeParms, weighting, statisticID, variableName ) {
  source("R/ShinyHelperFunctions.R")
  statIDList = as.list(statisticID)
  nodeTable <- NodeSelection(nodeID, GetDataObject("node"), depth = 0, skip = FALSE)
  
  dvl <- DateVectorList(timeParms$timeUnit, 
                        timeParms$startQuarterID, 
                        timeParms$quarterCount,
                        timeParms$yearRange)
  msl <- multiStrataCategories(dvl)
  pdt <- plotData(nodeTable[,NodeID], msl, statIDList, variableName, weighting)
  ldt <- levelledPlotData(pdt, dvl, nodeTable)
  plt <- tsNodeLinePlotBuild(data = ldt, 
                             title = nodeTable[1, name], 
                             variable = variableName, 
                             statisticID = statisticID)
  return(plt)
}

#'  @name tsLinePlotBuild
#'  
#'  @return a grid of line plots
#'  
#'  @param 
#'  tSeries - levelled stat data table for a single Node 
#'            ProgramNumber, NodeID and PeriodNumber are ordered factors with the labels which will appear in the chart
#'  title - Main title

tsNodeLinePlotBuild <- function(data, title, statisticID, variable, errorbars = FALSE) {
  require(ggplot2)
  
  plotData <- data
  plotFormat <- ggplotFormatElements()[StatisticID == statisticID & Variable == variable]
  # print(plotFormat)
  setnames(plotData, variable, "rawPlotValue")
  
  # eliminate NAs
  plotData <- plotData[!is.na(rawPlotValue)]
  if(nrow(plotData) > 0) { 
    
    plotData[, PlotValue := rawPlotValue * plotFormat[,ScalingFactor]]
    p <- ggplot(data = plotData, aes(x = TimePeriodNumber, y = PlotValue, group = 1)) + 
      geom_line(size = 1.5) + 
      # scale_y_continuous(labels = scales::percent) + 
      ggtitle(paste(title, plotFormat[,Title])) + 
      scale_fill_manual(values=c("#CC6666", "#9999CC")) +
      xlab("Time Period") + 
      ylab("") + 
      facet_grid(ProgramNumber ~ .)
    
    p <- p +
      switch(plotFormat[,YScaleCode],
             "pct" = {scale_y_continuous(labels = scales::percent)},
             "comma" = {scale_y_continuous(labels = scales::comma)},
             {scale_y_continuous(labels = scales::comma)}
      )
  } else {
    p <- NULL
  }
  return(p)
}


#' @name acAreaPlot
#' 
#' @desc return an area plot of wt% by Node for each program
#' 
#' @param 
#' nodeID:          the ID of the parent node.  The area plot will be for the nodes 1 level below
#' timePeriodType:  0 for quarters, 1 for years
#' timePeriodList:  if type is 0, list of quarterIDs otherwise, list of years
#' weighting:       Forecast, Actual or Unweighted
#' 
#' @return multi faceted ggplot (area plot) with 1 facet per program + 1 facet for all programs
#' 

acAreaPlot <- function(nodeID, timeParms, weighting ) {
  # statID and depth could, in theory, be changed to other values - this is not tested
  statIDList = as.list(1)  # weight percent
  depth = 1

  source("R/ShinyHelperFunctions.R")
  titleName <- NodeSelection(nodeID, depth = 0)[, name]
  nodeTable <- NodeSelection(nodeID, depth = depth, skip = TRUE)
  dvl <- DateVectorList(timeParms$timeUnit, 
                        timeParms$startQuarterID, 
                        timeParms$quarterCount,
                        timeParms$yearRange)
  msl <- multiStrataCategories(dvl)
  pdt <- plotData(nodeTable[,NodeID], msl, as.list(statIDList), variableName, weighting)
  ldt <- levelledPlotData(pdt, dvl, nodeTable)
  plt <- acAreaPlotBuild(data = ldt, 
                       title = titleName)

  return(plt)
}

acAreaPlotBuild <- function(data, title) {
  require(ggplot2)
  
  plotData <- data
  setnames(plotData, "mean", "PlotValue")
  p <- ggplot(data = plotData, aes(x = TimePeriodNumber, y = PlotValue, fill = NodeID, group = NodeID)) + 
    geom_area() + 
    scale_fill_brewer(name = "Product", 
                      palette = "Set2") + 
    scale_y_continuous(labels = scales::percent) + 
    ggtitle(paste("Weight Percent by Product / ", title)) +
    xlab("Time Period") + 
    ylab("") + 
    facet_grid(ProgramNumber ~ .)
  return(p)
}

#' @name DateVectorList
#' 
#' @desc Utility function to convert shiny date parameters into a
#'       list of date vectors
#' 
#' @return list of date vectors.  One item per period to be charted in ascending order.
#'         each item is a vector that contains the QuarterIDs that make up the period.
#'         Each item is given a name suitable for an x-axis label
#'
#' @param
#' unitType:       0 for quarters and 1 for years
#' startQuarterID: ID of the first quarter to display (ignored if timeType = 1)
#' quarterCount:   number of quarters to display (ignored if timeType = 1)
#' yearValues:     2 element vector of first and last year values (not the IDs)
#'                 to display (only used for timeType = 1)

DateVectorList <- function(unitType, startQuarterID, quarterCount, yearRange) {
  q <- GetDataObject("quarter")
  setkey(q, PeriodStartDate)
  dvlist <- list()
  if ( unitType == 0) {
    firstDate <- q[QuarterID == startQuarterID, PeriodStartDate]
    q <- q[PeriodStartDate >= firstDate]
    dvlist <- q[1:quarterCount, QuarterID]
    names(dvlist) <- q[1:quarterCount, PeriodName]
  } 
  
  else if (unitType == 1) {
    years <- yearRange[1]:yearRange[2]
    for (year in years) {
      dvlist[[as.character(year)]] <- q[Year == year, QuarterID]
    }
  }
  return(dvlist)
}

#' @name multiStrataCategories
#' 
#' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
#' 
#' @param 
#' datevectorlist:   each item is a list of quarterIDs
#' 
#' @details 
#' DataTable columns: MultiStratumHash, ProgramNumber (0,1,2 ...n), TimePeriodNumber (0, 1, 2, 3 ... )
#' 
#' @description 
#' yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R)
#' a multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
#' a multi stratum contains statistics for a given combination of strata
#' this function creates a data structure which defines multistrata
#'  creates Multistrata for each program in the database and each time unit in the datevectorlist parameter plus
#'  an additional multistratum for all prgrams (Canada) and each time unit in the datevector list
#' time period numbers assume time in ascending order
#' program numbers start with Canada and then order programs according to the program SortOrder paramter
#'  
multiStrataCategories <- function(datevectorlist){
  
  #' ProgramIDVector
  #' 
  #' returns: a table of all the Programs ordered by their sortorder
  ProgramIDVector <- function() {
    subs <- GetDataObject("subprogram")
    progs <- unique(subs[,.(ProgramID, ProgSortOrder)])
    progV = as.vector(progs[order(ProgSortOrder), ProgramID])
    return(progV)
  }
  
  #' SubprogramIDVector
  #' 
  #' returns: a vector of subprogramsIDs belonging to the programIDs
  #' 
  #' programIDs: a vector of programs IDs if NULL then all subprograms are included
  SubprogramIDVector <- function(programIDs = NULL) {
    subs <- GetDataObject("subprogram")
    
    if (is.null(programIDs)) {
      sVector <- as.vector(subs[, SubprogramID])
    } else {
      sVector <- as.vector(subs[ProgramID %in% programIDs, SubprogramID])
    }
    return(sVector)
  }
  
  
  # function entry point
  # Get strata
  strata <- GetDataObject("stratum")[, .(StratumID, QuarterID, ProgramID, SubprogramID)]
  
  # loop around programs and then around time periods to build stratum lists for each program
  pIDVector <- ProgramIDVector()
  pCount <- 0
  msl <- data.table(MultiStratumHash = character(), 
                    StratumID = integer(),
                    ProgramNumber = integer(), 
                    TimePeriodNumber = integer())
  for (programID in pIDVector) {
    pCount <- pCount + 1
    subIDVector <- SubprogramIDVector(programID)
    tCount <- 0
    for (qIDVector in datevectorlist) {
      tCount <- tCount + 1
      # sl <- StratumList(qIDVector, subIDVector)
      sl <- strata[ProgramID == programID & QuarterID %in% qIDVector, .(StratumID)]
      if (nrow(sl) > 0) {
        msHash <- Hash(c(pCount, tCount))
        msl <- rbind(msl, sl[ , .(MultiStratumHash = msHash, StratumID, ProgramNumber = pCount, TimePeriodNumber = tCount)])
      }
    }
  }
  
  # loop time periods to build stratum lists for all programs combined (Canada)
  subIDVector <- SubprogramIDVector()   # All programs
  pCount <- 0
  tCount <- 0
  for (qIDVector in datevectorlist) {
    tCount <- tCount + 1
    # sl <- StratumList(qIDVector, subIDVector)
    sl <- strata[QuarterID %in% qIDVector, .(StratumID)]
    if (nrow(sl) > 0) {
      msHash <- Hash(c(pCount, tCount))
      msl <- rbind(msl, sl[,.(MultiStratumHash = msHash, StratumID, ProgramNumber = pCount, TimePeriodNumber = tCount)])
    }
  }
  return(msl)
}

#' @name plotData
#' 
#' @return a table of values to plot - the data is in the format most conveniently returned by MultiStratumStats
#' 
#' @param 
#' nodeSelection: a table of nodes to plot (requires a NodeID column)
#' multiStratumTable: the table of subprograms and quarters (see tsMultiStrataCategories function)
#' statisticID: a list of statisticIDs
#' weightingType: "Forecast", "Actual" or "Unweighted"
#' variable: the list of variables to be plotted ("units", "gr", "mean", "obs", "var", "stdDev")
#' 
#' @details 
#' DataTable columns: Program(str), TimePeriodDesc(str), TimePeriodValue(integer), StatValue(double)
#'  
plotData <- function(nodeList, multiStratumList, statisticID, variable, weightingType){
  
  # get the multistratum statistics
  source("R/PrepareMultistratStats.R")
  plotDataTable1 <- MultiStratumTable(nodeList, multiStratumList, as.list(statisticID), weightingType)
  
  # create hashTable and merge
  hashTable <- unique(multiStratumList[,.(MultiStratumHash, ProgramNumber, TimePeriodNumber)])
  plotDataTable2 <- merge(plotDataTable1, hashTable, by = "MultiStratumHash", all.y = TRUE, allow.cartesian = TRUE)
  return(plotDataTable2)
}
#' @name levelledPlotData
#' 
#' @return a table of values to plot - with ordered factors for Node, Program and TimePeriod
#' 
#' @param 
#' plotData:         the table of data to plot
#' datevectorlist:   list of named datevectoritems (names are suitable for Axis labels)
#' nodeSelection:    the list of nodes
#' 
#' @details 
#' creates ordered factors
#' Node:  Factors are NodeIDs ordered according to the node Sort Order field.  Labels are node names
#' Program: Factors are ProgramNumber (0 for Canada, 1 to Number of programs) ordered by programNumber. Labels are "Can" and ProgramNames
#' TimePeriods: Factors are TimePeriodNumber ordered by TimePeriodNumber.  Names are "yyyy" or "Qn-yyyy" depending on TimePeriod type
#'  
levelledPlotData <- function(plotData, datevectorlist, nodeSelection){
  
  ProgramLabels <- function() {
    subs <- GetDataObject("subprogram")
    progs <- unique(subs[, .(ProgramID, ProgHdgE, ProgSortOrder)])
    setnames(progs, "ProgHdgE", "ProgramLabel") 
    progs <- rbind(progs, data.table(ProgramID = 0, ProgramLabel = "Can", ProgSortOrder = 0))
    progs <- progs[order(ProgSortOrder),ProgramLabel]
    return(progs)
  }
  
  StatisticLabels <- function() {
    stats <- GetDataObject("statistic")
    sLabels <- stats[,.(StatisticID, NameE)]
    return(sLabels)
  }
  
  plotData$TimePeriodNumber <- ordered(plotData$TimePeriodNumber, 
                                       levels = 1:length(datevectorlist), 
                                       labels = names(datevectorlist))
  plotData$ProgramNumber <- ordered(plotData$ProgramNumber, labels = ProgramLabels())
  plotData$NodeID <-  ordered(plotData$NodeID, levels=nodeSelection[,NodeID], labels = nodeSelection[,name])
  sLabels <- StatisticLabels()
  plotData$StatisticID <- factor(plotData$StatisticID, levels = sLabels[,StatisticID], labels = sLabels[,NameE]) 
  
  return(plotData)
}

ggplotFormatElements <- function() {
  ce <- data.table(StatisticID = integer(),
                   Variable = character(),
                   Title = character(),
                   ScalingFactor = numeric(),
                   YScaleCode = character())
  setkeyv(ce, c("StatisticID", "Variable"))
  statID <- 1 # WtPct
  ce <- rbind(ce, list(statID, "units", "Wt% - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Wt% - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Wt% - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Wt% - Mean", 1, "pct"))
  ce <- rbind(ce, list(statID, "var", "Wt% - Variance", 1, "pct"))
  ce <- rbind(ce, list(statID, "stdDev", "Wt% - Std Deviation", 1, "pct"))
  ce <- rbind(ce, list(statID, "lowerCI", "Wt% - Lower 95% CI", 1, "pct"))
  ce <- rbind(ce, list(statID, "upperCI", "Wt% - Upper 95% CI", 1, "pct"))
  
  statID <- 2 # Wt per unit
  ce <- rbind(ce, list(statID, "units", "Unit Wt - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Unit Wt - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Unit Wt - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Unit Wt (gr) - Mean", 1, "comma"))
  ce <- rbind(ce, list(statID, "var", "Unit Wt (gr) - Variance", 1, "comma"))
  ce <- rbind(ce, list(statID, "stdDev", "Unit Wt (gr) - Std Deviation", 1, "comma"))
  ce <- rbind(ce, list(statID, "lowerCI", "Unit Wt (gr) - Lower 95% CI", 1, "comma"))
  ce <- rbind(ce, list(statID, "upperCI", "Unit Wt (gr) - Upper 95% CI", 1, "comma"))
  
  statID <- 3 # Average Age
  ce <- rbind(ce, list(statID, "units", "Age - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Age - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Age - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Mean Age in years", 1, "comma"))
  ce <- rbind(ce, list(statID, "var", "Age Variance", 1, "comma"))
  ce <- rbind(ce, list(statID, "stdDev", "Age Std Deviation", 1, "comma"))
  ce <- rbind(ce, list(statID, "lowerCI", "Age Lower 95% CI", 1, "comma"))
  ce <- rbind(ce, list(statID, "upperCI", "Age Upper 95% CI", 1, "comma"))
  
  statID <- 6 # Damage
  ce <- rbind(ce, list(statID, "units", "Damage% - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Damage% - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Damage% - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Damage% - Mean", 1, "pct"))
  ce <- rbind(ce, list(statID, "var", "Damage% - Variance", 1, "pct"))
  ce <- rbind(ce, list(statID, "stdDev", "Damage% - Std Deviation", 1, "pct"))
  ce <- rbind(ce, list(statID, "lowerCI", "Damage% - Lower 95% CI", 1, "pct"))
  ce <- rbind(ce, list(statID, "upperCI", "Damage% - Upper 95% CI", 1, "pct"))
  
  statID <- 7 # Cannibalization
  ce <- rbind(ce, list(statID, "units", "Cannibalization% - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Cannibalization% - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Cannibalization% - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Cannibalization% - Mean" , 1, "pct"))
  ce <- rbind(ce, list(statID, "var", "Cannibalization% - Variance", 1, "pct"))
  ce <- rbind(ce, list(statID, "stdDev", "Cannibalization% - Std Deviation", 1, "pct"))
  ce <- rbind(ce, list(statID, "lowerCI", "Cannibalization% - Lower 95% CI", 1, "pct"))
  ce <- rbind(ce, list(statID, "upperCI", "Cannibalization% - Upper 95% CI", 1, "pct"))
  
  statID <- 8 # Average Screen Size
  ce <- rbind(ce, list(statID, "units", "Screen Size - units sampled", 1, "comma"))
  ce <- rbind(ce, list(statID, "datalines", "Screen Size - datalines", 1, "comma"))
  ce <- rbind(ce, list(statID, "gr", "Screen Size - Kg sampled", .0001, "comma"))
  ce <- rbind(ce, list(statID, "mean", "Screen Size - Mean" , 1, "comma"))
  ce <- rbind(ce, list(statID, "var", "Screen Size - Variance", 1, "comma"))
  ce <- rbind(ce, list(statID, "stdDev", "Screen Size - Std Deviation", 1, "comma"))
  ce <- rbind(ce, list(statID, "lowerCI", "Screen Size - Lower 95% CI", 1, "comma"))
  ce <- rbind(ce, list(statID, "upperCI", "Screen Size - Upper 95% CI", 1, "comma"))
  
}
