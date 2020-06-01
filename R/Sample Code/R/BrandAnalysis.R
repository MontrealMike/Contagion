# BranAnalysis.R
#
# Michael Bleau
# 2018-10-26
#
# Functions to generate Brand Analysis charts and tables for display in Shiny
# 

#' @section Test

barTest <- function(){
  rm(list = ls())
  source("R/UtilityFunctions.R")
  nodeID <- 12000
  programList <- c("P4", "P5", "P6", "P7", "P1", "P8", "P9")
  # programList <- c("P7")
  includeSummary = TRUE
  timeType <- 2
  yearValues <- c(2014, 2018)
  quarterStartID = 24
  quarterCount = 8
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    yearRange = yearValues)
  statID <- 1  #wt ratio
  # statID <- 2  #unit wt
  # variableName <- "units"
  variableName <- "mean"
  
  brandParms <- list()
  # brandParms$selectionType <- 0
  brandParms$selectionType <- 0
  brandParms$brandIDs <- c(18, 5, 4)
  # brandParms$brandIDs <- NULL
  brandParms$maxCount <- 20
  brandParms$summarization <- 2
  
  bar <- GetbarChartTable(nodeID = nodeID, 
                          brandParms = brandParms,
                          programList = programList, 
                          includeSummary = includeSummary,
                          statisticID = statID,
                          variableName = variableName,
                          timeParms = timeParms)
  return(bar)
}

#' @section main
source("R/UtilityFunctions.R")


#' @name GetbarChartTable
#' 
#' @desc Generate area charts and data tables for display with Shiny.  
#'       Both show time on the x axis and 
#'       the selected variable/statistic on the y axis.
#'       There is a chart and a table for each requested brand
#'        
#' @param 
#' nodeID           the ID of the parent node.  The area plot will be for the nodes 1 level below
#' brandParms       a list describing the desired brands
#'       selectionType    0       all brands (up to maxCount) or
#'                        1       user provided list of brands
#'       maxCount      if selectionType is 0, the maximum number of brands to be reported on
#'       brandIDs      user provided list of IDs (only used if selectionType is 1)
#'       summarization 0 -> none, 1 -> a summary row for non-specified brands (all other) or
#'                     2 -> non specified brand summary row and a full summary row.
#' programList      a list of the program ID's to include (each with a P prefix)
#' includeSummary   if TRUE, a program summary level is added
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

GetbarChartTable <- function(nodeID, 
                             brandParms, 
                             programList, 
                             includeSummary,
                             statisticID,
                             variableName,
                             timeParms ) {
  
  #' @name BrandSummaryList
  #' 
  #' @desc Produce a data object with the information needed to summarize, sort and present brand information  
  #' 
  #' @return a three item list
  #'  brandTable     table of programs with columns: 
  #'                 SummaryBrandID - the value on which brands should be grouped
  #'                 BrandID - the brands that belong to the group
  #'  summaryBrandLevels  a sorted list of levels for the SummaryBrandID field
  #'  summaryLabels       a list of labels to assign to those levels
  #' 
  #' @param 
  #' brandParms      a data object containing the brandSelection parameters
  #' brandStats      table of single stratum statistics (for the node strata and statistic of interest)
  #' strata          a list of the strata to be reported on
  #' 
  #' @description 
  #' There are too many brands to be reported on in one chart or report
  #' This function selects the brands to be reported on according to user specifications
  #'  - the user will either specify a list of brands or
  #'  - the user will request the brandParms$maxCountbrands with the most activity by weight 
  #'    for the specified node and strata
  #' 
  #' This function returns the same data structure as the IDLL functions in the 
  #' ShinyHelperFunctions.R file.  It is kept in this file because it is highly customized for this report format. 
  #' 
  #' Brand sort order is descending total weight (for the selected statistic and strata) if selectionType == 0 or
  #' the order requested by the user if selectionType == 1
  #' 
  BrandSummaryList <- function(brandParms, brandStats) {
    
    brand <- GetDataObject("brand")[,.(BrandID, Name = NameE)]
    
    # put the top Brands in the list
    if (brandParms$selectionType == 0) {
      brandWeight <- brandStats[, .(gr = sum(gr)), by = BrandID][order(-gr)]
      brandParms$brandIDs <- brandWeight[1:brandParms$maxCount, BrandID]
    }
    
    # awkward way to create a data table while preserving the sort order of the brandIDs list
    brandTable <- as.data.table(as.integer(brandParms$brandIDs))
    names(brandTable)[1] <- "SummaryBrandID"
    brandTable$SortOrder <- seq.int(nrow(brandTable))
    brandTable[, BrandID := SummaryBrandID] # BrandID and Summary ID are the same for user specified rows but will not be for summary rows added later
    brandTable <- merge(brandTable, brand, by = "BrandID")[order(SortOrder)]
    
    summaryBrandLevels <- brandTable[ , SummaryBrandID]
    summaryBrandLabels <- brandTable[ , Name]
    brandTable[ , SortOrder := NULL] # column no longer needed
    
    if (brandParms$summarization > 0) {
      nextID <- brandTable[ , max(BrandID)] + 1
      allOtherBrandTable <- brand[!(BrandID %in% brandParms$brandIDs),
                                  .(SummaryBrandID = nextID, BrandID, Name = "All other brands")]
      brandTable <- rbind(brandTable, allOtherBrandTable)
      summaryBrandLevels <- c(summaryBrandLevels, nextID)
      summaryBrandLabels <- c(summaryBrandLabels, "All other brands")
    }
    if (brandParms$summarization > 1) {
      # Changing the word "Summary" in the next line could have side effects on the table formats (see BuildTable function)
      brandTable <- rbind(brandTable, brandTable[, .(SummaryBrandID = 0, BrandID, Name = "Summary")])
      summaryBrandLevels <- c(0, summaryBrandLevels)
      summaryBrandLabels <- c("Summary", summaryBrandLabels)
    }
    
    return(list(summaryBrandTable = brandTable[, .(SummaryBrandID, BrandID)], summaryBrandLevels = summaryBrandLevels, summaryBrandLabels = summaryBrandLabels))
  }
  
  
  #' @name MultiStrataCategories
  #' 
  #' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
  #' 
  #' @param 
  #' periodTable      period table as produced by the GetPeriodIDLL function
  #' programTable     program table as produced by the GetProProgramdIDLL function
  #' strata           list of strata to include in each multistratum
  #' 
  #' @details 
  #' DataTable columns:  PeriodID, ProgramID, StratumID
  #' 
  #' @description 
  #' Yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R).
  #' A multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
  #' a multi stratum contains statistics for a given combination of strata
  #' this function creates a data structure which defines multistrata
  #' for each time unit in the datevectorlist parameter plus.  The multiStrata only include selected subprograms
  #'  
  MultiStrataCategories <- function(periodTable, programTable, strata){
    
    # merge in periods - to add PeriodID field
    ms <- merge(strata, periodTable, by = "QuarterID")[ ,.(PeriodID, StratumID, SubprogramID)]
    
    # merge in programs - to add ProgramID field
    # this will result in Strata appearing more than once if the programTable has Summary Level records
    # where ProgramID = 0.  (See GetProgramsIDLL function)
    ms <- merge(ms, programTable, by = "SubprogramID", allow.cartesian = TRUE)[ ,.(ProgramID, PeriodID, StratumID)]
    return(ms)
  }
  
  #' @name ReportData
  #' 
  #' @return a table of values to plot with columns: NodeID, SamplingLocationID, PeriodID and reportVariable
  #' 
  #' @param 
  #' brandStats             raw stats for the appropriate node, strata and Statistic
  #' multiStratumCategories the table of subprograms and quarters 
  #' summaryBrandTable      table showing how BrandIDs are summarized into SummaryBrandIDs
  #' variableName           the variable to retain
  #' 
  #' @details 
  #' A column named "reportVariable" is created.  It is a copy of the column specified by the variableName parameter.
  #' The report data is built without using MultiStratum stats because 1) all calculations are unweghted (and therefore to be used
  #' with caution) and 2) MultiStratumStats cannot summarize several brands into 1 SummaryBrands.
  ReportData <- function(brandStats, multiStratumCategories, summaryBrandTable, variableName){
    
    # add ProgramID and PeriodID to stats table 
    reportDataTable <- merge(brandStats, multiStratumCategories, by = "StratumID", allow.cartesian = TRUE)
    
    # add the SummaryBrandID  column to the table
    reportDataTable <- merge(reportDataTable, summaryBrandTable, by = "BrandID", allow.cartesian = TRUE)
    
    # summarize the data to the SummaryBrandID/StratumID  level first because Weight% statistics are different when
    # summarizing to this level
    reportDataTable <- reportDataTable[ , .(units = sum(units),
                                            obs = ifelse(StatisticID == 1, mean(obs), sum(obs)),  # this is the tricky part ...
                                            datalines = sum(datalines),
                                            gr = sum(gr),
                                            sum = sum(sum)),             # I know, this one is confusing.  I am summing a column whose name is sum.  Sorry.
                                        by = c("StratumID", "ProgramID", "PeriodID", "SummaryBrandID", "StatisticID")]
    
    # now straighforwardly summarize summarize to the summaryBrand/program/period level
    reportDataTable <- reportDataTable[ , .(units = sum(units),
                                            obs = sum(obs),
                                            datalines = sum(datalines),
                                            gr = sum(gr),
                                            sum = sum(sum)),             # Here it is again ... sorry ... again ...
                                        by = c("ProgramID", "PeriodID", "SummaryBrandID")]
    
    # Add a mean column
    reportDataTable[, mean := sum / obs]
    
    # create the reportVariable column and only retain needed data
    reportDataTable[, reportVariable := get(variableName)][ , .(SummaryBrandID, ProgramID, PeriodID, reportVariable)]
    
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
  #' brands                as returned by the BrandSummaryList function (includes levels list)
  #' programs              as returned by the GetNodesIDLL function (includes levels list)
  #' 
  #' @details 
  #' converting ID fields to factors allows table and charting functions to present with their names
  #' and in the appropriate sequence
  #'  
  ApplyLevels <- function(reportData, periods, brands, programs) {
    
    # convert ID field to factors
    reportData$PeriodID <- ordered(reportData$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
    reportData$ProgramID <- ordered(reportData$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
    reportData$SummaryBrandID <- ordered(reportData$SummaryBrandID, levels = brands$summaryBrandLevels, labels = brands$summaryBrandLabels)
    return(reportData)
  }
  
  #' @name BuildTable
  #' 
  #' @return a Shiny DT table for a single processor
  #'
  #' @param 
  #'    data           table to plot.  Must include the following columns: ProgramID, SummaryBrandID, PeriodID, reportVariable
  #'    displayParms   list of parameters for the statisticID, variableName as provided by the DTGetDisplayParameters function
  #'    statisticID    factor for the statistic
  #' 
  BuildTable <- function(data, displayParms, statisticID) {
    
    # create and format tableVariable
    data[ , tableVar := switch(displayParms$FormatType, 
                               "P" = scales::percent(reportVariable * displayParms$ConversionFactor),
                               "C" = scales::comma(reportVariable * displayParms$ConversionFactor, accuracy = displayParms$Accuracy))]
    if (nrow(data) == 0 ) {return(NULL)}
    
    data <- dcast(data, SummaryBrandID ~ PeriodID, value.var = "tableVar", drop = FALSE)
    
    require(knitr)
    require(kableExtra)
    
    colNames <- c(as.character(statisticID), colnames(data)[2:ncol(data)])
    options(knitr.kable.NA = "")
    htmlTable <- data %>%
      kable(format = "html", 
            linesep = "",
            col.names = colNames,
            align = c("l", rep("r", ncol(data) - 1)))

    # format summary row if there is one
    if (data[1, SummaryBrandID] == "Summary") {
      htmlTable <- row_spec(htmlTable, row = 1, bold = TRUE)
    }
    
    # it seems kableExtra functions should be applied last
    htmlTable <- kable_styling(htmlTable, 
                               bootstrap_options = c("condensed", "striped"),
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
  #'   data                Data table to plot.  Must include the following columns: ProgramID, SummaryBrandID, PeriodID, reportVariable.
  #'   displayParms        list of parameters for the statisticID, variableName as provided by the DTGetDisplayParameters function
  #' 
  #' @description 
  #' the table will have one facet (graph) per processor.  For statisticID = 1 (WtPct) and variableName "mean"
  #' the chart type is determined by the DisplayParms settting
  #' Summary rows are ignored
  #' 
  BuildChart <- function(data, displayParms) {
    require(ggplot2)
    require(RColorBrewer)
    
    # if the chrt type is an area chart, we do not want a brand summary, it will double count
    # not an issue for line charts since they are not additive
    if (displayParms$ChartType == "A") {
      data <- data[SummaryBrandID != "Summary"]
    }
    
    # add a plot variable columns
    data <- data[SummaryBrandID != "Summary" , plotVariable := reportVariable * displayParms$ConversionFactor]
    
    # get colour palette with a colour for each brand - obviously, more colours make things harder to tell apart
    # The number 8 is the number of colours available in RColorBrewer "Set2" before ramping
    brandCount = length(unique(data[, SummaryBrandID]))
    colours <- brewer.pal(8, "Set2")
    if(brandCount > 8) {
      colours <- colorRampPalette(colours)(brandCount)
    }
    
    # plot
    if (displayParms$ChartType == "A") {
      p <- ggplot(data = data, aes(x = PeriodID, 
                                   y = plotVariable, 
                                   fill = SummaryBrandID, 
                                   group = SummaryBrandID)) + 
        geom_area() + 
        scale_fill_manual(name = "Brand", values = colours)
    } else {
      p <- ggplot(data = data, aes(x = PeriodID, 
                                   y = plotVariable,
                                   group = SummaryBrandID, 
                                   colour = SummaryBrandID)) + 
        geom_line(size = 1.0) + 
        scale_color_manual(name = "Brand", values = colours)
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
  
  BuildDisplayObjects <- function(data, programIDs, displayParms) {
    displayObjects <- lapply(programIDs, 
                             function(x) list(table = BuildTable(data[ProgramID == x], displayParms, x),
                                              chart = BuildChart(data[ProgramID == x], displayParms))
    )
    return(displayObjects)
  }
  
  GetExportData <- function(data, displayParms) {
    data[ , exportVariable := reportVariable * displayParms$ConversionFactor]
    exportData <- dcast(data, SummaryBrandID + ProgramID ~ PeriodID, value.var = "exportVariable", drop = FALSE)
    return(exportData)
  }
  
  # main entry point -----------------------------------------------------------
  source("R/ShinyHelperFunctions.R")
  
  # Get Selection Leveling and Sorting categories for remaining dimensions
  programs <- GetProgramsIDLL(programList, includeSummary)
  nodes <- GetNodesIDLL(nodeID)
  periods <- GetPeriodsIDLL(timeParms)
  strata <- GetDataObject("stratum")[QuarterID %in% periods$periodTable[ , QuarterID] & 
                                       SubprogramID %in% programs$programTable[ , SubprogramID]]

  # get the raw stats data we need 
  brandStats <- GetDataObject("nodeBrandStratumStats")[StratumID %in% strata[, StratumID] & StatisticID == statisticID & NodeID == nodeID]
  if (nrow(brandStats) == 0) return(NULL)   # no data to report on
  
  # get the brands 
  brands <- BrandSummaryList(brandParms, brandStats)
  if (length(brands$summaryBrandLevels) == 0) return(NULL)  # no brands selected
  
  # Get the data summarized by MultiStratum and SummaryBrand
  msl <- MultiStrataCategories(periods$periodTable, programs$programTable, strata)
  reportData <- ReportData(brandStats = brandStats,
                           multiStratumCategories = msl,
                           summaryBrandTable = brands$summaryBrandTable,
                           variableName = variableName)
  
  levelledReportData <- ApplyLevels(reportData, periods, brands, programs)
  
  # Build table and chart objects
  displayParms <- DTGetDisplayParameters(statisticID, variableName)
  displayObjects <-BuildDisplayObjects(levelledReportData,
                                       programs$programLabels,
                                       displayParms)
  
  # Get report caption
  caption <- DTGetReportCaption(mainTitle = "Brand Analysis Report",
                                nodeSelection = nodeID,
                                statisticID = statisticID,
                                variableName = variableName,
                                weighting = "unweighted",
                                tableCaption = FALSE)
  
  # Assemble data object to be returned
  barData <- list(caption = caption,
                  displayObjects = displayObjects,
                  tableCount = length(displayObjects),
                  tableRowCount = length(brands$summaryBrandLevels), 
                  exportData = GetExportData(levelledReportData, 
                                             displayParms))
  
  return(barData)
}