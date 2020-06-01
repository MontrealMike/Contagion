#
# Michael Bleau
# 2015-03-02
#
# Analysis of container weights

#' @changes
#' 2019-02-06 (MRB)
#' - corrected bug that caused crash wnen variable "units" was chosen

caTest <- function(){
  source("R/UtilityFunctions.R")
  source("R/ShinyHelperFunctions.R")
  programList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
  # programList <- c("P9")
  includeSummary <- TRUE
  timeType <- 2
  yearValues <- c(2014, 2018)
  quarterStartID <- 1
  quarterCount <- 8
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    yearRange = yearValues)
  organizeBy <- "containerType"
  variable <- "units"
  caChartTable <- GetCaChartTable(organizeBy = organizeBy,
                                  programList = programList,
                                  includeSummary = includeSummary,
                                  timeParms = timeParms,
                                  variable = variable)
}

#' @name GetCaChartTable
#' 
#' @desc Generate area charts and data tables for display with Shiny.  
#'        
#' @param 
#' organizeBy         must be either "program" or "containerType" (See details section)
#' programList        a list of the program ID's to include (each with a P prefix)
#' includeSummary     TRUE if summary level program data is desired
#' timeParms          a list of parameters describing the time periods and units to cover
#' variable           the name of the variable on which to report (defaults to "meanGr")
#'
#' @details 
#'  if the organizeBy parameter is "program" there will be a table and chart for each program and
#'  each table and chart will present detail by container type

#'  if the organizeBy parameter is "containerType" there will be a table and chart for each container type and
#'  each table and chart will present detail by program
#'  
#'  Available variables are "containers", "units", "meanUnits", 
#'   
#' @return a five item list of objects
#'         caption           for display in shiny using renderUI and uiOutput
#'         displayObjects    a two item list.
#'               tables         a vector of chart kable tables - 1 per program or container type (depending on organizeBy)
#'               charts         a vector of plots - 1 per program or container type (depending on organizeBy)
#'         tableCount        number of tables in the tables vector (same as number of charts in the chart vector)
#'         tableRowCount     number of items in each chart - either programs or container types (depending on organizeBy)
#'         exportData        a simple table combining all the data contained in the tables vector for export
GetCaChartTable <- function(organizeBy, programList, includeSummary, timeParms, variable = "meanGr") {

  # customized formats for container data using same format as DTGetDisplayParameters function
  GetContainerDataFormats <- function(variable) {
    
    dpList = list()
    switch(variable, 
           "containers" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 0
             dpList$Accuracy <- 0
             dpList$ConversionFactor <- 1
             dpList$uom <- "units"
             dpList$DisplayName <- "Containers"
             dpList$ReportName <- "Containers"
             dpList$ChartType <- "A"
           },
           "units" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 0
             dpList$Accuracy <- 0
             dpList$ConversionFactor <- 1
             dpList$uom <- "units"
             dpList$DisplayName <- "Sampled items"
             dpList$ReportName <- "Sampled items"
             dpList$ChartType <- "A"
           },
           "sumGr" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 1
             dpList$Accuracy <- 1 / 10
             dpList$ConversionFactor <- 1 / 1000 # grams to kilos
             dpList$uom <- "kg"
             dpList$DisplayName <- "Sampled wt (kg)"
             dpList$ReportName <- "Sampled weight"
             dpList$ChartType <- "A"
           },
           "meanUnits" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 1
             dpList$Accuracy <- 1 / 10
             dpList$ConversionFactor <- 1
             dpList$uom <- "units"
             dpList$DisplayName <- "Items per container"
             dpList$ReportName <- "Items per container"
             dpList$ChartType <- "L"
           },
           "stdDevUnits" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 1
             dpList$Accuracy <- 1 / 10
             dpList$ConversionFactor <- 1
             dpList$uom <- "units"
             dpList$DisplayName <- "Std dev items per container"
             dpList$ReportName <- "Standard deviation of items per container"
             dpList$ChartType <- "L"
           },
           "meanGr" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 1
             dpList$Accuracy <- 1 / 10
             dpList$ConversionFactor <- 1 / 1000 # grams to kilos
             dpList$uom <- "kg"
             dpList$DisplayName <- "Wt per container (kg)"
             dpList$ReportName <- "Average container weight"
             dpList$ChartType <- "L"
           },
           "stdDevGr" = {
             dpList$FormatType <- "C"
             dpList$Decimals <- 1
             dpList$Accuracy <- 1 / 10
             dpList$ConversionFactor <- 1 / 1000 # grams to kilos
             dpList$uom <- "kg"
             dpList$DisplayName <- "Std dev wt per container (kg)"
             dpList$ReportName <- "Standard deviation of container weight"
             dpList$ChartType <- "L"
           }
    )
    return(dpList)
  }
  
    
  GetCaData <- function(programs, periods, containers, organizeBy, variable) {
    # get observations
    co <- GetDataObject("containerObs")
    
    # merge in program data only keeping desired programs
    co <- merge(co, programs$programTable, by = "SubprogramID", all.x = FALSE, allow.cartesian = TRUE)
    # merge in period data only keeping desired periods
    co <- merge(co, periods$periodTable, by = "QuarterID", all.x = FALSE)
    
    # summarize
    caDetail <- co[,
                   .(containers = sum(obs),
                     units = sum(units),
                     meanUnits = mean(units),
                     stdDevUnits = sd(units),
                     sumGr = sum(gr),
                     meanGr = mean(gr),
                     stdDevGr = sd(gr)
                   ), 
                   by = .(ProgramID, PeriodID, ContainerTypeID)]
    caSummary <- co[,
                    .(containers = sum(obs),
                      units = sum(units),
                      meanUnits = mean(units),
                      stdDevUnits = sd(units),
                      sumGr = sum(gr),
                      meanGr = mean(gr),
                      stdDevGr = sd(gr),
                      ContainerTypeID = 0
                    ), 
                    by = .(ProgramID, PeriodID)]
    ca <- rbind(caDetail, caSummary)
    
    # apply levels
    ca$ProgramID <- ordered(ca$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
    ca$PeriodID <- ordered(ca$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
    ca$ContainerTypeID <- ordered(ca$ContainerTypeID, levels = containers$containerLevels, labels = containers$containerLabels)

    # create reporting variable applying scaling and rounding
    varFormat <- GetContainerDataFormats(variable)
    ca$ReportVariable <- round(ca[, ..variable] * varFormat$ConversionFactor, varFormat$Decimals)
    
    return(ca)
  }
  
  BuildCaTable <- function(caData, displayParms, groupName) {
    
    if (nrow(caData) == 0 ) {return(NULL)}
    
    data <- dcast(caData, RowID ~ PeriodID, value.var = "ReportVariable", drop = TRUE)
    require(knitr)
    require(kableExtra)
    
    colNames <- c(groupName, colnames(data)[2:ncol(data)])
    options(knitr.kable.NA = "")
    htmlTable <- data %>%
      kable(format = "html", 
            linesep = "",
            col.names = colNames,
            align = c("l", rep("r", ncol(data) - 1)))
    
    # format summary row if there is one
    if (data[1, RowID] == "All" | data[1, RowID == "Summary"]) {
      htmlTable <- row_spec(htmlTable, row = 1, bold = TRUE)
    }
    
    htmlTable <- kable_styling(htmlTable, 
                               bootstrap_options = c("condensed", "striped"),
                               font_size = 10) %>%
      column_spec(1, width = "20em") %>% 
      scroll_box()
    
    return(htmlTable)
  }
  
  BuildCaChart <- function(caData, organizeBy, displayParms) {
    require(ggplot2)
    
    # if no data or only one period of data return NULL (to avoid blank plot or error mdg )
    if (nrow(caData) == 0 | length(unique(caData[, PeriodID])) < 2) {
      return(NULL)
    }
    
    # check for plottable data
    scaleName <- switch(organizeBy,
                        "program" = "Container Type",
                        "containerType" = "Program"
    )
    
    # plot
    if (displayParms$ChartType == "A") {      # area plot
      p <- ggplot(data = caData[RowID != "All" & RowID != "Summary"], aes(x = PeriodID, y = ReportVariable, fill = RowID, group = RowID)) + 
        geom_area() + 
        scale_fill_brewer(name = scaleName, 
                          palette = "Set2")
    } else {                                  # line plot
      p <- ggplot(data = caData, aes(x = PeriodID, 
                                   y = ReportVariable,
                                   group = RowID, 
                                   colour = RowID)) + 
        geom_line(size = 1.0) + 
        scale_color_brewer(name = scaleName, 
                           palette = "Set2")
    }
    
    p <- p + 
      scale_y_continuous(labels = scales::comma) + 
      expand_limits(y = 0) +
      scale_x_discrete(drop = FALSE) +
      xlab("") + 
      ylab("") + 
      theme(legend.position = "right")
    
    return(p)
  }
  
  GetExportData <- function(caData, displayParms) {
    exportData <- dcast(caData, GroupID + RowID ~ PeriodID, value.var = "ReportVariable", drop = FALSE)
    return(exportData)
  }
  
  # main entry point ----------------------------------------------  
  stopifnot(organizeBy == "program" | organizeBy == "containerType")
  
  programs <- GetProgramsIDLL(programList, includeSummary)  
  periods <- GetPeriodsIDLL(timeParms)
  containers <- GetContainersIDLL()
  
  caData <- GetCaData(programs, periods, containers, organizeBy, variable)
  displayParms <- GetContainerDataFormats(variable = variable)
    
  switch(organizeBy,
         "program" = setnames(caData, old=c("ProgramID", "ContainerTypeID"), new=c("GroupID", "RowID")),
         "containerType" = setnames(caData, old=c("ContainerTypeID", "ProgramID"), new=c("GroupID", "RowID"))
  )
  
  displayObjects <- lapply(levels(caData[, GroupID]), 
                           function(x) list(table = BuildCaTable(caData[GroupID == x], displayParms, x),
                                            chart = BuildCaChart(caData[GroupID == x], organizeBy, displayParms)
                           )
  )
  
  displayFormat <- GetContainerDataFormats(variable)
  caption <- DTGetReportCaption("Container Analysis", 
                                secondRowTitle = paste0(displayFormat$ReportName, " (", displayFormat$uom, ")"),
                                weighting = "unweighted",
                                tableCaption = FALSE)
  
  caData <- list(caption = caption,
                  displayObjects = displayObjects,
                  tableCount = length(displayObjects),
                  tableRowCount = length(levels(caData[ , RowID])), 
                  exportData = GetExportData(caData, displayParms))
}         

