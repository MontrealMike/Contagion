
source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

pdfDataTest <- function(){
  nodeSelection <- "1000"
  level <- -1
  geoSelection  = "C1"
  timeSelection = "Y-2016"
  weighting = "forecast"
  # includeOESFlag = FALSE
  drk <- GetReportData(nodeSelection, level, geoSelection, timeSelection, weighting)
  return(drk)
}

GetReportData <- function(nodeSelection, level, geoSelection, timeSelection, weighting) {
  
  GetRegionName <- function(geoSelection)  {
    return(GeoSelection(geoSelection)$desc)
  }
  
  GetPeriodName <- function(timeSelection) {
    return(TimeSelection(timeSelection)$desc)
  }
  
  GetLastUpdateMsg <- function() {
    return(DTDataEffectiveDate())
  }
  
  FormatScaleAlignColumn <- function(columnData, statisticID, variableName, columnType = "sum", displayParameters = NULL) {
    library(scales)  # provides comma and percent funtions
    dp <- DTGetDisplayParameters(statisticID, variableName)
    if (length(dp) > 0) {
      alignment <- "r"
      displayName <- dp$DisplayName # column name
      switch(dp$FormatType,
             "C" = {
               columnData <- comma(
                 columnData,
                 scale = dp$ConversionFactor,
                 accuracy = dp$Accuracy)
             },
             "P" = {
               columnData <- percent(
                 columnData,
                 accuracy = dp$Accuracy)
             }
      )
    } else {
      alignment <- "l"
      displayName <- NULL
    }
    return(list(columnData = columnData,
                displayName = displayName,
                alignment = alignment))
    
  }
  
  
  GetProgramDetailReport <- function(nodeList, msl, strata, weighting) {
    #   statistics parameters (ordered)
    statsList <- c(1, 2, 6, 7, 3, 8)
    
    # get the data
    source("R/PrepareMultistratStats.R")
      reportData <- MultiStratumTable(nodeSelectionTable = nodeList,
                                    stratumSelectionTable = msl, 
                                    statisticList = statsList,
                                    weighting = weighting)
    
    # shape and convert the data
    setkey(nodeTable, NodeID)
    setkey(reportData, NodeID)
    
    # Initial columns
    contents <- reportData[StatisticID == 1, 
                           .(NodeID, units, gr)]
    setkey(contents, NodeID)
    contents <- contents[nodeTable[, .(NodeID, Product = shortname, level, sortOrder)]]
    
    # Statistic columns - convert the statisticID variable to columns
    meanCols <- reshape(reportData[StatisticID %in% statsList, .(NodeID, StatisticID, meanCol = mean)],
                        idvar = "NodeID", timevar = "StatisticID", direction = "wide")
    setkey(meanCols, NodeID)
    contents <- merge(contents, meanCols, by = "NodeID")
    
    # row and column ordering
    contents <- contents[order(sortOrder)]
    colNames <- c("Product", "units", "gr", paste0("meanCol.", statsList), "level")
    contents <- contents[ , ..colNames]  # select and order columns
    
    # extract hierarchy level and remove column - to be used for report indentation
    levels <- contents$level 
    contents[ ,level := NULL]
    
    # Prepare report columns specs
    varNames <- c("Product", "units", "gr", rep("mean", times = length(statsList)))
    statisticID <- c(NA, 1, 1, statsList)
    
    # Set column names, column formats and Scale data to standard units
    columnAlignmentVector <- character(length(varNames))
    for (col in 1:length(statisticID)) {
      displayColumnInfo <- FormatScaleAlignColumn(contents[[names(contents)[col]]], statisticID[col], varNames[col])
      contents[[names(contents)[col]]] <- displayColumnInfo$columnData
      columnAlignmentVector[col] <- displayColumnInfo$alignment
      if (!is.null(displayColumnInfo$displayName)) {
        names(contents)[col] <- displayColumnInfo$displayName # column name
      }
    }
    
    # format the data into Kable
    library(kableExtra)
    
    # this may generate a warning for levels that are not found in the actual data
    # the warning is harmless
    detailReport <- kable(contents,
                          # format = "latex",
                          # booktabs = TRUE,
                          # longtable = TRUE,
                          align = columnAlignmentVector,
                          caption = "Detail Report")  %>%
      kable_styling(latex_options = c("HOLD_position", "repeat_header"),
                    font_size = 8) %>%
      column_spec(1, width = "8.4cm") %>% 
      column_spec(2:ncol(contents), "0.95cm") %>%
      row_spec(1:(nrow(contents) - 1), hline_after = TRUE) %>%
      row_spec(which(levels==1), bold = T, background ="#129B41") %>%
      row_spec(which(levels==2), bold = F, background ="#EDF2E2") %>%
      add_indent(which(levels>2)) %>%
      add_indent(which(levels>3)) %>%
      add_indent(which(levels>4))
    return(detailReport)
  }
  
  
  #' @name GetProgramComparisonReport
  #' 
  #' @desc return a Kable report comparing program statistics for a selected time period
  #' 
  #' @param 
  #' nodeID:          the ID of the parent node.  The area plot will be for the nodes 1 level below
  #' timeSelection:   0 for quarters, 1 for years
  #' statisticID:     StatisticID (a singleID)
  #' variableName:    Name of desired variable - choices are
  #'                  ("units", "datalines", "gr", "mean", "var", "stdDev", "lowerCI", "upperCI")
  #' weighting:       weighting factor to be used for multistratum statistic calculations ("forecast", "actual" or "unweighted")
  #' includeOES:      if TRUE, include the Ontario Environmental Stewardship program
  #' 
  #' @return report object
  GetProgramComparisonReport <- function(nodeList, timeSelection, weighting, statisticID = 1, 
                                         variableName = "mean", includeOES = FALSE ) {
    
    #' @name GetMultiStratumSelectionTable
    #' 
    #' @return table for selecting multistratum stats
    #' 
    #' @parm
    #'   programs        a list of Program IDs 
    #'   timeSelection   a text string describing the time period to be returned
    #'                   see the TimeSelection() function in ShinyHelperFunctions.R for details
    #'   
    #' @desc
    #'   returns a table in the format required for the stratumSelectionTable required by the
    #'   MultiStratum table function.  The table will specify the strata to assemble for
    #'   each Program and those to assemble for all of Canada
    #'   
    #'   2018-08-25 (MRB)
    #'    - added programs parameter to select specific programs
    GetMultiStratumSelectionTable <- function(programs, timeSelection) {
      
      quarters <- TimeSelection(timeSelection)$quarters
      
      s <- GetDataObject("stratum")[QuarterID %in% quarters & ProgramID %in% programs, .(StratumID, ProgramID)]
      
      
      # concatenate programs into Selection Table
      selectionTable <- data.table(MultiStratumHash = character(), StratumID = integer())
      for (prog in programs) {
        selectionTable <- rbind(selectionTable, 
                                s[ProgramID == prog, .(MultiStratumHash = as.character(prog), 
                                                       StratumID)])
      }
      
      # add all rows for Canada Wide data
      selectionTable <- rbind(selectionTable, 
                              s[, .(MultiStratumHash = "Sum",
                                    StratumID)])
      return(selectionTable)  
    }
    
    programs <- unique(GetDataObject("subprogram")[, .(ProgramID = as.character(ProgramID), ProgramShortname, ProgSortOrder)])[order(ProgSortOrder)]
    if (!includeOES) {
      programs <- programs[ProgramID != 3]
    }
    
    stratumSelection <- GetMultiStratumSelectionTable(programs[,ProgramID], timeSelection)
    # if we have no strata, bail and return NULL
    if (is.null(stratumSelection)) {
      return(NULL)
    }
    
    # get the needed strata - and pick out the selected variable
    contents <- MultiStratumTable(nodeSelectionTable = nodeList,
                                stratumSelection = stratumSelection, 
                                statisticList = statisticID,
                                weighting = weighting)[, value := .SD[[variableName]], .SDcols = variableName][ , .(NodeID, ProgramID = MultiStratumHash, value)]
    
    # add records for even those node programs combos without data
    progNode <- Cartesian.dt(programs[ , .(ProgramID)], nodeTable[ , .(NodeID)]) # All program Nodes
    progNode <- rbind(progNode, nodeTable [ ,. (NodeID, ProgramID = "Sum")])     # Add nodes for summary column
    contents <- merge(contents, progNode,  by = c("ProgramID", "NodeID"), all.x = TRUE, all.y = TRUE) # this is like an outer join
    
    # reshape programs into columns
    contents <- reshape(contents, 
                      idvar = "NodeID", 
                      timevar = "ProgramID", 
                      direction = "wide")
    
    # attach node descriptive columns and sort
    contents <- merge(contents, nodeTable[, .(NodeID, nodeName = shortname, level, sortOrder)], by = "NodeID")[order(sortOrder)]

    # extract hierarchy level and remove column
    levels <- contents$level 
    contents[ ,level := NULL]
    
    # columns names and sort order
    sortedColNames <- c("nodeName", paste0("value.", programs[ , ProgramID]), "value.Sum")
    contents <- contents[ , ..sortedColNames]
    
    # Prepare report columns specs
    varNames <- c("Product", rep("mean", times = nrow(programs) + 1))
    statisticID <- c(NA, rep(1, times = nrow(programs) + 1))
    
    # Set column formats and Scale data to standard units
    columnAlignmentVector <- character(length(varNames))
    for (col in 1:length(statisticID)) {
      displayColumnInfo <- FormatScaleAlignColumn(contents[[names(contents)[col]]], statisticID[col], varNames[col])
      contents[[names(contents)[col]]] <- displayColumnInfo$columnData
      columnAlignmentVector[col] <- displayColumnInfo$alignment
    }
    
    # customize display column names since this is all about programs
    names(contents) <- c("Product", programs[ , ProgramShortname], "Summary")
    
    # format the data into Kable
    library(kableExtra)
    
    # this may generate a warning for levels that are not found in the actual data
    # the warning is harmless
    comparisonReport <- kable(contents,
                          # format = "latex",
                          # booktabs = TRUE,
                          # longtable = TRUE,
                          align = columnAlignmentVector,
                          caption = "Program Comparison Report") %>%
      kable_styling(latex_options = c("HOLD_position", "repeat_header"),
                    font_size = 8) %>%
      column_spec(1, width = "8.4cm") %>% 
      column_spec(2:ncol(contents), "0.95cm") %>%
      row_spec(which(levels==1), bold = T, background ="#129B41") %>%
      row_spec(which(levels==2), bold = F, background ="#EDF2E2" ) %>%
      add_indent(which(levels>2)) %>%
      add_indent(which(levels>3)) %>%
      add_indent(which(levels>4))
    return(comparisonReport)
  }
  
  GetProcessorComparisonReport <- function(multiStratumList) {
    
    GetProcessorStats <- function(nodeID, statistic, multistratumList) {
      weighting <- "unweighted"
      
      stats <- MultiStratumTable(nodeSelectionTable = nodeID,
                                 stratumSelectionTable = multiStratumList, 
                                 statisticList = list(statistic), 
                                 weighting = weighting,
                                 locationID = TRUE)
      
      # keep only needed columns and rows (means)
      neededCols <- c("SamplingLocationID", "units", "gr", "mean")
      stats <- stats[ !is.na(SamplingLocationID), ..neededCols]
      
    }
    
    # Hard coded data (appropriate for a custom report I guess)
    masterHierarchyNodeID <- 1000
    displayProductsNodeID <- 12000

    mhStats <- GetProcessorStats(masterHierarchyNodeID, 2, multiStratumList)
    dpStats <- GetProcessorStats(displayProductsNodeID, 1, multiStratumList)
    
    # Merge data
    contents <- merge(mhStats, dpStats[ , .(SamplingLocationID, mean)], by = "SamplingLocationID")
    
    # Get processor names and sort order using ordered factor
    processors <- GetProcessorLocationsIDLL(list(selectionType = "allActive", locationIDs = NULL))
    contents$SamplingLocationID <- factor(contents$SamplingLocationID, levels = processors$locationLevels, labels = processors$locationLabels)

    # Prepare report columns specs
    varNames <- c("Processor", "units", "gr", "mean", "mean")
    statisticID <- c(NA, 1, 1, 2, 1)
    
    # Set column names, column formats and Scale data to standard units
    columnAlignmentVector <- character(length(varNames))
    for (col in 1:length(statisticID)) {
      displayColumnInfo <- FormatScaleAlignColumn(contents[[names(contents)[col]]], statisticID[col], varNames[col])
      contents[[names(contents)[col]]] <- displayColumnInfo$columnData
      columnAlignmentVector[col] <- displayColumnInfo$alignment
      if (!is.null(displayColumnInfo$displayName)) {
        names(contents)[col] <- displayColumnInfo$displayName # column name
      } else {
          names(contents)[col] <- varNames[col]
      }
    }
    # customize last column name
    names(contents)[ncol(contents)] <- "Display Product % by wt"
    
    # format the data into Kable
    library(kableExtra)
    
    # this may generate a warning for levels that are not found in the actual data
    # the warning is harmless
    processorReport <- kable(contents,
                          # format = "latex",
                          # booktabs = TRUE,
                          # longtable = TRUE,
                          align = columnAlignmentVector,
                          caption = "Processor Comaprison Report")  %>%
      kable_styling(latex_options = c("HOLD_position", "repeat_header"),
                    font_size = 8) %>%
      row_spec(1:(nrow(contents) - 1), hline_after = TRUE) %>%
      column_spec(1, width = "5cm") %>% 
      column_spec(2:ncol(contents), "1.2cm")
    return(processorReport)
  }
  
  # Main Entry point ---------------------------------------------------------
  
  #   node parameters
  nodeTable <- NodeSelection(nodeSelection, GetDataObject("node"), depth = level, skip = FALSE)
  nodeList = nodeTable[, NodeID]
  
  #   strata parameters
  strata <- as.data.table(StratumSelection(geoSelection, timeSelection))
  
  if (length(strata$strataID) == 0 ) {
    return(NULL)
  }
  msl <- strata[ , .(MultiStratumHash = "", StratumID = strataID)]
  
  reportData <- list(region = GetRegionName(geoSelection),
                     period = GetPeriodName(timeSelection),
                     weighting = weighting,
                     lastUpdateMessage = GetLastUpdateMsg(),
                     detailReport = GetProgramDetailReport(nodeList, msl, strata, weighting),
                     comparisonReport = GetProgramComparisonReport(nodeList, timeSelection, weighting,
                                                                statisticID = 1, variableName = "mean"),
                     processorReport = GetProcessorComparisonReport(multiStratumList = msl)
                     )
  return(reportData)
}
