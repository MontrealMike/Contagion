source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

xlrTest <- function() {
  # hard coded download parameters
  nodeSelection <- 1000   # Master hierarchy 
  # depth <- 2              # all nodes 2 levels down
  depth <- -2             # all nodes 
  progList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
  timeType <- 0           # quarters statistics 
  quarterStartID <- 1      
  quarterCount <- 8     
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount)
  
  statList <- c(1, 2)     # wt ratio and unit wt
  includeSummary <- TRUE  # program and summary as well
  weighting <- "forecast"
  filename <- "excel/ESAReport.xls"
  progress <- "debug"

  CreateExcelReport(filename,
                    timeParms,
                    weighting,
                    progress)
}

#' @name CreateExcelReport
#' 
#' @desc Creates an excel woprkbook with the Excel_001 style report
#'        
#' @param 
#' filename             the full filename (includes path) under which to save the report
#' timeParms            describes the time period to be covered by the report
#'                      (see timeparms object as described in the GetPeriodsIDLL function in ShinyHelperFunctions.R)
#'                      (see the geoID parameter in the GeoSelection function in SkinyHelperFunctions.R)
#' weighting            the weighting type to be used in statistical calculations ("Forecast", "Actual" or "Unweighted")
#' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
#'                      Can also be NULL (default) or "debug"      
#'
#' @return nothing
#' 
#' @description 
#' The Excel_001 style workbook contains the following sheets
#' 
#' A set of 3 statistic sheets (SingleStatWorksheet).  Each sheet displays quarterly statistics for each program and for all programs combined.
#' The sheets are 
#'  - Weight ratios                - weight ratios for level 1 product nodes of the Master Hierarchy
#'  - Unit weight                  - unit weights for selected Master Hierarchy product nodes
#'  - Display Device Weight Ratios - weight ratios for Master Hierarchy/Display Device child nodes
#'  
#' A sheet for each program plus a Summary sheet (ProgramWorksheet). 
#' Each shows key data for each product node and each quarter:
#'   - Sampled Units
#'   - Sampled Weight
#'   - Weight% mean and % CI
#'   - Weight Ratio mean and % CI
#'   
#' 
CreateExcelReport <- function(filename,
                              timeParms,
                              weighting,
                              progress = NULL) {
  #' @name UpdateProgress
  #' 
  #' @return nothing
  #' 
  #' @param 
  #' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
  #'                      Can also be NULL (default) or "debug"      
  #' msg                  the progress message
  #'  
  #' @details 
  #' if progress is a function, it is assumed it is a shiny progress option and it is called with the message
  #' for Shiny to update the progress indicator.
  #' if progress == "debug", the message is printed to the console 
  #'  
  UpdateProgress <- function(progress, msg) {
    if(!is.null(progress)) {
      if (is.function(progress)) {
        progress(detail = msg)
      } else if (progress == "debug") {
        print(msg)
      }
    }
  }
  
  #' @name GetData
  #' 
  #' @description Get the data needed for the report
  #' @param 
  #' statList            the list of neede statistcis
  #' weighting           the weighting type to be used in statistical calculations ("Forecast", "Actual" or "Unweighted")
  #' progress            a call-back function of type shiny::Progress to provide progress reports for the UI. 
  #'                     Can also be NULL (default) or "debug"      
  #'
  #' @return a table of statistical values keyed on StratumID, StatisticID and NodeID
  #'    
  #' @details 
  #' The table contains columns for all available statistical variables. (See the MultiStratumTable function in PrepareMultiStratumStats.R)
  #' It the following additional columns:
  #'    ProgramID    an ordered factor identifying the program
  #'    PeriodID     an ordered factor identifying the time period
  #'    depth        the depth in the hierarchy of the product Node (useful for formatting)
  #'    shortname    a short name for the product Node
  #' the ProgramID, PeriodID and NodeID columns of the returned table are ordered factors to ensure consistent naming and ordering of the data
  #'  
  GetData <- function(statList, weighting, progress) {
    #' @name ReportData
    #' 
    #' @return a table of statistical values keyed on StratumID, StatisticID and NodeID
    #' 
    #' @param 
    #' statisticID            the list of statisticIDs to select
    #' weightingType          "forecast", "actual" or "unweighted"
    #' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
    #'                      Can also be NULL (default) or "debug"      
    #' 
    #' @details 
    #' Assumes the nodes, programs and periods data items are visible
    ReportData <- function(statisticID, weightingType){
      
      # cartesian join of programs and periods
      pp <- Cartesian.dt(programs$programTable, periods$periodTable)
      
      # Get strata
      strata <- GetDataObject("stratum")[,.(StratumID, SubprogramID, QuarterID)]
      
      # merge in program periods
      multiStratumCategories <- merge(strata, pp, by = c("SubprogramID", "QuarterID"))
      
      # Create hash
      multiStratumCategories[ , MultiStratumHash := paste(ProgramID, PeriodID, sep = "|")]
      
      # get the multistratum statistics
      source("R/PrepareMultistratStats.R")
      reportDataTable <- MultiStratumTable(nodes$nodeList, 
                                           multiStratumCategories, 
                                           statisticID, 
                                           weightingType)
      
      # create hashTable, merge and lose the hash column
      hashTable <- unique(multiStratumCategories[,.(MultiStratumHash, ProgramID, PeriodID)])
      reportDataTable <- merge(reportDataTable, hashTable, by = "MultiStratumHash")
      reportDataTable[, MultiStratumHash := NULL]
      
      # add depth column and short name to table
      reportDataTable[, ':=' (depth = nodes$nodeDepths[match(NodeID, nodes$nodeList)],
                              shortname = nodes$nodeShortnames[match(NodeID, nodes$nodeList)])]
      
      # replace NaN (Not a Number) values with NAs.  openxlsx handles the latter more cleanly
      reportDataTable[is.nan(mean), mean := NA]
      reportDataTable[is.nan(CIPct), CIPct := NA]
      
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
    ApplyLevels <- function(reportData) {
      
      # convert ID field to factors
      reportData$ProgramID <- ordered(reportData$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
      reportData$PeriodID <- ordered(reportData$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
      reportData$NodeID <-  ordered(reportData$NodeID, levels = nodes$nodeLevels, labels = nodes$nodeLabels)
      return(reportData)
    }
    
    # GetData main entry point ------------------------------------------
    UpdateProgress(progress, "Retrieving data ... ")
    reportData <- ReportData(statList, weighting)
    exportData <- ApplyLevels(reportData)
    return(exportData)
  }
  
  #' @name CreateWorkbook
  #' 
  #' @description 
  #' Manages creation of the workbook
  #' 
  #' @param 
  #' filename             full filename (includes path) for the workbook
  #' data                 data table to be used in creating the workbook
  #' weighting            name of the weighting logic used to calculate the data
  #' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
  #'                      Can also be NULL (default) or "debug"      
  #'                      
  #' @returns nothing
  #' 
  CreateWorkbook <- function(filename, data, weighting, progress) {
    
    #' @name GetStyleList
    #' 
    #' @description 
    #' Create a list of style variable to standardize the look of the spreadsheet
    #' 
    #' @param node
    #'                      
    #' @returns nested list of style variables
    #' 
    GetStyleList <- function() {
      s <- list()
      
      # defaults
      s$colour$dark <- "#129B41"
      s$colour$light <- "#EDF2E2"
      s$font$name <- "Arial"
      s$font$size <- 10
      
      # titles (1 for Program sheet titles,
      #         2 for Data bloc titles
      #         3 for Stat sheet titles)
      s$title$style1 <- createStyle(fontName = s$font$name,
                                    fontSize = s$font$size + 4,
                                    fontColour = s$colour$dark,
                                    textDecoration = "bold",
                                    halign = "left",
                                    valign = "center")
      s$title$style2 <- createStyle(fontName = s$font$name,
                                    fontSize = s$font$size + 1,
                                    fontColour = s$colour$dark,
                                    textDecoration = "bold",
                                    halign = "left")
      s$title$style3 <- createStyle(fontName = s$font$name,
                                    fontSize = s$font$size + 4,
                                    fgFill = s$colour$light,
                                    textDecoration = "bold",
                                    halign = "center",
                                    valign = "center")
      
      # hierarchy dependent styles - these are placed in 5 item vectors  
      # each item applies to the equivalent hierarchy level
      s$hierarchy$indentincrement <- 1
      s$hierarchy$rowheight <- c(25, 20, NA, NA, NA) # NA means use standard ht

      # these should be added last and stacked onto existing styles (stack = TRUE)
      s$hierarchy$rowstyle <- c(createStyle(fontSize = s$font$size + 2,
                                            textDecoration = "bold",
                                            border = "bottom",
                                            borderStyle = "thin",
                                            borderColour = s$colour$dark,
                                            fgFill = s$colour$light,
                                            valign = "bottom"), 
                                createStyle(textDecoration = "bold",
                                            border = "bottom",
                                            borderStyle = "thin",
                                            borderColour = s$colour$dark,
                                            fgFill = s$colour$light,
                                            valign = "bottom"),
                                createStyle(fontSize = s$font$size - 1),
                                createStyle(fontSize = s$font$size - 1),
                                createStyle(fontSize = s$font$size - 1))
      
      # report specifications info
      s$reportSpecStyle <- createStyle(fontName = s$font$name,
                                     fontSize = s$font$size - 2,
                                     textDecoration = "italic",
                                     halign = "left")
      
      # rows and columns
      s$rowheader$style <- createStyle(fontName = s$font$name,
                                       fontSize = s$font$size,
                                       halign = "left")
      s$colheader$style <- createStyle(fontName = s$font$name,
                                       fontSize = s$font$size,
                                       border = "Bottom",
                                       borderColour = s$colour$dark,
                                       borderStyle = "medium",
                                       textDecoration = "bold",
                                       halign = "center")
      
      # data blocks
      s$data$percentStyle <- createStyle(fontName = s$font$name,
                                         fontSize = s$font$size,
                                         numFmt = "0.00%")
      s$data$commaStyle0 <-  createStyle(fontName = s$font$name,
                                         fontSize = s$font$size,
                                         numFmt = "#,###")
      s$data$commaStyle3 <-  createStyle(fontName = s$font$name,   # use this one for kg
                                         fontSize = s$font$size,
                                         numFmt = "#,###.000")
      s$data$borders <- "surrounding"
      s$data$borderStyle <- "medium"
      s$data$borderColour <- s$colour$dark
      return(s)    
    }
    
    #' @name AddReportSpecs
    #' 
    #' @description 
    #' Ads a header section to a worksheet
    #' 
    #' @param 
    #' wb                   workbook
    #' wheet                worksheet (within the workbook)
    #' weighting            name of the weighting logic used to calculate the data
    #' row                  the worksheet row on which to start the header
    #' col                  the worksheet column on which top star the header
    #'                      
    #' @returns nothing
    #' 
    #' @description
    #' uses three rows for the data effective date (last upload), the print date
    #' and the weighting logic
    
    AddReportSpecs <- function(wb, sheet, weighting, row = 2, col = 1) {
      writeData(wb = wb, sheet = sheet, x = DTDataEffectiveDate(),
                colNames = FALSE, startRow = row, startCol = col)
      writeData(wb = wb, sheet = sheet, x = paste("Prepared on:", Sys.time()),
                colNames = FALSE, startRow = row + 1, startCol = col)
      writeData(wb = wb, sheet = sheet, x = paste("Weighting: ", weighting),
                colNames = FALSE, startRow = row + 2, startCol = col)
      addStyle(wb = wb, sheet = sheet, style = GetStyleList()$reportSpecStyle, 
               rows = seq(row, row + 2), cols = col)
    }
    
    #' @name PrepareProgramWorksheet
    #' 
    #' @description 
    #' Manages creation of of a program style worksheet
    #' 
    #' @param 
    #' wb                   workbook
    #' progData             the program data to be added to the new worksheet
    #' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
    #'                      Can also be NULL (default) or "debug"      
    #'                      
    #' @returns nothing
    #' 
    PrepareProgramWorksheet <- function(wb, progData, progress) {
      s <- GetStyleList()
      
      # add sheet
      # eliminate illegal characters from worksheet name
      # (I should also ignore the backslash character but I don't know how to escape it)
      sheetName <- stringr::str_replace_all(program, "[/\\[\\]:?*]", " ")
      UpdateProgress(progress, paste("Creating worksheet", sheetName))
      programSheet  <- addWorksheet(wb, sheetName)
      showGridLines(wb, programSheet, FALSE)    
      
      startRow <- 5
      currentColumn <- 1

      # title
      writeData(wb = wb, sheet = programSheet, x = program, xy = c(1, 1))
      addStyle(wb = wb, sheet = programSheet, style = s$title$style1, 
               rows = 1, cols = 1)
      setRowHeights(wb = wb, sheet = programSheet, rows = 1, heights = 30)
      
      # date info
      AddReportSpecs(wb, programSheet, weighting)
      
      # check for data for his program on these dates
      if (nrow(progData) == 0) {
        writeData(wb = wb, sheet = programSheet, x = "No data for this program in the specified time period.",
                  xy = c(currentColumn, startRow + 1))
        return()  # exit here
      }
      
      # node column
      writeData(wb = wb, sheet = programSheet, x = nodes$nodeShortnames, 
                xy = c(currentColumn, startRow + 1)) # add 1 to row because there is no Column header row
      addStyle(wb = wb, sheet = programSheet, style = s$rowheader$style, gridExpand = TRUE,
               rows = 0:length(nodes$nodeShortnames) + startRow + 1, cols = currentColumn)
      setColWidths(wb = wb, sheet = programSheet, cols = currentColumn, widths = 70)
      currentColumn <- currentColumn + 1

      # Sample Units data table
      writeData(wb = wb, sheet = programSheet, x = "Sample Units", 
                xy = c(currentColumn, startRow - 1))
      addStyle(wb = wb, sheet = programSheet, style = s$title$style2, gridExpand = TRUE,
               rows = startRow - 1, cols = currentColumn)
      sampleUnits <- dcast(progData[StatisticID == 1], NodeID ~ PeriodID, 
                           value.var = "units", drop = FALSE)[, NodeID := NULL]
      writeData(wb = wb, sheet = programSheet, x = sampleUnits, 
                colNames = TRUE, headerStyle = s$colheader$style,
                borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                xy = c(currentColumn, startRow))
      addStyle(wb = wb, sheet = programSheet, style = s$data$commaStyle0, gridExpand = TRUE,
               rows = 1:nrow(sampleUnits) + startRow , cols = 0:ncol(sampleUnits) + currentColumn,
               stack = TRUE)
      currentColumn <- currentColumn + 1 + ncol(sampleUnits)
      setColWidths(wb = wb, sheet = programSheet, cols = currentColumn - 1, widths = 2)
      
      # Sample Weights data table
      writeData(wb = wb, sheet = programSheet, x = "Sample Weight (kg)", 
                xy = c(currentColumn, startRow - 1))
      addStyle(wb = wb, sheet = programSheet, style = s$title$style2, gridExpand = TRUE,
               rows = startRow - 1, cols = currentColumn)
      sampleWeight <- dcast(progData[StatisticID == 1][, kg := gr / 1000], NodeID ~ PeriodID,
                            value.var = "kg", drop = FALSE)[, NodeID := NULL]
      writeData(wb = wb, sheet = programSheet, x = sampleWeight, 
                borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                colNames = TRUE, headerStyle = s$colheader$style,
                xy = c(currentColumn, startRow))
      addStyle(wb = wb, sheet = programSheet, style = s$data$commaStyle0, gridExpand = TRUE,
               rows = 1:nrow(sampleWeight) + startRow, cols = 0:ncol(sampleWeight) + currentColumn,
               stack = TRUE)
      currentColumn <- currentColumn + 1 + ncol(sampleWeight)
      setColWidths(wb = wb, sheet = programSheet, cols = currentColumn - 1, widths = 2)
      
      
      # Weight Ratio and Unit Weight tables
      # The next two tables interleave mean and CIPct columns.  The code gets a little elaborate to 
      # order, title and format them
      sampleWeightRatio <- dcast(progData[StatisticID == 1], NodeID ~ PeriodID,
                                 value.var = c("mean", "CIPct"), drop = FALSE)[, NodeID := NULL]
      sampleUnitWeight <- dcast(progData[StatisticID == 2][, mean := mean / 1000], NodeID ~ PeriodID,
                                 value.var = c("mean", "CIPct"), drop = FALSE)[, NodeID := NULL]
      
      # some tricky code to reorder columns for these two tables
      meanColnames <- paste("mean", periods$periodLabel, sep =  "_")    # create a list of mean_Q1-2014, mean-Q2_2014, ...
      ciPctColnames <- paste("CIPct", periods$periodLabel, sep =  "_")  # create a list of CIPct_Q1-2014, CIPct-Q2_2014, ...
      sortedColnames <- c(rbind(meanColnames, ciPctColnames))           # Interleaves the lists!!!   mean_Q1-2014, CIPct_Q1-2014, mean_Q2-2014, CIPct_Q2-2014, ...
      setcolorder(sampleWeightRatio, sortedColnames)
      setcolorder(sampleUnitWeight, sortedColnames)
      
      # redo column names - use interleaving again to create a horizontal matrix of Q1-2014,(blank),Q2-2014,(blank), ...
      displayColNames <- as.matrix(c(rbind(periods$periodLabels, rep("", length(periods$periodLabels)))))
      dim(displayColNames) <- c(1, length(periods$periodLabels) * 2)
      
      writeData(wb = wb, sheet = programSheet, x = "Weight Ratio (%, CI%)", 
                xy = c(currentColumn, startRow - 1))
      addStyle(wb = wb, sheet = programSheet, style = s$title$style2, gridExpand = TRUE,
               rows = startRow - 1, cols = currentColumn)
      writeData(wb = wb, sheet = programSheet, x = sampleWeightRatio, 
                borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                colNames = TRUE, headerStyle = s$colheader$style,
                xy = c(currentColumn, startRow))
      writeData(wb = wb, sheet = programSheet, x = displayColNames, colNames = FALSE,
                xy = c(currentColumn, startRow))
      meanCols <- seq(0, ncol(sampleUnitWeight), by = 2) + currentColumn
      setColWidths(wb = wb, sheet = programSheet, cols = meanCols, widths = 10)
      for (col in seq(currentColumn, currentColumn + length(displayColNames) - 1, by = 2)) {
        mergeCells(wb = wb, sheet = programSheet, cols = col:(col + 1), rows = startRow)
      }
      addStyle(wb = wb, sheet = programSheet, style = s$data$percentStyle, gridExpand = TRUE,
               rows = 1:nrow(sampleWeightRatio) + startRow, cols = 0:ncol(sampleWeightRatio) + currentColumn,
               stack = TRUE)
      currentColumn <- currentColumn + 1 + ncol(sampleWeightRatio)
      setColWidths(wb = wb, sheet = programSheet, cols = currentColumn - 1, widths = 2)
      
      writeData(wb = wb, sheet = programSheet, x = "Unit Weight (kg, +/- CI%)", 
                xy = c(currentColumn, startRow - 1))
      addStyle(wb = wb, sheet = programSheet, style = s$title$style2, gridExpand = TRUE,
               rows = startRow - 1, cols = currentColumn)
      writeData(wb = wb, sheet = programSheet, x = sampleUnitWeight, 
                colNames = TRUE, headerStyle = s$colheader$style,
                borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                xy = c(currentColumn, startRow))
      writeData(wb = wb, sheet = programSheet, x = displayColNames, colNames = FALSE,
                xy = c(currentColumn, startRow))
      meanCols <- seq(0, ncol(sampleUnitWeight), by = 2) + currentColumn
      setColWidths(wb = wb, sheet = programSheet, cols = meanCols, widths = 10)
      for (col in seq(currentColumn, currentColumn + length(displayColNames) - 1, by = 2)) {
        mergeCells(wb = wb, sheet = programSheet, cols = col:(col + 1), rows = startRow)
      }
      
      # style for unitWt means
      addStyle(wb = wb, sheet = programSheet, style = s$data$commaStyle3, gridExpand = TRUE,
               rows = 1:nrow(sampleUnitWeight) + startRow, cols = meanCols,
               stack = TRUE)
      setColWidths(wb = wb, sheet = programSheet, cols = meanCols, widths = 10)

      # style for unitWt CIPct
      addStyle(wb = wb, sheet = programSheet, style = s$data$percentStyle, gridExpand = TRUE,
               rows = 1:nrow(sampleUnitWeight) + startRow, cols = meanCols + 1,
               stack = TRUE)
      currentColumn <- currentColumn + 1 + ncol(sampleUnitWeight)
      
      # style hierarchy rows (max depth of 5)
      for (h in 1:5) {
        rows <- which(nodes$nodeDepths==h) + startRow  # array of row numbers with this hierarchy level
        addStyle(wb = wb, sheet = programSheet, style = s$hierarchy$rowstyle[[h]], 
                 rows = rows, cols = 1:currentColumn, 
                 gridExpand = TRUE, stack = TRUE)
        indentStyle <- createStyle(indent = s$hierarchy$indentincrement * (h - 1))
        addStyle(wb = wb, sheet = programSheet, style = indentStyle, 
                 rows = rows, cols = 1,
                 gridExpand = TRUE, stack = TRUE)
        if (!is.na(s$hierarchy$rowheight[[h]])) {
          setRowHeights(wb = wb, sheet = programSheet, rows = rows, heights = s$hierarchy$rowheight[[h]])
        }
      }
      
      # freeze the panes for a pleasant scrolling experience
      freezePane(wb = wb, sheet = programSheet, 
                 firstActiveRow = startRow + 1,
                 firstActiveCol = 2)
    }
    
    #' @name PrepareSingleStatWorksheet
    #' 
    #' @description 
    #' Manages creation of of a single statistic style worksheet
    #' 
    #' @param 
    #' wb                   workbook
    #' sheetName            the name to give the new sheet
    #' statData             the statistics data to be added to the new worksheet
    #' dataStyle            the style used to format the type of statistic used in this sheet
    #' progress             a call-back function of type shiny::Progress to provide progress reports for the UI. 
    #'                      Can also be NULL (default) or "debug"      
    #'                      
    #' @returns nothing
    PrepareSingleStatWorksheet <- function(wb, sheetName, statData, 
                                           dataStyle, heading, weighting, progress) {
      
      #' @name AddTable
      #' 
      #' @description 
      #' Adds a table of data for a single program to the sheet
      #' 
      #' @param 
      #' wb                   workbook
      #' sheet                worksheet
      #' startRow             the row on which to start the table
      #' progIndex            the number of the program whose table is being printed
      #' data                 the data table to be added
      #' dataStyle            the style used to format the type of statistic used in this sheet
      #'                      
      #' @returns nothing
      AddTable <- function(wb, sheet, startRow, progIndex, data, dataStyle) {
        s <- GetStyleList()
        startColumn <- 1
        
        # write the data and column headers and format        
        writeData(wb = wb, sheet = sheet, x = data,
                  borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                  colNames = TRUE, headerStyle = s$colheader$style,
                  xy = c(startColumn, startRow))
        addStyle(wb = wb, sheet = sheet, style = dataStyle, gridExpand = TRUE,
                 rows = 1:nrow(data) + startRow, cols = 0:ncol(data) + startColumn)

        # overwrite the top left corner with the program name and overwrite format
        writeData(wb = wb, sheet = sheet, x = programs$programLabel[[progIndex]],
                  borders = s$data$borders, borderStyle = s$data$borderStyle, borderColour = s$data$borderColour,
                  colNames = FALSE, xy = c(startColumn, startRow))
        addStyle(wb = wb, sheet = sheet, style = s$title$style2, 
                 rows = startRow, cols = startColumn)
        
        # widen the row header column
        setColWidths(wb = wb, sheet = sheet, cols = startColumn, widths = 30)
        
        nextRow <- startRow + nrow(data) + 1
        setRowHeights(wb, sheet, nextRow, 60)
        return(nextRow + 1)
      }
      
      # PrepareSingleStatWorksheet main entry point -------------------------------------------
      s <- GetStyleList()
      
      # add sheet
      UpdateProgress(progress, paste("Creating worksheet", sheetName))
      statSheet  <- addWorksheet(wb, sheetName)
      showGridLines(wb, statSheet, FALSE)    
      
      # add title
      writeData(wb = wb, sheet = statSheet, x = heading, xy = c(2, 1))
      addStyle(wb = wb, sheet = statSheet, style = s$title$style3, 
               rows = 1, cols = 2)
      mergeCells(wb = wb, sheet = statSheet, cols = 1:length(periods$periodLabel) + 1, rows = 1)
      setRowHeights(wb = wb, sheet = statSheet, rows = 1, heights = 30)
      
      # add date info
      AddReportSpecs(wb, statSheet, weighting)
      
      # add program tables
      currentRow <- 6 # first table appears at this row
      for (progIndex in 1:length(programs$programLabel)) {
        data <- statData[ProgramID == programs$programLabel[[progIndex]]][, ':=' (ProgramID = NULL, NodeID = NULL)]
        currentRow <- AddTable(wb = wb, sheet = statSheet, startRow = currentRow, progIndex = progIndex, 
                               data = data, dataStyle = dataStyle) 
      }
    }
    
    # CreateWorkbook main entry point ------------------------------------
    library("openxlsx")
    
    # Create workbook
    wb <- createWorkbook() 
    
    # Add Weight Ratio worksheet
    # Applies to all Depth 2 nodes
    wrData <- data[StatisticID == 1 & depth == 2, .(ProgramID, PeriodID, NodeID, shortname, ratio = mean)]
    if (nrow(wrData) > 0) {
      wrData <- dcast(wrData, ProgramID + NodeID + shortname ~ PeriodID, value.var = "ratio")
      PrepareSingleStatWorksheet(wb = wb, sheetName = "Weight Ratio", 
                                 statData = wrData, dataStyle = GetStyleList()$data$percentStyle,
                                 heading = "Weight Ratios",
                                 weighting = weighting,
                                 progress = progress)
    }
    
    # Add Unit Weight worksheet
    # hard coding ID for Master Hierarchy, Display Devices and Computers
    uwNodeLabels <- nodes$nodeLabels[which(nodes$nodeLevels %in% c(1000, 12000, 12100))]
    uwData <- data[StatisticID == 2 & NodeID %in% uwNodeLabels, .(ProgramID, PeriodID, NodeID, shortname, kg = mean / 1000)]
    if(nrow(uwData > 0 )) {
      uwData <- dcast(uwData, ProgramID + NodeID + shortname ~ PeriodID, value.var = "kg")
      PrepareSingleStatWorksheet(wb = wb, sheetName = "Unit Weight",
                                 statData = uwData, dataStyle = GetStyleList()$data$commaStyle3,
                                 heading = "Weight Per Unit (kg)", 
                                 weighting = weighting,
                                 progress = progress)
    }
    
    # Add Display Device worksheet
    # hard coding display devices 
    ddNodeLevels <- GetNodesIDLL(12000,1)$nodeLevels
    ddNodeLabels <- nodes$nodeLabels[which(nodes$nodeLevels %in% ddNodeLevels)]
    ddData <- data[StatisticID == 1 & NodeID %in% ddNodeLabels, .(ProgramID, PeriodID, NodeID, shortname, ratio = mean)]
    if (nrow(ddData) > 0) {
      ddData <- dcast(ddData, ProgramID + NodeID + shortname ~ PeriodID, value.var = "ratio")
      PrepareSingleStatWorksheet(wb = wb, sheetName = "Display Devices",
                                 statData = ddData, dataStyle = GetStyleList()$data$percentStyle,
                                 heading = "Display Device Weight Ratios",
                                 weighting = weighting,
                                 progress = progress)
    }
    
    # Add program worksheets
    for (program in programs$programLabels) {
      PrepareProgramWorksheet(wb, data[ProgramID == program], progress = progress)
    }

    # Save the work book
    UpdateProgress(progress, paste("Saving workbook", filename))
    saveWorkbook(wb, filename, TRUE)
  }
  
  # main entry point ------------------------------------
  
  # hard coded report parameters
  rootNode <- 1000        # Master hierarchy root node
  depth <- 2              # all nodes 2 levels down
  progList <- as.vector(GeoSelectionList(F, T, F))
  statList <- c(1, 2)     # wt ratio and unit wt
  includeSummary <- TRUE  # program and summary as well
  
  # nodes, programs and periods will be visible to and used by
  # all functions declared within the current function
  nodes <- GetNodesIDLL(rootNode, depth = depth, skip = FALSE)
  programs <- GetProgramsIDLL(progList, includeSummary)
  periods <- GetPeriodsIDLL(timeParms)
  
  data <- GetData(statList, weighting, progress = progress)

  CreateWorkbook(filename = filename, data = data, 
                 weighting = weighting, progress = progress)
}