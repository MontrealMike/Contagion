
source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

drTest <- function(){
  nodeSelection <- "1000"
  level <- 1
  geoSelection  = "P3"
  timeSelection = "Y2018"
  weighting = "forecast"
  # includeOESFlag = FALSE
  dr <- GetDetailReport(nodeSelection, level, geoSelection, timeSelection, weighting)
  return(dr)
}

GetDetailReport <- function(nodeSelection, level, geoSelection, timeSelection, weighting) {
  
  # main entry point ---------------------------------------------------------
  
  #   node parameters
  nodeTable <- NodeSelection(nodeSelection, GetDataObject("node"), depth = level, skip = FALSE)
  nodeList = nodeTable[, NodeID]
  
  #   strata parameters
  strata <- as.data.table(StratumSelection(geoSelection, timeSelection))
  
  if (length(strata$strataID) == 0 ) {
    return(NULL)
  }
  msl <- strata[ , .(MultiStratumHash = "", StratumID = strataID)]
  
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
  contents <- contents[nodeTable[, .(NodeID, name = shortname, level, sortOrder)]]
  
  # Statistic columns - convert the statisticID variable to columns
  meanCols <- reshape(reportData[StatisticID %in% statsList, .(NodeID, StatisticID, meanCol = mean)],
                      idvar = "NodeID", timevar = "StatisticID", direction = "wide")
  setkey(meanCols, NodeID)
  contents <- merge(contents, meanCols, by = "NodeID")

  # row and column ordering
  contents <- contents[order(sortOrder)]
  colNames <- c("name", "units", "gr", paste0("meanCol.", statsList), "level")
  contents <- contents[ , ..colNames]
  
  # Prepare report columns specs
  varNames <- c(colNames[1:3], rep("mean", times = length(statsList)), "level")
  statisticID <- c(NA, 1, 1, statsList, NA)
  colDisplayNames <- DTGetDisplayColumns(statisticID = statisticID,
                                         variables = varNames)
  colDisplayNames[1] <- "Product"
  
  # set the caption for the report
  caption = DTGetReportCaption(mainTitle = "Detail Report",  
                               geoSelection = geoSelection, 
                               timeSelection = timeSelection,
                               weighting = weighting)
  
  # format the data into a datatable for use in Shiny
  library(DT)
  dt <- datatable(contents,
                  colname =  colDisplayNames,
                  style = "default",
                  rownames = FALSE,
                  escape = TRUE,
                  caption = caption,
                  extensions = 'Buttons',  #added by Andrew  Dec 15 2016 to support download buttons
                  options = list(dom = 'Bt', #change from 't' to 'bt' by Andrew  Dec 15 2016 to support download buttons
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), #added by Andrew  Dec 15 2016 to support download buttons
                                 pageLength = -1L,
                                 columnDefs = list(list(visible = FALSE, targets = ncol(contents) - 1)) # hide the level column
                  )
  ) %>%
    formatStyle(
      columns = 'level',
      target = "row",
      fontWeight = styleEqual(c(1, 2, 3, 4, 5), c("bold", "normal", "normal", "normal", "normal")),
      fontFamily = "Helvetica, Arial, Sans-serif",
      fontSize = styleEqual(c(1, 2, 3, 4, 5), c("120%", "120%", "100%", "80%", "60%"))  
    )

  dt <- DTPostProcess(shinyTable = dt,
                      statisticID = statisticID,
                      columnVars = varNames)
  
  return(dt)
}
