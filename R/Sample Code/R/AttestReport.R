# test data table functions

# rm(list=ls())
library(DT)
source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")
source("R/PrepareMultistratStats.R")

atTest <- function(){
  nodeSelection <- "1000"
  level <- 1
  geoSelection  = "C"
  timeSelection = "Y2016"
  weighting = "forecast"
  # includeOESFlag = FALSE
  dt <- GetAttestReport(nodeSelection, level, geoSelection, timeSelection, weighting)
  return(dt)
}

GetAttestReport <- function(nodeSelection, level, geoSelection, timeSelection, weighting) {
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
  statsList <- c(1, 2)
  
  # get the data
  reportData <- MultiStratumTable(nodeSelectionTable = nodeList,
                                  stratumSelectionTable = msl, 
                                  statisticList = statsList,
                                  weighting = weighting)
  
  # shape and convert the data
  setkey(nodeTable, NodeID)
  setkey(reportData, NodeID)
  
  # Initial columns
  contents <- reportData[StatisticID == 1, .(NodeID)]
  setkey(contents, NodeID)
  contents <- contents[nodeTable[, .(NodeID, name = shortname, level, sortOrder)]]
  
  # Statistic columns - convert the statisticID variable to columns mean first
  meanCols <- reshape(reportData[StatisticID %in% statsList, .(NodeID, StatisticID, meanCol = mean)],
                      idvar = "NodeID", timevar = "StatisticID", direction = "wide")
  contents <- merge(contents, meanCols, by = "NodeID")
  
  # Statistic columns - convert the statisticID variable to columns CI second
  ciCols <- reshape(reportData[StatisticID %in% statsList, .(NodeID, StatisticID, ciCol = CIPct)],
                      idvar = "NodeID", timevar = "StatisticID", direction = "wide")
  contents <- merge(contents, ciCols, by = "NodeID")
  

  # sort rows
  contents <- contents[order(sortOrder)]
  
  # select and order columns and get display column names
  colNames <- c("name", "meanCol.1", "ciCol.1", "meanCol.2", "ciCol.2", "level")
  contents <- contents[ , ..colNames]
  varNames <- c("", "mean", "CIPct", "mean", "CIPct", "")
  statisticID <- c(NA, 1, 1, 2, 2, NA)
  displayColumnNames <- DTGetDisplayColumns(statisticID = statisticID,
                                            variables = varNames)
  displayColumnNames[1] <- "Product"

  # set the caption for the report
  caption = DTGetReportCaption(mainTitle = "Attest Report",  
                               geoSelection = geoSelection, 
                               timeSelection = timeSelection,
                               weighting = weighting)
  
  
  # format the data into a datatable for use in Shiny
  dt <- datatable(contents,
                  colname =  displayColumnNames,
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
