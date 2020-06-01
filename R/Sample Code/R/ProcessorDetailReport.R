#' @name ProcessorDetailReport.R
#' @author Michael Bleau
#' 
#' 
#' @description prepares a Shiny Report for a specific processor
#' 2017-06-20
#' 
#' Modified 2017-10-23 (MRB)
#'  - fixed bug where no data for a processor causes error messages
#' 

source("R/ShinyHelperFunctions.R")
source("R/UtilityFunctions.R")
source("R/PrepareMultistratStats.R")

# test Processor Detail report
pdTest <- function() {
  nodeSelection <- "1000"
  level <- -1
  geoSelection  <-  "C0"
  timeSelection <-  "Y2016"
  processorSelection <- 1
  GetProcessorDetailReport(processorSelection, nodeSelection, level, geoSelection, timeSelection)
}


#' @name GetProcessorDetailReport
#' 
#' @return A datatable (meant for display in Shiny) presenting sampling statistics of a specific sampling location (aka processor)
#' 
#' @param 
#' processorSelection: the ID of a sampling location
#' nodeSelection:      a the parent node in the product hierarchy
#' level:              the number of levels down from the parent node to include in the report (-1 means include all nodes)
#' geoSelection:       a string encoded to select a group of subprograms (see the GeoSelection function in Utilities.R for coding info)
#' timeSelection:      a string encoded to select a group of quarters (see the TimeSelection function in  Utilities.R for coding info).
#' 
#' @description 
#' The data table contains the following columns: Product category, Units, Kg, Wt%, Kg/Unit, Damage%, Cannib%, Avg age (y), Avg screen size (cm)
#' Each row in the report is a descendant of the nodeSelection parameter
#' Rows are sorted by the sort column in the node table
#' The report presents the statistics generated for the geoSelection and timeSelection specified in the input parameters.
#' All statistics calculations are unweighted
GetProcessorDetailReport <- function(processorSelection, 
                                     nodeSelection,
                                     level, 
                                     geoSelection, 
                                     timeSelection,
                                     weighting = "unweighted") {

  #   node parameters
  nodeTable <- NodeSelection(nodeSelection, GetDataObject("node"), depth = level, skip = FALSE)
  nodeList = nodeTable[, NodeID]

  strata <- as.data.table(StratumSelection(geoSelection, timeSelection))
  if (length(strata$strataID) == 0 ) {
    return(NULL) # no strata have been created for this geography time combination
  }
  msl <- strata[ , .(MultiStratumHash = "", StratumID = strataID)]
  statsList <- c(1, 2, 6, 7, 3, 8) # ordered list of needed stats
  
  stats <- MultiStratumTable(nodeSelectionTable = nodeList,
                             stratumSelectionTable = msl, 
                             weighting = weighting,
                             statisticList = statsList,
                             locationID = TRUE)[SamplingLocationID == processorSelection]
  
  if (nrow(stats) == 0) {
    return(NULL)  # no data for this processor location
  }
    
  # keep only needed columns and rows (means)
  neededCols <- c("NodeID", "StatisticID", "units", "gr", "mean")
  stats <- stats[ , ..neededCols]
  
  # Initial columns (use weight ratio statistic for units and wt totals)
  contents <- stats[StatisticID == 1, 
                    .(NodeID, units, gr)]
  
  # Statistic columns - convert the statisticID variable to columns and attach to contents
  meanCols <- reshape(stats[, .(NodeID, StatisticID, meanCol = mean)],
                      idvar = "NodeID", timevar = "StatisticID", direction = "wide")
  contents <- merge(contents, meanCols, by = "NodeID")
  
  # Attach node data and sort
  contents <- merge(contents, nodeTable[ ,.(NodeID, name = shortname, level, sortOrder)], by = "NodeID")[order(sortOrder)]
  
  # select and sort table columns
  colNames <- c("name", "units", "gr", paste0("meanCol.", statsList), "level")
  contents <- contents[ ,..colNames]
  
  # get display column names
  displayParameters <- DTImportDisplayParameters()
  statisticID <- c(NA, 1, 1, statsList, NA)
  varNames <- c("", "units", "gr", rep("mean", times = length(statsList)), "")
  displayColumnNames <- DTGetDisplayColumns(statisticID = statisticID,
                                            variables = varNames,
                                            displayParameters = displayParameters)
  displayColumnNames[1] <- "Product"
  
  caption <- DTGetReportCaption(mainTitle = "Processor Product Report",
                               nodeSelection = nodeSelection,
                               geoSelection = geoSelection,
                               timeSelection = timeSelection,
                               processorID = processorSelection,
                               weighting = weighting
                               )
  
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
  )  %>%
    formatStyle(
      columns = 'level',
      target = "row",
      fontWeight = styleEqual(c(1, 2, 3, 4, 5), c("bold", "normal", "normal", "normal", "normal")),
      fontFamily = "Helvetica, Arial, Sans-serif",
      fontSize = styleEqual(c(1, 2, 3, 4, 5), c("120%", "120%", "100%", "80%", "60%"))  
    )
  dt <- DTPostProcess(shinyTable = dt,
                      statisticID = statisticID,
                      columnVars = varNames,
                      displayParameters = displayParameters)
  return(dt)
}