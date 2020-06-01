# EPRASampling
#
# Michael Bleau
# 2016-03-02
#
# Functions to generate Program Comparison report
#
# The report rows are Nodes selected depending on the Hierarchy and level values
# The columns are programs (the last column is all programs)
# The values depend on the Statistic, Period and Weighting selections
#
# 2018-3-10 MRB
#  fixed bug causing crashes when no strata are found for the date/program selected

#' @section Setup
source("R/ShinyHelperFunctions.R")
source("R/PrepareMultistratStats.R")

#'  @section Test functions
pcrTest <- function(){
  nodeID <- 1000
  timeSelection <- "Y2018"
  statID <- 2  #wt ratio change
  variableName = "CIPct"
  weightingType = "forecast"
  level <- 2;
  includeOES = FALSE
  rpt <- GetProgramComparisonReport(nodeID = nodeID, 
                                    timeSelection = timeSelection, 
                                    weighting = weightingType,
                                    statisticID = statID,
                                    variableName = variableName,
                                    level = level,
                                    includeOES = includeOES
  )
  return(rpt)
}


#' @name GetProgramComparisonReport
#' 
#' @desc return a line plot of user selected statistic and variables
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
#' 

GetProgramComparisonReport <- function(nodeID, timeSelection, weighting, statisticID, 
                                       variableName = "mean", level, includeOES ) {
  
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
  
  # Main entry point ------------------------------------------------------------------------------------
  programs <- unique(GetDataObject("subprogram")[, .(ProgramID = as.character(ProgramID), ProgramShortname, ProgSortOrder)])[order(ProgSortOrder)]
  if (!includeOES) {
    programs <- programs[ProgramID != 3]
  }
  
  stratumSelection <- GetMultiStratumSelectionTable(programs[,ProgramID], timeSelection)
  # if we have no strata, bail and return NULL
  if (is.null(stratumSelection)) {
    return(NULL)
  }
  
  nodeTable <- NodeSelection(nodeID, GetDataObject("node"), depth = level, skip = FALSE)
  nodeList = nodeTable[, NodeID]
  
  # get the needed strata - and pick out the selected variable
  msData <- MultiStratumTable(nodeSelectionTable = nodeList,
                              stratumSelection = stratumSelection, 
                              statisticList = statisticID,
                              weighting = weighting)[, value := .SD[[variableName]], .SDcols = variableName][ , .(NodeID, ProgramID = MultiStratumHash, value)]
  
  # add records for even those node programs combos without data
  # programs <- unique(GetDataObject("subprogram")[, .(ProgramID = as.character(ProgramID), ProgramShortname, ProgSortOrder)])[order(ProgSortOrder)]
  progNode <- Cartesian.dt(programs[ , .(ProgramID)], nodeTable[ , .(NodeID)]) # All program Nodes
  progNode <- rbind(progNode, nodeTable [ ,. (NodeID, ProgramID = "Sum")])     # Add nodes for summary column
  msData <- merge(msData, progNode,  by = c("ProgramID", "NodeID"), all.x = TRUE, all.y = TRUE) # this is like an outer join
    
  # reshape programs into columns
  msData <- reshape(msData, 
                    idvar = "NodeID", 
                    timevar = "ProgramID", 
                    direction = "wide")
  
  # attach node descriptive columns and sort
  msData <- merge(msData, nodeTable[, .(NodeID, nodeName = shortname, level, sortOrder)], by = "NodeID")[order(sortOrder)]
  
  # columns names and sort order
  displayParameters <- DTImportDisplayParameters() #it is mildly more efficient to get them once and feed them to DT functions
  colNames <- c("nodeName", paste0("value.", programs[ , ProgramID]), "value.Sum", "level")
  msData <- msData[ , ..colNames]
  
  # customize display column names since this is all about programs
  displayColumnNames <- c("Product", programs[ , ProgramShortname], "Summary", "level")
  
  # report gaption
  caption <- DTGetReportCaption(mainTitle = "Program Comparison Report",
                                timeSelection = timeSelection, 
                                weighting = weighting,
                                statisticID = statisticID,
                                variableName = variableName)
  
  dt <- datatable(msData,
                  colname =  displayColumnNames,
                  style = "default",
                  rownames = FALSE,
                  escape = TRUE,
                  caption = caption,
                  extensions = 'Buttons',  #added by Andrew  Dec 15 2016 to support download buttons
                  options = list(dom = 'Bt', #change from 't' to 'bt' by Andrew  Dec 15 2016 to support download buttons
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), #added by Andrew  Dec 15 2016 to support download buttons
                                 pageLength = -1L,
                                 columnDefs = list(list(visible = FALSE, targets = ncol(msData) - 1)) # hide the level column
                  )
  ) %>% formatStyle(columns = 'level',
                    target = "row",
                    fontWeight = styleEqual(c(1, 2, 3, 4, 5), c("bold", "normal", "normal", "normal", "normal")),
                    fontFamily = "Helvetica, Arial, Sans-serif",
                    fontSize = styleEqual(c(1, 2, 3, 4, 5), c("120%", "120%", "100%", "80%", "60%"))
  )
  
  statisticID <- c(NA, rep(statisticID, nrow(programs) + 1), NA)
  variableNames <- rep(variableName, times = length(statisticID))
  dt <- DTPostProcess(shinyTable = dt,
                      statisticID = statisticID,
                      columnVars = variableNames,
                      displayParameters = displayParameters)
  
  return(dt)
}
