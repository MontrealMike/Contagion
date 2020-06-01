#' @name ShinyHelperFunctions.R
#' 
#' @author Michael Bleau
#'        2017-12-08
#' 
#' @description 
#' Contains functions for use by the shiny interface
#' Includes:
#'    - Preparation of lists for drop down menus
#'    - Interpretation of drop down list selections
#'    - Formatting and ordering for shiny tables and charts
#' Also includes formatting and ordering for PDF tables and charts
#' 
#' @details
#' function names follow the following naming conventions
#'    Ending in List:      Function generates values for a drop down list
#'                         The list contents are what are returned by the function
#'                         The name of each list element is what appears in the list
#'    Ending in Selection: Converts values returned in a list selection into values
#'                         useful for retrieving data
#'    Starting with DT:    Shiny data table formatting functions
#'    Ending in IDLL:      IDLL = ID Leveled List.  Produce lists of data suitable for retrieving and displaying
#'                         category data with standardized descriptors and in a standardized order.
#'
#' @changes
#'   2018-08-14 (MRB)
#'   - added data effective date to report captions
#'   2018-08-17 (MRB)
#'   - added Canada (except OES) option to GeoSelectionList and GeoSelection functions as well as to DTGetReport Caption
#'   2018-08-20 (MRB)
#'   - added accuracy parameter to output of GetDisplayParameters function
#'   2018-09-05 (MRB)
#'   - moved levelled list functions from ProgramComparisonChartTable.R and made them more generic
#'   2018-09-10 (MRB)
#'   - added ContainerVariableSelectionList function for container analysis report
#'   2018-10-24 (MRB)
#'   - added features to enable reporting by half.  Changes/additions made to all 4 function types
#'     (Selection, List, DT, IDLL)
#'   2018-10-29 (MRB)
#'   - added BrandSelectionList function
#'   2019-01-17 (MRB)
#'   - corrected some documentation
#'   - fixed bug in GetPeriodsIDLL causing only last quarter in half to contribute to Half Results
#'   - expanded scope of file contents to help non-shiny reporting (i.e Huxtable tables)
#'   2019-01-27 (MRB)
#'    - added depth parameter and skip to GetNodesIDLL
#'    - added nodeDepths to output of GetNodesIDLL
#'    - added shortnames to output of GetNodeIDLL
#'   2019-05-30 (MRB)
#'    - corrected bug in GetPeriodsIDLL that caused halves to be double counted

library(DT, quietly = TRUE)
source("R/UtilityFunctions.R")

#' @section data selection
#' 
#' @name GeoSelection
#' 
#' @return A 2 element list describing a subprogram or group of subprograms
#'         1. desc - text description of the subprogram or group
#'         2. subprograms - a vector of the SubprogramIDs in the group
#' 
#' @param 
#'         geoID - a text string describing the group to be returned
#'                 the first character (prefix) identifies the type of group. It is either C, P or S
#'                 the remaining characters (ID) form an integer identifying the specific group 
#'                 
#'         ignoreOESFlag - default = TRUE - if True, do not include any subprograms belonging to the Ontario Electronics 
#'                         Stewardship program.  Only apples if the geoID is C
#'                 
#'                 
#' @description 
#'         this function is designed to convert user selections from a Shiny drop down list 
#'         into a list of subprograms
#'                         
#'         if the prefix of geoID is C (for Canada) - if ID = 0, all sub programs are returned, 
#'                                                    if ID = 1, all subprograms except those belong to OES program are returned
#'                                   P - all the subprograms having the ProgramID == ID are returned
#'                                   S - one subprogram with SubprogramID == ID is returned
#'                                   
#'         +-------------------------------------------------------------------------------------------------------------------+
#'         | CAUTION - the Ontario Stewardship Program (OES) is identified by its ID of 3.                                     |
#'         |          This is hard coded!                                                                                      |
#'         +-------------------------------------------------------------------------------------------------------------------+
#'         


#  Selection Section ---------------------------------------------------------------------- 
GeoSelection <- function(geoID){
  s <- GetDataObject("subprogram")
  p  <- unique(s[,.(ProgramID, ProgramName)])
  geoType <- substring(geoID, 1, 1)
  geoIDInt <- as.integer(substring(geoID, 2))
  geoList <- switch(geoType,
                    "C" = switch(as.character(geoIDInt), 
                                 "0" = list(desc = "Canada", subprograms = s[,SubprogramID]),
                                 "1" = list(desc = "Canada", subprograms = s[ProgramID != 3, SubprogramID])
                    ),
                    "P" = list(desc = p[ProgramID == geoIDInt, ProgramName],
                               subprograms = s[ProgramID == geoIDInt, SubprogramID]),
                    "S" = list(desc = s[SubprogramID == geoIDInt, FullName],
                               subprograms = s[SubprogramID == geoIDInt, SubprogramID])
  )
  return(geoList)
}

#' @name TimeSelection
#' 
#' @return A 2 element list describing a time period of a quarter, a half or a year
#'         1. desc - text description of the time period
#'         2. quarters - a vector of the QuarterIDs in the period
#' 
#' @param 
#'         timeID - a 2 part text string describing the time period to be returned
#'                  the first part (prefix) defines the type of time period. It must be Y, H1, H2, or Q
#'                  the remaining characters (ID) form an integer identifying the specific time period
#'                  the separator of the two parts is the "-" hyphen
#'                 
#' @description 
#'         this function is designed to convert user selections from a Shiny drop down list 
#'         into a list of quarters
#'                         
#'         if the prefix of timeID is Y - all quarters in year with value ID are returned
#'                                    H1 - all quarters in the first half in year of value ID are returned
#'                                    H2 - all quarters in the second half in year of value ID are returned
#'                                    Q - the quarter with QuarterID == ID is returned
TimeSelection <- function(timeID){
  q <- GetDataObject("quarter")
  timeSelectionParms <- strsplit(timeID, "-")
  timeType <- timeSelectionParms[[1]][[1]]
  timeIDInt <- timeSelectionParms[[1]][[2]]
  ts <- switch(timeType,
         "Y" = list(desc = as.character(timeID),
                    quarters = q[Year == timeIDInt, QuarterID]),
         "H1" = list(desc = paste0("H1-", timeIDInt), 
                     quarters = q[Year == timeIDInt & HalfValue == 1, QuarterID]), 
         "H2" = list(desc = paste0("H2-", timeIDInt), 
                     quarters = q[Year == timeIDInt & HalfValue == 2, QuarterID]), 
         "Q" = list(desc = q[QuarterID == timeIDInt, PeriodName],
                    quarters = q[QuarterID == timeIDInt, QuarterID])
  )
  return(ts)
}

#' @name StratumSelection
#' 
#' @return A 2 element list describing a stratum group of strata
#'         1. desc - text description of the selected strata
#'         2. strata - a vector of the StratumIDs in the group
#' 
#' @param 
#'         geoID - a text string describing the group to be returned
#'                 the first character (prefix) identifies the type of group. It is either C, P or S
#'                 the remaining characters (ID) form an integer identifying the specific group
#'         timeID - a text string describing the time period to be returned
#'                  the first character (prefix) defines the type of time period. It must be Y or Q.
#'                  the remaining characters (ID) form an integer identifying the specific time period
#'                 
#' @description 
#'         this function is designed to convert user selections from a Shiny drop down list 
#'         into a list of strata
#'                         
#'         See the GeoSelection function for an explanation of the geoID parameter
#'         See the TimeSelection function for an explanation of the timeID parameter
#'
StratumSelection <- function(geoID, timeID) {
  t <- TimeSelection(timeID)
  g <- GeoSelection(geoID)
  s <- GetDataObject("stratum")[QuarterID %in% t$quarters & SubprogramID %in% g$subprograms, StratumID]
  desc <- paste0(g$desc, " ", t$desc)
  return(list(desc = desc, strataID = s))
}

#'  recursively expand a list of nodes with root = nodeID
#' to a maximum depth of depth
#' if depth is negative all levels are returned
#' a global sort order field is created with format 0000.0000.---
#'   where there is a 0000 section for each level and each section is the rank within the level
NodeSelection <- function(nodeID, 
                          node = NULL, 
                          depth = -1, 
                          level = 1, 
                          sortOrder = "", 
                          name = "", 
                          skip = FALSE,
                          leaf = TRUE)
{
  depth = as.integer(depth)
  nodeID <- as.integer(nodeID)
  if (is.null(node)) {
    node <- GetDataObject("node")
  }
  children <- node[ParentnodeID == nodeID, NodeID]
  if (length(children) == 0 & leaf == FALSE) {
    return(data.table(NodeID = integer(), sortOrder = character(), level = integer(), name = character(), shortname = character()))
  }
  
  if (skip) {
    nodeSortOrder <-  ""
    nodeName <-  ""
    nodeList <- data.table(NodeID = integer(), sortOrder = character(), level = integer(), name = character(), shortname = character())
  } else {
    nodeSortOrder <- formatC(node[NodeID == nodeID, SortOrder], width=4, flag="0")
    if (sortOrder != "") {
      nodeSortOrder <- paste(sortOrder, nodeSortOrder, sep = ".")
    }
    
    nodeShortName <- node[NodeID == nodeID, NodeName]
    if (name != ""){
      nodeName = paste(name, nodeShortName, sep = "/")
    } else {
      nodeName <- nodeShortName
    }
    
    nodeList <- data.table(NodeID = nodeID, sortOrder = nodeSortOrder, level = level, name = nodeName, shortname = nodeShortName)
  }
  
  if (depth != 0) {
    children <- node[ParentnodeID == nodeID, NodeID]
    if (length(children) > 0) {
      for (i in 1:(length(children))) {
        nodeList <- rbind(nodeList, NodeSelection(nodeID = children[i], 
                                                  node = node, 
                                                  depth = depth - 1, 
                                                  level = level + 1, 
                                                  sortOrder = nodeSortOrder, 
                                                  name = nodeName,
                                                  leaf = leaf))
      }
    }
  }
  return(nodeList)
}

#  List Section ---------------------------------------------------------------------- 
#' @name StratumList
#' 
#' @description 
#' get a list of Stratum ID's for use in a shiny drop down list
#' each item is given a descriptive name which is what appears in the drop down
#' only strata with non-NA datalines are returned
#' List is ordered chronologibally (quarter) and then by program sort order
#' 
StrataList <- function() {
  strata <- GetDataObject("stratum")[order(PeriodStartDate, ProgSortOrder, SubprogramID)][!is.na(dataLines)]
  sl <- strata[ , StratumID]  # creates a list of stratumIDs
  names(sl) <- strata[ , paste(PeriodName, ProgramShortname, SubprogramShortname)]
  return(sl)
}

#' Geo Selection list
#' returns a list of items for a Shiny drop down box
#' each item has an appropriate name which is what appears in the box
#' each as a unique ID which is what is returned when the user selects that item
#'  - if a subprogram is selected - the id is the letter "S" followed by the subprogramID (e.g. S17)
#'  - if a program is selected - the id is the letter "P" followed by the programID (e.g. P3)
#'  - if Canada is selected - the id is "C0"
#'  - if Canada except OES is selected - the id is "C1"
#' 
#' modified 2017-11-17 (MRB)
#'   - allow restricting selection according to level (subprogram, program and/or canada)
#'   - mods do not change default behaviour
#'   - mods do not require changing calling function
#'   
#' modified 2018-08-20 (MRB)
#'   - added Canada (except OES) to the options list and placed in the 1st position
#' modified 2018-10-12 (MRB)
#'   - removed Canada (except OES) to the options list and placed in the 1st position
GeoSelectionList <- function(includeSubs = FALSE, includeProgs = TRUE, includeCan = TRUE){
  subs <- GetDataObject("subprogram")
  geo <- data.table(ID = character(), Name = character(), SortOrder = character())
  if (includeSubs) {
    geo <- rbind(geo, 
                 subs[,.(ID = paste("S", as.character(SubprogramID), sep=""), 
                         Name = FullName, 
                         SortOrder = paste(as.character(ProgSortOrder), SubprogramShortname, sep = ""))]
    )
  }
  if (includeProgs){
    
    geo <- rbind(geo, 
                 unique(subs[,.(ID = paste("P", as.character(ProgramID), sep=""), 
                                Name = ProgramShortname, 
                                SortOrder = as.character(ProgSortOrder))])
    )
  }
  geo <- geo[order(SortOrder)]  # sorts subprograms within programs
  geoList <- geo[, ID]   # creates a list of unique IDs
  names(geoList) <- geo[, Name] # gives each item a name (which appears in the drop down list)
  if (includeCan) {
    geoList <- c(c( "Canada" = "C0"), geoList)  #puts Canada at the second on the list
    # geoList <- c(c( "Canada (except OES)" = "C1"), geoList) #puts Canada ex Ontario first on the list (removed 2018-10-12)
  }
  return(geoList)
}

WeightingSelectionList <- function(){
  w  <- c("Forecast" = "forecast", "Actual" = "actual", "Unweighted" = "unweighted")
  return(w)
}

LevelSelectionList <- function(){
  l <- c("1" = 1, "2" = 2, "3" = 3, "4" = 4 , "5" = 5, "All" = -1)
  return(l)
}

#'  Node selection list
#'  Creates a list of NodeID's
#'  Each item is named with the full node name: L1/L2 ... /Ln
#'  if a topNodeID is provided, only that node and those nodes under it node will be listed
#'  otherwise all nodes except super root
#'  output is sorted by the sortorder column in the node table
NodeSelectionList <- function(topNodeID = NULL, skip = FALSE, leaf = TRUE) {
  node <- GetDataObject("node")
  
  if (is.null(topNodeID)) {
    superRootNodeID <- node[SuperRootFlag == TRUE, NodeID]     # the top node (should not be part of the selection list)
    topNode  <- node[ParentnodeID == superRootNodeID, NodeID]  # the root node
  } else {
    topNode  <- node[NodeID == topNodeID, NodeID]  # the root node
  }
  
  n <- data.table(NodeID = integer(), sortOrder = character(), name = character())
  for (i in 1:length(topNode)) {
    n <- rbind(n, NodeSelection(topNode[i], node, skip = skip, leaf = leaf)[, .(NodeID, sortOrder, name)])
  }
  
  n <- n[order(sortOrder)]
  nList <- n[, NodeID]
  names(nList) <- n[, name]
  return(nList)
}

#' creates a shiny selection list of master hierarchies
HierarchySelectionList <- function(){
  node <- GetDataObject("node")
  masterNodeID <- node[SuperRootFlag == TRUE, NodeID]    # the top node (should not be part of the selection list)
  hNodes  <- node[ParentnodeID == masterNodeID][order(SortOrder)]  # the root nodes
  hList <- hNodes[, NodeID]
  names(hList) <- hNodes[, NodeName]
  return(hList)
}

#' Time Selection list
#' returns a list of time periods  for a Shiny drop down box
#' the list will be arranged as follows: Q1-2001, Q2-2001, Q3-2001, Q4-2001, 2001, Q1-2002, ... ,2002, Q1-2003, ...
TimeSelectionList <- function(){
  q <- GetDataObject("quarter")[,.(Year, HalfValue, ID = paste("Q-", QuarterID, sep=""), desc = PeriodName, sort = paste(Year, 3, QuarterValue))]
  h1 <- q[HalfValue == 1, .(ID = paste0("H1-", Year), desc = paste0("H1-", Year), sort = paste(Year, 2, HalfValue)), by = Year]
  h2 <- q[HalfValue == 2, .(ID = paste0("H2-", Year), desc = paste0("H2-", Year), sort = paste(Year, 2, HalfValue)), by = Year]
  y <- q[, .(ID = paste("Y-", min(Year), sep=""), desc = min(Year), sort = Year), by=Year]
  qhy <- rbind(q,h1, h2, y, fill = TRUE)[order(sort)]
  qList  <- qhy[,ID]
  names(qList) <- qhy[,desc]
  return(qList)
}

#' Quarter Selection List
#' returns a list of quarters between (inclusive) minQtrID and maxQtrID
QuarterSelectionList <- function(minQtrID = NULL, maxQtrID = NULL){
  q <- GetDataObject("quarter")[,.(QuarterID, PeriodName, PeriodStartDate)][order(PeriodStartDate)]
  
  if (!(is.null(minQtrID))) {
    minStartDate <- q[QuarterID == minQtrID, PeriodStartDate]
  } else {
    minStartDate <- q[1, PeriodStartDate]
  }
  
  if (!(is.null(maxQtrID))) {
    maxStartDate <- q[QuarterID == maxQtrID, PeriodStartDate]
  } else {
    maxStartDate <- max(q[, PeriodStartDate]) 
  }
  
  q <- q[PeriodStartDate >= minStartDate & PeriodStartDate <= maxStartDate]
  qList  <- q[,QuarterID]
  names(qList) <- q[,PeriodName]
  return(qList)
}

#' Half Selection List
#' returns a list of halves between (inclusive) minHalfName and maxHalfName
#' 
#' Note: there two ways that halves are named
#'  1. In the QuarterTable, the HalfName is of the form yyyy-Hh (e.g. 2018-H2)
#'     this form can be sorted.  It is used for the input parameters and the output list values
#'  2. The code creates a DisplayName column of the form Hh-yyyy (e.g. H2-2018)
#'     which is more human readable and is used for output list names
HalfSelectionList <- function(minHalfName = "2000-H1", maxHalfName = "9999-H2"){
  q <- GetDataObject("quarter")
  h <- q[HalfName >= minHalfName & HalfName <= maxHalfName, .(Year = min(Year), HalfValue= min(HalfValue)), by = HalfName][order(HalfName)]
  h[, DisplayName := paste0("H", HalfValue, "-", Year)]
  
  hList  <- h[ ,HalfName]
  names(hList) <- h[ ,DisplayName]
  return(hList)
}

#' Year Selection List 
#' returns a list of year values between (inclusive) minYr and maxYr
YearSelectionList <- function(minYr = 0, maxYr = 9999) {
  q <- GetDataObject("quarter")
  y <- q[Year>=minYr & Year <= maxYr, Year, by = Year][order(Year)]
  yList <- y[,Year]
  return(yList)
}

#' @name ProcessorSelectionList
#' 
#' @return List of processorIDs
#' 
#' @description 
#' each item in the list contains a processor ID and the item name is the 
#' Sampling Location Name
#' This is useful for Shiny drop down the menus which display the item name but return
#' item value - in this case, the ID
#' 
ProcessorSelectionList <- function() {
  processors <- GetDataObject("samplingLocation")[,.(SamplingLocationID, Name)][order(Name)]
  processorList <- processors[, SamplingLocationID]
  names(processorList) <- processors[, Name]  
  return(processorList)
}

#' @name BrandSelectionList
#' 
#' @return List of brandIDs
#' 
#' @description 
#' each item in the list contains a Brand ID and the item name is the 
#' Brand Name
#' List is in ascending brand name order
#' This is useful for Shiny drop down the menus which display the item name but return
#' item value - in this case, the ID
#' 
BrandSelectionList <- function() {
  brands <- GetDataObject("brand")[,.(BrandID, NameE)][order(NameE)]
  brandList <- brands[, BrandID]
  names(brandList) <- brands[, NameE]  
  return(brandList)
}

#' @name StatisticSelectionList
#' 
#' @return List of statistics
StatisticSelectionList <- function(){
  s <- GetDataObject("statistic")
  sList <- s[, StatisticID]
  names(sList) <- s[, NameE]
  return(sList)
}

#' @name VariableSelectionList
#' 
#' @return List of descriptive variables available for each statistic
#' @description The list item name is what appears in the drop down box (what the user sees)
#' The value is the actual name of the columns in the MultiStratumStatistic Data Table
VariableSelectionList <- function() {
  vList <- list(
    "Sampled units" = "units",
    "Observations" = "obs",
    "Sampled weight" = "gr",
    "Average" = "mean",
    "Variance" = "var",
    "Standard deviation" = "stdDev",
    "Lower 95%CI limit" = "lowerCI",
    "Upper 95%CI limit" = "upperCI",
    "95% Confidence Interval" = "CIPct"
  )
  return(vList)
}

#' @name ContainerVariableSelectionList
#' 
#' @return List of descriptive variables available for each container statistic
#' @description The list item name is what appears in the drop down box (what the user sees)
#' The value is the actual name of the columns in the MultiStratumStatistic Data Table
ContainerVariableSelectionList <- function() {
  cvList <- list(
    "Sampled containers" = "containers",
    "Sampled units" = "units",
    "Sampled weight" = "sumGr",
    "Average items per container" = "meanUnits",
    "Std deviation items per container" = "stdDevUnits",
    "Average weight per container" = "meanGr",
    "Std deviation weight per container" = "stdDevGr"
  )
  return(cvList)
}

#' @section Presentation standards
#' 
#' @description 
#' the functions in this section are designed to ensure that data is presented to users in a consistent way in all reports
#' this consistency includes 3 elements: 
#'  - the names given to each data element - DTColumnFormat
#'  - the numeric format for each data lement - DTColumnName
#'  - the unit of measure assigned to each element - DTUom
#'  Each of these function "assumes" that the other two functions are also being used:
#'    the column name where needed will include the unit of measure
#'    the numeric format will assume that the data is scaled to the UOM
#'  Note that the presentation unit of measure can be different from the uom in which the data is stored
#'    weights are stored in grams but presented in kg
#'    equipment age is stored in days but presented in years
#'  These functions need to know which statistic is being presented
#'    at time of writing these included: 1 - Weight ratio
#'                                       2 - Weight per unit
#'                                       3 - Average age
#'                                       6 - Damaged units
#'                                       7 - Cannibalized units
#'                                       8 - Average screen size
#'  For each statistic the presentation will depend on which data element is presented
#'  The following elements for each statistic can be presented:
#'                                       datalines - the number of SampleProduct rows (or containers for weight ratio)
#'                                       obs - the number of statistical observations
#'                                       units - the number of units observed
#'                                       gr - the weight of the sampled units (may not be in grams due to UOM)
#'                                       mean - the statistical average
#'                                       var - the variance
#'                                       stdDev - the standard deviation
#'                                       stdError - the standard error
#'                                       lowerCI - the lower CI bound
#'                                       upperCI - upper CI
#'                                       CIPct - +/-CI as a pct of mean
#'

#  DT Section ---------------------------------------------------------------------- 
#' @name DTGetDisplayColumnNames
#' 
#' @return
#'   a vector of "official names".  The vector will contain empty strings for columns with
#'   no official name
#'   
#' @param 
#'   statisticID        a vector of statistic IDs.  It must be of the same length as the variables vector
#'                      columns with null statisticsIDs are ignored.
#'   variables          a vector of variable names.
#'   type               either "obs" for single observations or "sum" (the default) for summary observations.
#'   displayParameters  a table of display parameters to be used.  If not provided, the function will import the default table.
#'
#' @description 
#'   statistic/variable/type combinations with display parameters that cannot be found will be given the variable name
#'   
#'
DTGetDisplayColumns <- function(statisticID, 
                                variables,
                                type = "sum",
                                displayParameters = NULL) {
  
  # check for valid inputs
  stopifnot(ncol(variables) == length(statisticID) |
              length(statisticID) == 1)
  
  # if needed, get display parameters table
  if (is.null(displayParameters))
    displayParameters <- DTImportDisplayParameters()
  
  # extend statisticID and variable vectors if needed
  if (length(statisticID) == 1)
    statisticID <- rep(statisticID, times = length(variables))
  
  # format each column
  columnNames <- NULL
  for (i in 1:length(variables)) {
    dp <-
      DTGetDisplayParameters(statisticID[i], variables[i], type, displayParameters)
    if (length(dp) != 0) {
      columnNames <- c(columnNames, dp$DisplayName)
    } else {
      columnNames <- c(columnNames, variables[i]
      )
    }
  }
  return(columnNames)
}


#' @name DTPostProcess
#' 
#' @return 
#'   table              a shinyTable in "official" format
#'   
#' @param 
#'   shinyTable         the table to be converted (must be a DT object - this is not a regular data.table)
#'   statisticID        a vector of statistic IDs.  It must be of the same length as the # of columns in the underlying table
#'                      or of length 1.  If of length 1 all columns are assumed to refer to the same statistic.
#'                      columns with null statisticsIDs are ignored.
#'   columnVars         a vector of data table variables  If left NULL (the default) the actual underlying  data table column names will be used.
#'                      if not, it must have the same number of entries as there are columns in the data table.
#'   type               either "obs" ( for single observations or "sum" (the default) for summary observations
#'   displayParameters  a table of display parameters to be used.  If not provided, the function will import the default table
#'
#' @description 
#'   for those columns whose variable name, statisticID and type combination have an official display parameter
#'   converts the value to the official unit of measure (if needed)
#'   Columns "not" in the official list are left unchanged
#'   The optional columns vector can be used if the underlying datatable names don't match the "official" variable names or if 
#'   the same variable is displayed in more than one column
#'   
DTPostProcess <- function(shinyTable, 
                         statisticID, 
                         columnVars = NULL,
                         type = "sum",
                         displayParameters = NULL) {
  
  # check for valid inputs
  stopifnot(ncol(shinyTable$x$data) == length(columnVars) |
              is.null(columnVars))
  stopifnot(ncol(shinyTable$x$data) == length(statisticID) |
              length(statisticID) == 1)
  
  # if needed, get display parameters table
  if (is.null(displayParameters))
    displayParameters <- DTImportDisplayParameters()
  
  # determine the columns to preprocess
  if (is.null(columnVars)){
    columnVars <- colnames(shinyTable$x$data)
  }
  
  # extend statisticID and variable vectors if needed
  if (length(statisticID) == 1)
    statisticID <- rep(statisticID, times = length(columnVars))
  
  # format each column
  for (i in 1:length(columnVars)) {
    if (!is.null(statisticID[i])) {
      dp <- DTGetDisplayParameters(statisticID[i], columnVars[i], type, displayParameters)
      if (length(dp) != 0) {
        
        # scale data then format column
        shinyTable$x$data[, i]  <-
          shinyTable$x$data[, i] * dp$ConversionFactor
        shinyTable <- switch(
          dp$FormatType,
          "C" = formatCurrency(
            shinyTable,
            columns = colnames(shinyTable$x$data)[i],
            currency = "",
            digits = dp$Decimals,
            interval = 3,
            mark = ","
          ),
          "P" = formatPercentage(
            shinyTable,
            columns = colnames(shinyTable$x$data)[i],
            digits = dp$Decimals
          ),
          shinyTable
        )
      }
    } 
  }
  return(shinyTable)
}

DTImportDisplayParameters <- function() {
  columns <- c("Type","Variable","StatisticID","FormatType","Decimals","ConversionFactor", 
               "Uom","DisplayName", "ReportName", "ChartType")
  dp <- as.data.table(read.csv("import/DisplayParameters.csv", 
                               skip = 1,
                               sep = ",",
                               stringsAsFactors = FALSE))[, ..columns]
}

#' @name DTGetDisplayParameters
#' 
#' @return a list of display parameters for a single statistic, variable displayType
#' 
#' @param 
#'    statisticID
#'    variableName
#'    type
#'    
#' @details 
#'    if no display parameters are specified this function will import them (default version)
#'    this is probably inefficient if this function is to be used several times in preparing a report
#'    It is preferable for the caller to do the import only once and provide the table
#'    
#' 2018-08-22 (MRB)
#'    Added accuracy item to the list of parameters.  This is used by the latest version of the scaling package
#'    The accuracy is defined as 1 / 10 ^ Decimals 
DTGetDisplayParameters <- function(statisticID, variableName, type = "sum", displayParameters = NULL) {
  if (is.null(displayParameters)) {
    displayParameters <- DTImportDisplayParameters()
  }
  dp <- displayParameters[  Variable == variableName &
                              StatisticID == statisticID &
                              Type == type]
  # there should be at most only be one format spec
  stopifnot(nrow(dp) <= 1)

  dpList <- list()
  if (nrow(dp) == 1) {
    dpList$FormatType <- dp[ , FormatType]
    dpList$Decimals <- dp[ , Decimals]
    dpList$Accuracy <- 1 / 10 ^ dp[ , Decimals]
    dpList$ConversionFactor <- dp[ , ConversionFactor]
    dpList$uom <- dp[ , Uom]
    dpList$DisplayName <- dp[ , DisplayName]
    dpList$ReportName <- dp[ , ReportName]
    dpList$ChartType <- dp[ , ChartType]
  }
  return(dpList)
}

#' @name DTGetReportCaption
#' 
#' @return an HTML caption for a shiny table or plot in standard form
#' 
#' @param 
#'  mainTitle            name of the report or chart
#'  secondRowTitle       manually set first item in the second row
#'  nodeSelection        id of the node for this report
#'  geoSelection         code for the geographical coverage of this report (see GeoSelection() for format spec)
#'  timeSelection        code for the temporal coverage of this report (see TimeSelection() for format spec)
#'  weighting            weighting used for this report ("forecast", "actual" or "unweighted")
#'  processorID          ID of the processor to which this report applies
#'  statisticID          ID of the statistic being presented
#'  variableName         ID of the variable being presented
#'  effectiveDateFlag    if TRUE, the default, includes the official effective date
#'  tableCaption         if TRUE, the default, for table captions.  Otherwise, enclosed in a div 
#'                         (use this when using renderText in Shiny)
#'  pdfCaption           if TRUE a simple text string is returned for use in a PDF report.  the tableCaptionValue is ignored
#'                       default is false.
#'                         
DTGetReportCaption <- function(
  mainTitle,
  secondRowTitle = NULL,
  nodeSelection = NULL,
  geoSelection = NULL,
  timeSelection = NULL,
  weighting = NULL,
  processorID = NULL,
  statisticID = NULL,
  variableName = NULL,
  effectiveDateFlag = TRUE,
  tableCaption = TRUE,
  pdfCaption = FALSE) {

  library(htmltools)  
  
  desc <- list()
  
  if (!is.null(secondRowTitle)) {
    desc <- c(desc, secondRowTitle)
  }
  
  if (!is.null(nodeSelection)) {
    desc <- c(desc, GetDataObject("node")[NodeID == nodeSelection, NodeName])
  }
  
  if (!is.null(geoSelection)) {
    selectionType = substring(geoSelection, 1, 1)
    selectionDetail = substring(geoSelection, 2)
    desc <- c(desc, switch(selectionType, 
                        "C" = switch(selectionDetail, 
                                     "0" = "Canada",
                                     "1" = "Canada (except OES)"
                        ),
                        "P" = GetDataObject("subprogram")[ProgramID == selectionDetail][1, ProgHdgE],
                        "S" = GetDataObject("subprogram")[SubrogramID == selectionDetail, FullName],
                        ""
              ) 
    )
  }
  
  if(!is.null(timeSelection)) {
    timeSelectionParameters <- strsplit(timeSelection, "-")
    desc <- c(desc, switch(timeSelectionParameters[[1]][[1]],
                           "Y" = timeSelectionParameters[[1]][[2]],
                           "H1" = paste0(timeSelectionParameters[[1]][[1]], "-", timeSelectionParameters[[1]][[2]]),
                           "H2" = paste0(timeSelectionParameters[[1]][[1]], "-", timeSelectionParameters[[1]][[2]]),
                           "Q" = GetDataObject("quarter")[QuarterID == timeSelectionParameters[[1]][[2]], PeriodName],
                           "")
    )
  }
  
  if(!is.null(weighting)) {
    desc <- c(desc, switch(weighting, 
                        "forecast" = "Forecast weighting",
                        "actual" = "Actual weighting",
                        "unweighted" = "Unweighted",
                        "")
    )
  }
  
  if(!is.null(processorID)) {
    desc <- c(desc, GetDataObject("samplingLocation")[SamplingLocationID == processorID, Name])
  }
  
  desc2 <- list()
  if(!is.null(statisticID) & !is.null(variableName)) {
    desc2 <- c(desc2, DTGetDisplayParameters(statisticID, variableName)$ReportName)
  } else {
    if(!is.null(statisticID)) {
      desc2 <- c(desc2, GetDataObject("statistic")[StatisticID == statisticID, NameE])
    }
    if(!is.null(variableName)) {
      desc2 <- c(desc2, variableName)
    }
  }
  
  reportDesc <- ifelse(length(desc) > 0, paste0(desc, collapse = " - "), " ")
  reportDesc2 <- ifelse(length(desc2) > 0, paste0(desc2, collapse = " - "), " ")
  effDate <- ifelse(effectiveDateFlag, DTDataEffectiveDate(), "")
  if (pdfCaption) {
    caption <- paste(mainTitle,  
                      reportDesc, 
                      reportDesc2,
                      effDate, sep = " ")
    return(caption)  
  }
  
  if (tableCaption) {
    caption <- withTags(caption(
      style = 'caption-side: top; text-align: center;',
      hr(),                  # horizontal line (rule)
      h2(strong(mainTitle)),
      h3(reportDesc),
      h4(reportDesc2),
      h5(effDate)
    ))
    return(caption)
  }
  
  caption <- withTags(div(
    style = 'caption-side: top; text-align: center;',
    h2(strong(mainTitle)),
    h3(reportDesc),
    h4(reportDesc2),
    h5(effDate),
    hr()                  # horizontal line (rule)
  ))
  return(caption)
}

#' @name DTDataEffectiveDate
#' 
#' @return a string containing the date/time stamp on which data was last loaded
#' 
#' @details 
#'  - date format is yyyy-mm-dd hh:mm:ss
#'  - if the data is inconsistent a warning is returned instead
##' 

DTDataEffectiveDate <- function() {
  effDateInfo <- GetDataLoadTiming()
  
  if (effDateInfo$consistencyFlag) {
    return(paste("Data as of: ", format(effDateInfo$officialTimeStamp, "%Y-%m-%d")))
  }
  
  return("Caution - inconsistent data - refresh")
}

#  IDLL Section ---------------------------------------------------------------------- 
#' @name GetPeriodsIDLL
#' 
#' @desc Utility function to convert shiny date parameters into a list 
#'       of time period information 
#' 
#' @return a three item list
#'  periodTable   table of quarters. 
#'                PeriodID - the value on which periods should be grouped
#'                QuarterID - the quarters that belong to each group
#'  periodLevels  a sorted list of levels for the PeriodID field
#'  periodLabels  a list of labels to assign to those levels
#'
#' @param a timeparms object with the following elements:
#'   timeUnit:       0 for quarters, 1 for halves and 2 for years
#'   startQuarterID: ID of the first quarter to display (ignored if timeUnit != 0)
#'   quarterCount:   number of quarters to display (ignored if timeUnit != 0)
#'   startHalfID:    ID of the first quarter to display (ignored if timeUnit != 1)
#'   halfCount:      number of quarters to display (ignored if timeUnit != 1)
#'   yearValues:     2 element vector of first and last year values (not the IDs)
#'                   to display (only used for timeUnit = 2)

GetPeriodsIDLL <- function(timeParms) {
  q <- GetDataObject("quarter")
  setkey(q, PeriodStartDate)
  
  # quarters
  if (timeParms$timeUnit == 0) {
    firstDate <- q[QuarterID == timeParms$startQuarterID, PeriodStartDate]
    q <- q[PeriodStartDate >= firstDate]
    # 2019-01-26 chained in code to eliminate NAs
    qTable <- q[1:timeParms$quarterCount, .(PeriodID = QuarterID,
                                            PeriodName,
                                            PeriodStartDate, 
                                            QuarterID)][!is.na(PeriodID)]
    periodLevels <- qTable[order(PeriodStartDate), PeriodID]
    periodLabels <- qTable[order(PeriodStartDate), PeriodName]
  } 
  
  # halves
  if (timeParms$timeUnit == 1) {
    firstDate <- q[HalfName == timeParms$startHalfID, min(PeriodStartDate)]
    q <- q[PeriodStartDate >= firstDate]

    # bug corrected on 2019-01-17 - note parens added in upcoming line
    # qTable <- q[1:timeParms$halfCount * 2, .(PeriodID = HalfName,
    #                                          DisplayName = paste0("H", HalfValue, "-", Year),
    #                                          PeriodStartDate, 
    #                                          QuarterID)]
    qTable <- q[1:(timeParms$halfCount * 2), .(PeriodID = HalfName,
                                             DisplayName = paste0("H", HalfValue, "-", Year),
                                             PeriodStartDate, 
                                             QuarterID)][!is.na(PeriodID)]
    periodLevels <- unique(qTable[order(PeriodStartDate), PeriodID])
    periodLabels <- unique(qTable[order(PeriodStartDate), DisplayName])
  }
  
  # years
  else if (timeParms$timeUnit == 2) {
    years <- timeParms$yearRange[1]:timeParms$yearRange[2]
    qTable <- q[Year %in% years, .(PeriodID = Year,
                                   QuarterID)]
    periodLevels <- sort(years)
    periodLabels <- sort(years)
  }
  
  return(list(periodTable = qTable[ , .(PeriodID, QuarterID)], periodLevels = periodLevels, periodLabels = periodLabels))
}

#' @name GetProgramsIDLL
#' 
#' @desc Utility function to convert shiny program parameters into a
#'       program/subprogram information 
#' 
#' @return a three item list
#'  programTable   table of programs. 
#'                 ProgramID - the value on which subprograms should be grouped
#'                 SubprogramID - the subprograms that belong to the group
#'  programLevels  a sorted list of labels for the ProgramID field
#'  programLabels  a list of labels to assign to those levels
#'
#' @param 
#'   programIDs      a list of programIDs with a "P" prefix (this is how they come from the UI)
#'   includeSummary  if TRUE, a summary level program covering all selected programs is provided (default is FALSE)
#'   

GetProgramsIDLL <- function(programIDs = NULL, includeSummary = FALSE) {
  if (is.null(programIDs)) {
    programs <- GetDataObject("subprogram")[, .(ProgramID, ProgHdgE, ProgSortOrder, SubprogramID)]
  } else {
    # strip prefix from program IDs
    programIDs <- sapply(programIDs, function(x) as.integer(substr(x, 2, 999)))
    programs <- GetDataObject("subprogram")[ProgramID %in% programIDs, .(ProgramID, ProgHdgE, ProgSortOrder, SubprogramID)]
  }
  
  programLevels <- unique(programs[order(ProgSortOrder), ProgramID])
  programLabels <- unique(programs[order(ProgSortOrder), ProgHdgE])
  programs <- programs[, .(ProgramID, SubprogramID)]
  if (includeSummary) {
    programs <- rbind(programs[, .(ProgramID = 0, SubprogramID)], programs)
    programLevels <- c(0, programLevels)
    programLabels <- c("Summary", programLabels) # Changing the word "Summary" could have side effects on the formats of certain tables
  }
  return(list(programTable = programs, programLevels = programLevels, programLabels = programLabels))
}

#' @name GetContainersIDLL
#' 
#' @desc Utility function to convert shiny program parameters into a
#'       container information 
#'
#' @param 
#'   includeALL       if TRUE a summary level is created with ID = 0 and Label = "All" 
#'
#' @return a three item list
#'  containerList     list of containerTypeIDs 
#'  containerLevels   a sorted list of levels for the ContainerID field
#'  containerLabels   a list of labels to assign to those levels
#'

GetContainersIDLL <- function(includeAll = TRUE) {
  containerTypes <- GetDataObject("containerType")
  containerList  <- containerTypes[, ContainerTypeID] 
  containerLevels <- containerTypes[order(ContainerTypeName), ContainerTypeID]
  containerLabels <- containerTypes[order(ContainerTypeName), ContainerTypeName]
  if (includeAll) {
    containerList  <- c(0, containerList)
    containerLevels <- c(0, containerLevels)
    containerLabels <- c("All", containerLabels) # Changing the word "All" could have side effects on the formats of certain tables
  }
  
  return(list(containerList = containerList, containerLevels = containerLevels, containerLabels = containerLabels))
}

#' @name GetNodesIDLL
#' 
#' @desc Utility function to convert shiny program parameters into a
#'       node information 
#' 
#' @return a three item list
#'  nodeList       list of NodeIDs
#'  nodeLevels     a sorted list of levels for the NodeID field
#'  nodeLabels     a list of labels to assign to those levels
#'  nodeShortnames a list of shortnames (last section of hierarchy)
#'  nodeDepth      hierarchy level  of node (parentNode is 1)
#'
#' @param 
#'   parentNodeID       the ID of the parent node
#'   depth              how many nodes deep into the hierarchy are to be retrieved (default = 1)
#'   skip               if TRUE (default) do not retrieve the parent node

GetNodesIDLL <- function(parentNodeID, depth = 1, skip = TRUE) {
  nodes <- NodeSelection(parentNodeID, depth = depth, skip = skip)
  nodeLevels <- nodes[order(sortOrder), NodeID]
  nodeLabels <- nodes[order(sortOrder), name]
  nodeShortnames <- nodes[order(sortOrder), shortname]
  nodeDepths <- nodes[order(sortOrder), level]
  return(list(nodeList = nodes[,NodeID], nodeLevels = nodeLevels, nodeLabels = nodeLabels,
              nodeShortnames = nodeShortnames, nodeDepths = nodeDepths))
}

#' @name GetProcessorLocationsIDLL
#' 
#' @desc Utility function to convert shiny program parameters into 
#'       processor/location information 
#' 
#' @return a three item list
#'  nodeTable      list of processorLocation
#'  nodeLevels     a sorted (by name) list of levels for the ID field
#'  nodeLabels     a list of labels to assign to those levels
#'
#' @param 
#' processorParms   a two item list describing the desired processor/locations
#'       selectionType "all"       (all processorLocations),
#'                     "allActive" all processorLocations with activity,
#'                     "specific"  user provided list of locations
#'       locationIDs   user provided list of IDs
GetProcessorLocationsIDLL <- function(processorParms) {
  locations <- GetDataObject("samplingLocation")[, .(SamplingLocationID, Name)]
  
  # kludgey fix to address problem with accented characters in Linux
  # this whould never trigger in pr`oduction which is a Windows environment`
  if (getOS() == "linux") {
    locations[SamplingLocationID == 2, Name := "Arteau, Québec, QC"]
    locations[SamplingLocationID == 7, Name := "CFER, St-Raphaël, QC"]
  }
  
  if (processorParms$selectionType == "specific") {
    locations <- locations[SamplingLocationID %in% processorParms$locationIDs]
  }
  
  locationList <- locations[order(Name), SamplingLocationID]
  locationLevels <- locations[order(Name), SamplingLocationID]
  locationLabels <- locations[order(Name), Name]
  return(list(locationList = locationList, 
              locationLevels = locationLevels, 
              locationLabels = locationLabels,
              activeOnly = processorParms$selectionType == "allActive"))
}
