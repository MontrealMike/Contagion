#'
#' Michael Bleau
#' 2015-03-02
#
#' Functions to create multistratum statistic data sets
#' 
#' 2017-06-22 (MRB)
#'   - added the option to breakdown statistics by sampling location
#' 2017-12-04 (MRB)
#'   - modified the stratum selection to use Stratum ID instead of a combination of
#'     subprogramID and QuarterID
#' 2017-03-06
#'   - Greatly simplified the MultiStratumNodeStats function. It now only calls MultiStratumTable once.
#'     
#' @section Setup

# load libraries
source("R/UtilityFunctions.R")

#' @section test code

# test MultiStratumTable
mstTest <- function(){
  source("R/ShinyHelperFunctions.R")
  nodeSelection <- "1000"
  level <- -1
  geoSelection <- "C1"  # C0, C1, or PprogramID or SsubprogramID
  timeSelection <- "Y-2017" #Yyyyy or QqID
  weighting <- "unweighted"
  brandID <- FALSE
  locationID <- TRUE
  nodeTable <- NodeSelection(nodeSelection, GetDataObject("node"), depth = level, skip = FALSE)
  nodeList <- nodeTable[, NodeID]
  
  geoSelectionList <- GeoSelection(geoSelection)
  timeSelectionList <- TimeSelection(timeSelection)
  strata <- as.data.table(StratumSelection(geoSelection, timeSelection))
  msl <- strata[ , .(MultiStratumHash = "", StratumID = strataID)]
  
  statsList <- as.list(1)
  
  mst <- MultiStratumTable(nodeSelectionTable = nodeList,
                           stratumSelectionTable = msl,
                           statisticList = statsList,
                           weighting = weighting,
                           brandID = brandID, 
                           locationID = locationID)
  return(mst)
}


#' @name MultiStratumTable
#' 
#' @return A table of statistical data
#' 
#' @param 
#' nodeSelectionTable:      a list of NodeIDs (integers)
#' stratumSelectionTable:   a 2-column table of MultiStratumHash , StratumID (Other columns are ignored).  There may be no rows with duplicate
#'                          MultiStratumHash, StratumID combinations or invalid resuts may be produced.
#' statisticList:           a list of StatisticIDs (integers). If NULL all statistics are returned. Default is NULL.
#' weighting:               the weighting logic to use in calulations ("forecast", "actual" or "unweighted")
#' brandID:                 TRUE if data is to be further broken down by brand. Cannot be TRUE if locationID is also TRUE.  Default is FALSE
#' locationID:              TRUE if data is to be further broken down by location. Cannot be TRUE if brandID is also TRUE.  Default is FALSE.
#' 
#' @description 
#' This is a critical function from a statistical accuracy perspective
#' Returns one row for each combination of NodeID, StatisticID and MultiStratumHash
#' The MultiStratumHash column is a user defined character value which determines which strata (Subprogram, Quarter) get combined
#' into a single MultiStratum
#' Columns returned are:
#'    MultiStratumHash, 
#'    NodeID, 
#'    BrandID (if brandID parameter set to true),
#'    SampleLocationID (if locationID parameter set to true),
#'    StatisticID, 
#'    units (number of units sampled),
#'    gr (grams sampled),
#'    obs (number of obs sampled - this can be the container count or units depending on the statistic),
#'    datalines (number of datalines from the sampling spreadsheets),
#'    mean (average - potentially weighted),
#'    var (calculated using Stats Canada formula),
#'    stdDev,
#'    errorCI (95% interval size - one side),
#'    lowerCI (mean - errorCI - or 0 whichever is greater),
#'    upperCI (mean + errorCI)
#'    MeanAvailable (TRUE/FALSE depending on NodeID and sufficiency of Observations)
#'    StDevAvailable (TRUE/FALSE depending on NodeID and sufficiency of Observations)
#'    
MultiStratumTable <- function(nodeSelectionTable, stratumSelectionTable, statisticList = NULL, weighting, 
                              brandID = FALSE, locationID = FALSE){
  
  # ensure that the table has the right columns and is not empty
  ValidStratumTable <- function(stratumSelectionTable) {
    valid <- all(c("MultiStratumHash", "StratumID") %in% colnames(stratumSelectionTable)) & 
      nrow(stratumSelectionTable) > 0
    return(valid)
  }
  
  
  # Create a table with one row containing the keys for each stat needed
  # The table has the following columns: SubprogramID, QuarterID, MultiStratumHashID, StatisticID, NodeID
  # All columns (but MultiStratumHashID) define a unique row and statistic
  CreateStatTable <- function(nodeList, stratumSelectionTable, statisticList = NULL){
    
    # convert statisticList to a data.table to enable Cartesian joins
    if (is.null(statisticList)) {
      statisticTable <- GetDataObject("statistic")[,.(StatisticID)]
    } else {
      
      statisticTable = as.data.table(as.integer(statisticList))
      setnames(statisticTable, names(statisticTable[1]), "StatisticID")
    }
    nodeTable = as.data.table(as.integer(nodeList))
    setnames(nodeTable, names(nodeTable[1]), "NodeID")
    
    statTable1 = Cartesian.dt(nodeTable, stratumSelectionTable)
    statTable2 = Cartesian.dt(statTable1, statisticTable)
    return(statTable2)
  }
  
  # retrieve the needed statistic records from individual stata and adds weighting values
  # and "result" column for use when combining statistics from multiple strata
  GetStratumStats <- function(statTable, weightingType = "forecast", brandID, locationID) {
    stopifnot(brandID == FALSE | locationID == FALSE) # at most one can be true
    if(brandID){
      stats <- GetDataObject("nodeBrandStratumStats")
    } else if (locationID) {
      stats <- GetDataObject("nodeLocationStratumStats")
    } else {
      stats <- GetDataObject("nodeStratumStats")    }
    stats <- merge(stats, statTable, by = c("StratumID", "StatisticID", "NodeID"), all.x=FALSE, all.y=TRUE)
    
    # Select the stats relevant to this query
    stratum <- GetDataObject("stratum")
    
    # select the appropriate statistic weights
    weightingColumn <- switch(weightingType,
                              "forecast" = stratum[ , .(fcstWeighting)],
                              "actual" = stratum[ , .(actWeighting)],
                              "unweighted" = stratum[ , .(unWeighting)])
    
    stratum[, weighting := weightingColumn]
    stats <- merge(stats, stratum[, .(StratumID, weighting, containers)], 
                   by = c("StratumID"), all.y = TRUE)
    stats[StatisticID == 1 & is.na(obs), ":=" 
          (
            obs = as.integer(containers), 
            sum = 0.0)
          ]  
    
    stats[, ":="
          (
            weightedSum = weighting * sum,
            weightedQty = weighting * obs
          )]
    
    stats[, ":="
          (
            term1 =  weightedQty^2,
            term2 = (1 - (obs / weightedQty)),
            term3 = var/obs
          )]
    stats[, result := term1 * term2 * term3]
  }
  
  
  # calculate multistratum statistics for each Node, Statistic combination
  # stratumStats: the statistical data
  CalcMultiStratumStats <- function(stratumStats, brandID, locationID){
    
    # set up grouping clause    
    stopifnot(brandID == FALSE | locationID == FALSE) # at most one can be true
    byClause = c("MultiStratumHash", "NodeID", "StatisticID")
    if(brandID){
      byClause = c(byClause, "BrandID")
    } 
    if (locationID) {
      byClause = c(byClause, "SamplingLocationID")
    }
    
    # sum across strata
    msStats <- stratumStats[, 
                            .(units = sum(units, na.rm = TRUE),
                              gr = sum(gr, na.rm = TRUE),
                              obs = sum(obs, na.rm = TRUE),
                              datalines = sum(datalines, na.rm = TRUE),
                              mean = sum(weightedSum, na.rm = TRUE) / sum(weightedQty, na.rm = TRUE),
                              var = sum(result, na.rm = TRUE) * (1/(sum(weightedQty, na.rm = TRUE)^2))
                            ),
                            by = byClause]
    
    msStats[, stdDev := sqrt(var)]
    msStats[, errorCI := normError(stdDev, 1, .95)]
    msStats[, c("lowerCI", "upperCI") :=
              list(lowerCI(mean, stdDev, 1, .95),
                   upperCI(mean, stdDev, 1, .95))]
    msStats[, CIPct := (upperCI - mean) / mean]
    
    # some statistics even though calculated are not truly valid
    # they are eliminated here
    # 2018-04-16 MRB removed minObs criterion for MeanAvailable but NOT for StDevAvailable
    nodeStatistic <- GetDataObject("nodeStatistic")
    msStats <- merge(msStats, nodeStatistic, by = c("NodeID", "StatisticID"))
    msStats[MeanAvailable == FALSE, ":=" 
            (
              mean = NA,
              var = NA,
              stdDev = NA,
              lowerCI = NA,
              upperCI = NA,
              CIPct = NA
            )]
    
    myMinObs <- minObs()
    msStats[StDevAvailable == FALSE | obs < myMinObs, ":=" 
            (              
              var = NA,
              stdDev = NA,
              lowerCI = NA,
              upperCI = NA,
              CIPct = NA
            )]
    
    return(as.data.table(msStats))
  }
  
  # main entry point ---------------------------------------
  # check stratum selection table
  if (!ValidStratumTable(stratumSelectionTable)) {
    return(NULL)
  }
  
  # finalize the statistics list
  if (is.null(statisticList)) {
    asl <- GetDataObject("statistic")[,StatisticID]
  }
  statTable <- CreateStatTable(nodeSelectionTable, stratumSelectionTable, statisticList)
  stratumStats <- GetStratumStats(statTable, weighting, brandID = brandID, locationID = locationID)
  msData <- CalcMultiStratumStats(stratumStats, brandID = brandID, locationID = locationID)
  return(msData)
}
