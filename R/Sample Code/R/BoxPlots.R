# BoxPlots.R
#
# Michael Bleau
# 2019-02-06
#
# Functions to generate BoxPlots to examine the distribution of various
# statistics
# 
#' @changes
#' 2019-02-16 (MRB)
#'   rewrite to use tiydverse
#' 
#' 
#' @section Test

boxTest <- function(){
  nodeID <- 12000
  programList <- c("P1", "P3", "P4", "P5", "P6", "P7", "P8", "P9")
  # programList <- c("P9")
  timeType <- 1
  yearValues <- c(2014, 2018)
  quarterStartID = 1
  quarterCount = 8
  halfStartID = "2015-H1"
  halfCount = 8
  timeParms <- list(timeUnit = timeType, 
                    startQuarterID = quarterStartID,
                    quarterCount = quarterCount,
                    startHalfID = halfStartID,
                    halfCount = halfCount,
                    yearRange = yearValues)
  statID <- 2  #unit wt  
  plotBy <- "node" # or program, period
  
  boxplot <- GetBoxPlot(statisticID = statID,
                        nodeID = nodeID, 
                        programList = programList, 
                        timeParms = timeParms,
                        plotBy = plotBy)
  return(boxplot)
}

#' @section main


#' @name GetBoxPlot
#' 
#' @desc Generate a gg2plot BoxPlot  
#' 
#' @param 
#' statisticID      the ID of the statistic to use
#' nodeID           A node ID.  
#'                  If plotBy = "node" there will be plot for each (direct) child of the node ID.  
#'                  Otherwise only data for this node will be included in the plot
#'                  
#' programList      a list of the program ID's to include (each with a P prefix)
#'                  If plotBy = "program" there will be a plot for each program in the list
#'                  Otherwise only data for the programs in the list will be included in the plot
#' timeParms        a list of parameters describing the time periods and units to cover
#'                  If plotBy = "priod" there will be a plot for each selected period
#'                  Otherwise only data for the selected time periods will be included in the plot
#' plotBy           identify the data item for which individual boxplots will be produced
#'                  must be one of "node", "program", "period"
#' 
#' @return a ggplot2 BoxPlot item or NULL if no data matching the parameters can be found

GetBoxPlot <- function(statisticID,
                       nodeID, 
                       programList, 
                       timeParms,
                       plotBy) {
  
  #' @name MultiStrataCategories
  #' 
  #' @return a table of multistrata to plot.  Each multistratum is given a multistratum ID (MultiStratumHash).  
  #' 
  #' @param 
  #' periods          period table as produced by the GetPeriod function
  #' programs         programs table as produced by the GetPrograms function
  #' 
  #' @details 
  #' DataTable columns:  MultiStratumHash, PeriodID, StratumID, QuarterID, ProgramID
  #' 
  #' @description 
  #' Yields a table designed to be used by the MultiStratumTable function (in PrepareMultiStratumStats.R).
  #' A multi stratum is a combination of individual strata.  A stratum contains the statistics for a single subprogram, quarter combo
  #' a multi stratum contains statistics for a given combination of strata
  #' this function creates a data structure which defines multistrata
  #' for each time unit in the datevectorlist parameter plus.  
  #' There is a multi stratum for each period program combination (that is found in the stratum table)
  #'  
  MultiStrataCategories <- function(periods, programs){
    
    # cartesian join of programs and periods
    pp <- Cartesian.dt(programs, periods)
    
    # Get strata
    strata <- GetDataObject("stratum")[,.(StratumID, SubprogramID, QuarterID)]
    strata <- GetDataObject("stratum")[,.(StratumID, SubprogramID, QuarterID)]
    
    # merge in program periods
    ms <- merge(strata, pp, by = c("SubprogramID", "QuarterID"))
    
    # Create hash
    ms[ , MultiStratumHash := paste(ProgramID, PeriodID, sep = "|")]
    
    return(ms)
  }
  
  # main entry point -----------------------------------------------------------
  source("R/UtilityFunctions.R")
  source("R/ShinyHelperFunctions.R")
  library(tidyverse)
  periods <- GetPeriodsIDLL(timeParms)
  programs <- GetProgramsIDLL(programList, includeSummary = FALSE)
  
  # if plotBy is by node, we just want the single Node value ortherwise we want it's children``
  # TODO think further about depth
  nodes <- switch(plotBy,
                  "node" = GetNodesIDLL(nodeID, depth = 1, skip = TRUE),
                  GetNodesIDLL(nodeID, depth = 0, skip = FALSE))

  # Get strata
  msl <- MultiStrataCategories(periods$periodTable, programs$programTable)
  
  # Get Observations that meet criteria
  obs <- as_tibble(GetDataObject("nodeObs")[StatisticID == statisticID & 
                                  StratumID %in% msl[,StratumID] &
                                  NodeID %in% nodes$nodeList,
                                  .(NodeID, StratumID, plotVariable = x)])
  
  # attach Program & Period columns
  obs <- inner_join(x = obs, y = msl[, .(StratumID, ProgramID, PeriodID)], by = "StratumID") 
  
  # convert ID fields to sorted factors
  obs$ProgramID <- ordered(obs$ProgramID, levels = programs$programLevels, labels = programs$programLabels)
  obs$PeriodID <- ordered(obs$PeriodID, levels = periods$periodLevels, labels = periods$periodLabels)
  obs$NodeID <-  ordered(obs$NodeID, levels = nodes$nodeLevels, labels = nodes$nodeLabels)
  
  # standardize category name
  obs <- switch(plotBy,
                "node" = obs %>% rename(plotCategory = NodeID),
                "program" = obs %>% rename(plotCategory = ProgramID),
                "period" = obs %>% rename(plotCategory = PeriodID))
  
  # create boxplot
  p <- ggplot(obs, aes(plotCategory, plotVariable))
  p <- p + geom_boxplot() + coord_flip()
  
  return(p) 
}