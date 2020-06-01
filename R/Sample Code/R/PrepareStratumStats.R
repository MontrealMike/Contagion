# EPRASampling
#
# Michael Bleau
# 2015-03-02
#
# Functions to generate statistics for each node, stratum statistic combination

#' 2017-06-14 (MRB)
#'   - generate and save a data table with stats by sampling location
#'   
#' 2018-08-10 (MRB)
#'   - return timing data

#' 2018-10-29 (MRB)
#'   - changed the calculation of the units variable for the nodeBrandStratumStats table
#'   - added containers column to nodeBrandStratumStats table
#'     note that this table is only used for brand analysis which was not implemented prior to these changes
#'     there should be therefore be no side effects from this modification


#' @section Calculate stats
PrepareStats <- function () {
  StratumStats <- function(obs, byBrand = FALSE, byLocation = FALSE){
    
    # byBrand and byLocation cannot both be true
    stopifnot(byBrand == FALSE || byLocation == FALSE)

    byClause = c("StratumID", "QuarterID", "SubprogramID", "StatisticID", "NodeID")
    
    if(byBrand){
      byClause = c(byClause, "BrandID")
    }
    if(byLocation){
      byClause = c(byClause, "SamplingLocationID")
    }
    
    stratumStats <- obs[, .(sum = sum(x), 
                            meanNc = mean(x), 
                            mean = sum(x) / sum(obs),
                            var = var(x), 
                            stdDev = sd(x),
                            units = sum(units),
                            gr = sum(gr),
                            obs = sum(obs), 
                            datalines = sum(datalines),
                            MeanAvailable = as.logical(min(MeanAvailable)),
                            StDevAvailable = as.logical(min(StDevAvailable)),
                            SufficientObs = (sum(obs) >= minObs())),  #TODO
                        by = byClause]
    stratumStats[, c("lowerCI", "upperCI") :=
                   list(lowerCI(mean, stdDev, obs),
                        upperCI(mean, stdDev, obs))]
    stratumStats[ , CIPct := (upperCI - mean) / mean]

    # we have calculated all the stats - now eliminate the ones that are not statistically valid
    # 2017-12-11 MRB removed minObs criterion from MeanAvailable calc
    stratumStats[MeanAvailable == FALSE, c("mean", "var", "stdDev", "lowerCI", "upperCI", "CIPct") := NA]

    # 2018-05-10 MBR Removed Sufficient Obs criterion <- it is now only applied when calculating multistratum stats
    # stratumStats[StDevAvailable == FALSE | SufficientObs == FALSE, c("var", "stdDev", "lowerCI", "upperCI", "CIPct") := NA]
    stratumStats[StDevAvailable == FALSE, c("var", "stdDev", "lowerCI", "upperCI", "CIPct") := NA]
    
    return(stratumStats)
  }

    
  StratumWtPctStats <- function(obs, stratum) {
    # Get unique list of node/brand/stratum and merge in container count
    nbs <- as.data.table(unique(obs[ , .(StratumID, NodeID, BrandID)]))
    nbs <- merge(nbs, stratum[ , .(StratumID, containers)], by = "StratumID")
    
    # sum across containers
    nbStratumStats <- obs[ , 
                           .(sum = sum(x),
                             gr = sum(gr),
                             units = sum(units),
                             datalines = sum(datalines),
                             explicitObs = .N),
                           by = .(StratumID, QuarterID, SubprogramID, NodeID, BrandID)]
    
    # merge with node brand strata to get full outer join and container count
    nbStratumStats <- merge(nbs, nbStratumStats, 
                            by = c("StratumID", "NodeID", "BrandID"),
                            all.x = TRUE)
    
    # calc the mean
    nbStratumStats[ , ":=" (mean = sum / containers,
                            obs = containers,
                            implicitObs = containers - explicitObs)]
    
    # using the mean go back to get sum's of squares for explicit observations
    obs <- merge(obs, nbStratumStats, by = c("StratumID", "NodeID", "BrandID"))
    obs[ , ":=" (sqdiff = (mean - x)^2)]
    nbVariance <- obs[, .(sumsqdiff = sum(sqdiff)), 
                      by = .(StratumID, NodeID, BrandID)]
    nbStratumStats <- merge(nbStratumStats, nbVariance, by = c("StratumID", "NodeID", "BrandID"), all.x = TRUE)
    
    # add explicit to implicit sumsq to get Variance
    # implicit obs are from containers that do not have any of the node/brand combo
    # they therefore each have a 0 statistic sum of square of (mean - 0) squared
    nbStratumStats[ , var := (sumsqdiff + implicitObs * mean^2) / obs ]
    
    # get other statistics
    nbStratumStats[ , stdDev := sqrt(var) ]
    nbStratumStats[ , c("lowerCI", "upperCI") :=
                      list(lowerCI(mean, stdDev, obs),
                           upperCI(mean, stdDev, obs))]
    
    # delete/add columns to make it "bindable" with remaining stats
    nbStratumStats[ , ":=" (meanNc = mean,
                            explicitObs = NULL, 
                            implicitObs = NULL,
                            sumsqdiff = NULL,
                            CIPct = (upperCI - mean) / mean,
                            StatisticID = 1,
                            MeanAvailable = NA,
                            StDevAvailable = NA,
                            SufficientObs = NA)]
    
    return(nbStratumStats)
  }
  
  
  #' @Section Entry point ------------------------------
  statsDateTimeStamp <- list(start = Sys.time(), end = Sys.time())
  
  # node based statistics
  nObs <- GetDataObject("nodeObs")
  nodeStats <- GetDataObject("nodeStatistic")
  obs <- merge(nObs, nodeStats, by = c("NodeID", "StatisticID"))
  nodeStratumStats <- StratumStats(obs)
  nodeLocationStratumStats <- StratumStats(obs, byLocation = TRUE)
  
  # node brand based statistics
  strata <- GetDataObject("stratum")
  containerNBObs <- GetDataObject("containerNodeBrandObs")
  nodeBrandPctWtStratumStats <- StratumWtPctStats(containerNBObs, strata)
  nodeBrandStats <- StratumStats(obs[StatisticID != 1], byBrand = TRUE)
  nodeBrandStratumStats <- rbind(nodeBrandPctWtStratumStats, nodeBrandStats, fill = TRUE)
  
  statsDateTimeStamp$end = Sys.time()
  save(nodeStratumStats, nodeLocationStratumStats, nodeBrandStratumStats, statsDateTimeStamp, file = EPRASamplingStatsFile())
  return(statsDateTimeStamp)
}