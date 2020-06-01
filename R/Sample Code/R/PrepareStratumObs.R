# EPRASampling
#
# Michael Bleau
# 2015-03-02
#
# Functions to generate sampling observations
# by Node, statistic and stratum
# 
#' 2017-06-14 (MRB)
#'   - add sampling location field to obs data sets
#'   - this will allow for calculation of statistics by location
#'
#' 2017-11-20 (MRB)
#'   - add product, subproduct ID information to observations
#'   - add stratumID to observations
#'   - this is used for later Auditing
#'   
#' 2018-08-10 (MRB)
#'   - return timing data


PrepareStratumObs <- function() {
  
  #' Sample product obs
  #' 
  #' one observation per statistic per input row (Product, Subproduct)
  #' Statistic 1 (Wt%) observations are calculated separately 
  #' 
  SampleProductObs <- function(sampleProduct, product, productStatistic) {
    UnitWtObs <- function(sampleProduct, productStatistic) {
      prodObs <- sampleProduct[productStatistic[MeanAvailable == 1 & StatisticID == 2L]][Gr > 0]
      prodObs <- prodObs[, x := Gr][, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, 
                                           ProductID, SubproductID, SampleProductID, SampleID, ContainerNumber, 
                                           units = SampleQty, gr = Gr, x)]
    }
    
    AvgAgeObs <- function(sampleProduct, productStatistic) {
      prodObs <- sampleProduct[productStatistic[MeanAvailable == 1 & StatisticID == 3L]]
      prodObs <- prodObs[, x := as.integer(difftime(as.Date(SampleDate), as.Date(MfgDate), units = "days"))][, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, 
                                                                                                                    ProductID, SubproductID, SampleProductID, SampleID, ContainerNumber,
                                                                                                                    units = SampleQty, gr = Gr, x)]
      prodObs <- prodObs[x > 0]    
    }
    
    DamageObs <- function(sampleProduct, productStatistic) {
      prodObs <- sampleProduct[productStatistic[MeanAvailable == 1 & StatisticID == 6L]]
      prodObs <- prodObs[, x := DamageScore][, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, ProductID, SubproductID, SampleProductID, SampleID, ContainerNumber,
                                                    units = SampleQty, gr = Gr, x)]
    }
    
    CannibObs <- function(sampleProduct, productStatistic) {
      prodObs <- sampleProduct[productStatistic[MeanAvailable == 1 & StatisticID == 7L]]
      prodObs <- prodObs[, x := CannibalizationScore][, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, ProductID, SubproductID, SampleProductID, SampleID, ContainerNumber,
                                                             units = SampleQty, gr = Gr, x)]
    }
    
    ScreenSizeObs <- function(sampleProduct, productStatistic) {
      prodObs <- sampleProduct[productStatistic[MeanAvailable == 1 & StatisticID == 8L]][DiagonalCm > 0]
      prodObs <- prodObs[, x := DiagonalCm][, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, ProductID, SubproductID, SampleProductID, SampleID, ContainerNumber,
                                                   units = SampleQty, gr = Gr, x)]
    }
    
    setkey(productStatistic, ProductID)
    setkey(sampleProduct, ProductID)
    
    sampleProductObs <- UnitWtObs(sampleProduct, productStatistic)
    sampleProductObs <- rbindlist(list(sampleProductObs, AvgAgeObs(sampleProduct, productStatistic)))
    sampleProductObs <- rbindlist(list(sampleProductObs, DamageObs(sampleProduct, productStatistic)))
    sampleProductObs <- rbindlist(list(sampleProductObs, CannibObs(sampleProduct, productStatistic)))
    sampleProductObs <- rbindlist(list(sampleProductObs, ScreenSizeObs(sampleProduct, productStatistic)))
    
    # For this kind of observation data lines are always 1, and observations and units are the same
    sampleProductObs[ , ":=" (datalines = 1, obs = units)] 
    return(sampleProductObs)
  }
  
  #' Generate Observations for each Data line/Node combination
  SampleNodeObs <- function(sampleProductObs, nodeLeafnode){
    
    # attach leaf node ID to each subproduct
    setkey(sampleProductObs, ProductID, SubproductID)
    setkey(nodeLeafnode, ProductID, SubproductID)
    sampleProductObs <- nodeLeafnode[sampleProductObs, allow.cartesian = TRUE]
    
    # line up column names for later merge
    sampleNodeObs <- sampleProductObs[, list(StratumID, QuarterID, SubprogramID, StatisticID, BrandID, SamplingLocationID, ProductID, SubproductID, NodeID, SampleProductID, SampleID, ContainerNumber,
                                             units, obs,gr, x, datalines)]
    
    return(sampleNodeObs)
  }
  
  #' Generate observations for each container/node combination (whether or not it has been sampled)
  ContainerNodeObs <- function(container, product, sampleProduct, node, nodeLeafnode) {
    #' attach LeafSubNodeID to each sampleProduct record
    setkey(sampleProduct, ProductID, SubproductID)
    setkey(nodeLeafnode, ProductID, SubproductID)
    sampleProduct <- nodeLeafnode[sampleProduct, allow.cartesian = TRUE][,.(SampleID, ContainerNumber, NodeID, LeafnodeID, SampleQty, Gr)]
    
    #' One observation per leafnode/container
    containerNodeObs <- sampleProduct[,.(units = sum(SampleQty, na.rm = TRUE),
                                         gr = sum(Gr, na.rm = TRUE),
                                         datalines = .N),
                                      by = .(SampleID, ContainerNumber, NodeID)]
    
    #' calculate weight ratio
    containerNodeObs <- merge(containerNodeObs, 
                              container[,.(SampleID, ContainerNumber, ContGr)], 
                              by = c("SampleID", "ContainerNumber"))
    containerNodeObs[, x := gr / ContGr]
    
    
    #' ContainerNode - 1 record per container node       
    cn <- Cartesian.dt(container, node)[,.(SampleID, ContainerNumber, StratumID, QuarterID, SubprogramID, SamplingLocationID, NodeID)]  # one observation per container product
    containerNodeObs <- merge(cn, containerNodeObs, by = c("SampleID", "ContainerNumber", "NodeID"), all.x = TRUE)    
    containerNodeObs[is.na(x), x := 0]
    containerNodeObs[is.na(units), units := 0]
    containerNodeObs[is.na(gr), gr := 0]
    containerNodeObs[is.na(datalines), datalines := 0]
    containerNodeObs <- containerNodeObs[,.(StratumID, QuarterID, SubprogramID, StatisticID=1L, BrandID = NA, SamplingLocationID, NodeID, ProductID = NA, 
                                            SubproductID = NA, SampleProductID = NA, SampleID, ContainerNumber, 
                                            units, obs = 1L, gr, x, datalines)]
    return(containerNodeObs)
  }
  
  #' Generate observations for each container/node/brand
  ContainerNodeBrandObs <- function(weightObs, container, node) {
    #' attach LeafSubNodeID to each sampleProduct record
    
    #' One observation per node-brand/container
    containerNodeBrandObs <- weightObs[ ,
                                        .(units = sum(units, na.rm = TRUE),
                                          gr = sum(x, na.rm = TRUE),
                                          datalines = .N),
                                        by = c("SampleID", "ContainerNumber", "NodeID", "BrandID")]
    
    #' calculate weight ratio
    containerNodeBrandObs <- merge(containerNodeBrandObs, 
                                   container[,.(StratumID, QuarterID, SubprogramID, SampleID, ContainerNumber, ContGr)], 
                                   by = c("SampleID", "ContainerNumber"))
    containerNodeBrandObs[, x := gr / ContGr]
    containerNodeBrandObs[, ContGr := NULL]
    
    # ContainerNode - 1 observation per container node       
    containerNodeBrandObs[is.na(units), units := 0]
    containerNodeBrandObs[is.na(x), x := 0]
    containerNodeBrandObs[is.na(gr), gr := 0]
    containerNodeBrandObs[is.na(datalines), datalines := 0]
    containerNodeBrandObs[, ":=" (StatisticID = 1L, obs = 1L, SampleProductID = NA)]
    
    return(containerNodeBrandObs)
  }
  
  #' Generate observations for each container/node/sampling location
  ContainerNodeLocationObs <- function(weightObs, container, node) {
    #' attach LeafSubNodeID to each sampleProduct record
    
    #' One observation per node-brand/container
    containerNodeLocationObs <- weightObs[ ,
                                           .(units = sum(obs, na.rm = TRUE),
                                             gr = sum(x, na.rm = TRUE),
                                             datalines = .N),
                                           by = c("SampleID", "ContainerNumber", "NodeID", "SamplingLocationID")]
    
    #' calculate weight ratio
    containerNodeLocationObs <- merge(containerNodeLocationObs, 
                                      container[,.(QuarterID, SubprogramID, SampleID, ContainerNumber, ContGr)], 
                                      by = c("SampleID", "ContainerNumber"))
    containerNodeLocationObs[, x := gr / ContGr]
    containerNodeLocationObs[, ContGr := NULL]
    
    # ContainerNode - 1 observation per container node       
    containerNodeLocationObs[is.na(units), units := 0]
    containerNodeLocationObs[is.na(x), x := 0]
    containerNodeLocationObs[is.na(gr), gr := 0]
    containerNodeLocationObs[is.na(datalines), datalines := 0]
    containerNodeLocationObs[, ":=" (StatisticID = 1L, obs = 1L, SampleProductID = NA)]
    
    return(containerNodeLocationObs)
  }
  
  #' Generate an observation for each container
  ContainerObs <- function(container, sampleProduct) {
    
    #' One observation per container
    containerObs <- sampleProduct[,.(units = sum(SampleQty, na.rm = TRUE),
                                     obs = 1,
                                     gr = sum(Gr, na.rm = TRUE),
                                     x = sum(Gr, na.rm = TRUE) / 1000,   # redundant, but OK this is the official statistic for container weights
                                     datalines = .N),
                                  by = .(SampleID, ContainerNumber)]
    
    #' Container - outer join on containers in case there are any empty containers
    containerObs <- merge(container, containerObs, by = c("SampleID", "ContainerNumber"), all.x = TRUE)    
    containerObs[is.na(units), units := 0]
    containerObs[is.na(x), x := 0]
    containerObs[is.na(obs), obs := 0]
    containerObs[is.na(gr), gr := 0]
    containerObs[is.na(datalines), datalines := 0]
    containerObs <- containerObs[,.(StratumID, QuarterID, SubprogramID, StatisticID=200L, ContainerTypeID, SampleID, ContainerNumber, 
                                    units, obs, gr, x, datalines)]
    return(containerObs)
  }
  
  #' @Section Entry point ---------------------------------------------------
  obsDateTimeStamp <- list(start = Sys.time(), end = Sys.time())
  load(EPRASamplingDataFile())
  sampleProductObs <- SampleProductObs(epradata$sampleProduct, epradata$product, epradata$productStatistic)
  sampleNodeObs <- SampleNodeObs(sampleProductObs, epradata$nodeLeafnode)
  containerNodeObs <- ContainerNodeObs(epradata$sampleContainer, epradata$product, epradata$sampleProduct, epradata$node, epradata$nodeLeafnode)
  nodeObs <- rbind(sampleNodeObs, containerNodeObs)
  
  containerNodeBrandObs <- ContainerNodeBrandObs(sampleNodeObs[StatisticID == 2],
                                                 epradata$sampleContainer, 
                                                 epradata$node)
  containerObs <- ContainerObs(epradata$sampleContainer, epradata$sampleProduct)
  obsDateTimeStamp$end = Sys.time()
  save(nodeObs, containerNodeBrandObs, containerObs, obsDateTimeStamp, file = EPRASamplingObsFile())
  return(obsDateTimeStamp)
}

