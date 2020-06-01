#' @author Michael Bleau
#'
#' 2017-06-20
#'
#' @description
#' Functions for producing detailed sampling data reports reports to provide auditors
#' with an explanation of how sampling results are calculated and a link to the database
#' items used in the calculations
#' Note that these reports apply to multi-stratum / single statistic combinations
#'

source("R/UtilityFunctions.R")

# test multi stratum report
msTest <- function() {
  nodeSelection <- "12000"
  strataSelection = c(2554, 2594) # Q1-2017 Ont and Q1-2017 Que
  strataSelection = StratumSelection("C", "Y2017")$strataID
  # strataSelection = StratumSelection("C", "Y2012")$strataID # dummy with no observations
  statSelection <- 1
  weightingSelection <- "forecast"
  reportData <-
    GetMultiStratumReports(nodeSelection,
                           strataSelection,
                           statSelection,
                           weightingSelection)
}

#' @name GetMultiStratumReports
#'
#' @return A two item list:
#'         detail -  a Shiny DT object containing the observations specified in the input parameters and
#'         summary - a one row Shiny DT object with various statistics variables summarizing the contents of the detail table
#'
#' @param
#' nodeID:             a single nodeID in the product hierarchy
#' subSelection:       a vector of stratumIDs
#' statSelection:      the ID of a single statistic
#' weightingSelection: the type of weighting to be used
#'
#' @description
#' The reports are designed to make visible  the statistics for a given Node/Statistic and from multiple strata
#' are assembled and summarized.  In particular it shows how weighting logic is applied and how the estimated
#' sampling variance within each stratum is computed and then the stratum contributions added up
#' to give the complete multiple stratum estimate.
#'
GetMultiStratumReports <- function(nodeSelection,
                                   strataSelection,
                                   statSelection,
                                   weightingSelection) {
  #' @name GetSummaryTable
  #'
  #' @return a single line DT formatted table
  #'
  #' @param
  #' summary:       a one line data table containing summary statistics
  #' statisticID:   the ID of the statistic being summarized
  #' displayParameters:  a table of display parameters to be used (see ShinyHelperFunctions)
  #'
  #' @description
  #' converts the datatable to a Shiny DT object
  #' columns added and data is formatted and converted to appropriate units
  #'
  GetSummaryTable <-
    function(summary,
             statisticID,
             displayParameters) {

      # select and reorder columns
      columnNames <- c("strata", "obs", "units", "gr", "mean", "var", "stdDev", "lowerCI", "upperCI", "CIPct")
      summary <- summary[, ..columnNames]

      # get display column names
      displayColumnNames <- DTGetDisplayColumns(statisticID = statisticID,
                                                variables = columnNames,
                                                type = "sum",
                                                displayParameters = displayParameters)
      # tidy up column names
      displayColumnNames[1] <- "Strata"
      
      # build the data table
      dt <- datatable(
        summary,
        colname = displayColumnNames,
        style = "default",
        rownames = FALSE,
        options = list(dom = "t")
      )
      
      # postprocess
      dt <- DTPostProcess(shinyTable = dt,
                           statisticID = statisticID,
                           type = "sum",
                           displayParameters = displayParameters)
      return(dt)
    }
  
  #' @name GetStratumDetailTable
  #'
  #' @return
  #' a DT table formatted for presentation in shiny containing one line for each
  #' line of data contributing to that statistic
  #'
  #' @param
  #' detail:          datatable with one row per data line
  #' displayParameters:  a table of display parameters to be used (see ShinyHelperFunctions)
  #'
  #' @description
  #' Adds descriptive columns to the data table
  #' Selects and orders the data columns for display purposes
  #' Formats the values according the statistic being displayed
  #' Creates a total row which appears as the first row of the display
  GetStratumDetailTable <-
    function(detail,
             statisticID,
             strataSelection,
             displayParameters)
    {
      
      # add stratum info
      stratum <-
        GetDataObject("stratum")[StratumID %in% strataSelection, .(
          StratumID,
          StratumDesc = paste0(PeriodName, " ", ProgramShortname, "-", SubprogramShortname),
          PeriodStartDate,
          ProgSortOrder
        )]
      
      detail <- merge(detail, stratum, by = "StratumID")

      # add row names
      detail[, ShinyRowName := as.character(.I)]
      
      # sort rows
      detail <- detail[order(PeriodStartDate, ProgSortOrder)]
      
      
      # add a total row
      total <- detail[, .(
        obs = sum(obs),
        gr = sum(gr),
        weightedSum = sum(weightedSum),
        weightedQty = sum(weightedQty),
        result = sum(result),
        ShinyRowName = "Total"
      )]
      detail <- rbind(total, detail, fill = TRUE)

      # variable columns
      columnNames <- c(
        "ShinyRowName",
        "StratumID",
        "StratumDesc",
        "weighting",
        "obs",
        "gr",
        "sum",
        "mean",
        "var",
        "weightedSum",
        "weightedQty",
        "term1",
        "term2",
        "term3",
        "result"
      )
      
      # keep only needed columns
      detail <- detail[, ..columnNames]
      
      
      # override most custom (non-official) column names to refer to StatsCan formula
      columnName <- DTGetDisplayColumns(statisticID = statisticID,
                                        variables = columnNames,
                                        type = "sum",
                                        displayParameters = displayParameters)
      columnName[1] <- ""
      columnName[2] <- "StratumID"
      columnName[3] <- "Name"
      columnName[4] <- "Weighting"
      columnName[5] <- "Obs (n)"
      columnName[7] <- "Sum"
      columnName[9] <- "Variance (S)"
      columnName[10] <- "Weighted sum"
      columnName[11] <- "Weighted obs (N)"
      columnName[12] <- "N^2"
      columnName[13] <- "1 - (n/N)"
      columnName[14] <- "S^2 / n"
      columnName[15] <- "N^2(1 - n/N)S^2/n"
      
      dt <- datatable(
        detail,
        colname = columnName,
        style = "default",
        rownames = FALSE,
        escape = TRUE,
        extensions = 'Buttons',
        options = list(
          dom = 'Bt',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = -1L
        )
      )
      
      # apply numeric formats
      dt <- DTPostProcess(shinyTable = dt,
                          statisticID = statisticID)
      return(dt)
    }
  
  # Main Entry Point -----------------------------------------------------------------------------------------------------------------------------------------
  
  # get the requested statistics
  nodeStratumStats <-
    GetDataObject("nodeStratumStats")[NodeID == nodeSelection &
                                        StratumID %in% strataSelection &
                                        StatisticID == statSelection]
  
  # merge in appropriate weighting factor
  strata <- GetDataObject("stratum")[StratumID %in% strataSelection]
  strata <- switch(
    weightingSelection,
    "forecast" = strata[, .(StratumID, weighting = fcstWeighting)],
    "actual" = strata[, .(StratumID, weighting = actWeighting)],
    "unweighted" = strata[, .(StratumID, weighting = unWeighting)],
    stop("invalid weighting selection")
  )
  nodeStratumStats <-
    merge(nodeStratumStats, strata[, .(StratumID, weighting)], by = "StratumID")
  
  # create intermediate columns
  nodeStratumStats[, ":="
                   (weightedSum = weighting * sum,
                     weightedQty = weighting * obs)]
  
  nodeStratumStats[, ":="
                   (term1 =  weightedQty ^ 2,
                     term2 = (1 - (obs / weightedQty)),
                     term3 = var / obs)]
  nodeStratumStats[, result := term1 * term2 * term3]
  
  # summarize most basic stats
  summaryStats <- nodeStratumStats[,
                                   .(
                                     strata = .N,
                                     units = sum(units, na.rm = TRUE),
                                     gr = sum(gr, na.rm = TRUE),
                                     obs = sum(obs, na.rm = TRUE),
                                     datalines = sum(datalines, na.rm = TRUE),
                                     sumWeightedSum = sum(weightedSum, na.rm = TRUE),
                                     sumWeightedQty = sum(weightedQty, na.rm = TRUE),
                                     sumResult = sum(result, na.rm = TRUE),
                                     mean = NA,
                                     var = NA,
                                     stdDev = NA,
                                     errorCI = NA,
                                     lowerCI = NA,
                                     upperCI = NA,
                                     CIPct = NA
                                   )]
  
  # determine availability of statistics for this particular Node Statistic combo
  nodeStatistic <-
    GetDataObject("nodeStatistic")[NodeID == nodeSelection &
                                     StatisticID == statSelection]  # this should return only one record
  meanAvailable <- nodeStatistic[1, MeanAvailable]
  stDevAvailable <- nodeStatistic[1, StDevAvailable]
  obs <- summaryStats[1, obs]
  
  # calculate available statistics  
  # 2018-08-10 removed minObs requirement for mean calculation
  if (meanAvailable) {
    summaryStats[, mean := sumWeightedSum / sumWeightedQty]
  }
  
  if (stDevAvailable & obs > minObs()) {
    summaryStats[, var := sumResult * (1 / sumWeightedQty ^ 2)]
    summaryStats[, stdDev := sqrt(var)]
    summaryStats[, errorCI := normError(stdDev, 1, .95)]
    summaryStats[, ":=" (
      lowerCI = lowerCI(mean, stdDev, 1, .95),
      upperCI = upperCI(mean, stdDev, 1, .95)
    )]
    summaryStats[, CIPct := (upperCI - mean) / mean]
  }
  
  # format into Shiny tables
  displayParameters <-
    DTImportDisplayParameters()[StatisticID == statSelection]   # more efficient to retrieve them just once
  detailTable <- GetStratumDetailTable(nodeStratumStats, statSelection, strataSelection, displayParameters)
  summaryTable <-
    GetSummaryTable(summaryStats, statSelection, displayParameters)
  
  reports <-
    list(detailTable = detailTable, summaryTable = summaryTable)
  return(reports)
}