#' @name SamplingMathSingleStratumReport.R
#' @author Michael Bleau
#'
#' 2017-06-20
#'
#' @description
#' Functions for producing detailed sampling data reports reports to provide auditors
#' with an explanation of how sampling results are calculated and a link to the database
#' items used in the calculations
#' Note that these reports only apply to single stratum / single statistic combinations
#'

source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

# test single stratum report
ssTest <- function() {
  nodeSelection <- "12000"
  stratumSelection <- 1344  #Qc Q1-2016
  statSelection <- 2
  reportData <-
    GetSingleStratumReports(nodeSelection, stratumSelection, statSelection)
}

#' @name GetSingleStratumReports
#'
#' @return A two item list:
#'         detail -  a Shiny DT object containing the observations specified in the input parameters and
#'         summary - a one row Shiny DT object with various statistics variables summarizing the contents of the detail table
#'
#' @param
#' nodeID:             a single nodeID in the product hierarchy
#' stratum:            a single stratumID
#' statSelection:      the ID of a single statistic to be calculated
#'
#' @description
#' The reports are designed to make visible the details of how statistics for any
#' node / stratum / statistic combination are calculated. They provide the user with a link to each
#' Sample item which contributed to that statistic. This in turn makes it possible to retrieve the relevant
#' database records and, from there, the data collection worksheets.
#'
GetSingleStratumReports <- function(nodeSelection,
                                    stratumSelection,
                                    statSelection) {

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
      # Ordered list of the report column variable names for this statistic
      reportVars <-
        c(
          "datalines",
          "obs",
          "units",
          "gr",
          "mean",
          "var",
          "stdDev",
          "lowerCI",
          "upperCI",
          "CIPct",
          "meanAvailable",
          "stDevAvailable"
        )
      setcolorder(summary, reportVars)
      
      # Get standard column names (last 2 are not std)
      colNames <- DTGetDisplayColumns(statisticID = statisticID,
                                      variables = reportVars,
                                      type = "sum",
                                      displayParameters = displayParameters)
      colNames[11] <- "Mean available?"
      colNames[12] <- "Std dev available?"
      
      dt <- datatable(
        summary,
        colname = colNames,
        style = "default",
        rownames = FALSE,
        options = list(dom = "t")
      )
      
      # scale and format column data
      dt <- DTPostProcess(shinyTable = dt,
                          statisticID = statisticID,
                          type = "sum",
                          displayParameters = displayParameters)      
      
      return(dt)
    }
  
  #' @name GetDetailTable
  #'
  #' @return
  #' a DT table formatted for presentation in shiny containing one line for each
  #' line of data contributing to that statistic
  #'
  #' @param
  #' detail:             a datatable with one row per data line
  #' statisticID:        the ID of the statistic on which the data table is reporting
  #' displayParameters:  a table of display parameters to be used (see ShinyHelperFunctions)
  #'
  #' @description
  #' Determine which type of Statistics are needed: container-based or product-based
  #' calls the appropriate function toobtain the needed table.
  #' Currently, the only statistic that is container based is the Weight Ratio whose
  #' statisticID is 1.
  GetDetailTable <-
    function(detail, statisticID, displayParameters) {
      # WeightRatio statistics are different and require different columns
      if (statisticID == 1) {
        rt <- GetContainerDetailTable(detail, displayParameters)
      } else {
        rt <- GetProductDetailTable(detail, statisticID, displayParameters)
      }
      return(rt)
    }
  
  #' @name GetContainerDetailTable
  #'
  #' @return
  #' a DT table formatted for presentation in shiny containing one line for each
  #' line of data contributing to that statistic
  #'
  #' @param
  #' detail:             a datatable with one row per data line
  #' displayParameters:  a table of display parameters to be used (see ShinyHelperFunctions)
  #'
  #' @description
  #' This function is meant to be used for container based statistics
  #' These are statistics where the unit of observation is the container
  #' At time of coding, the only such statistic was the Weight Ratio
  #' Adds descriptive columns to the data table
  #' Selects and orders the data columns for display purposes
  #' Formats the values according the statistic being displayed
  #' Creates a total row which appears as the first row of the display
  GetContainerDetailTable <- function(detail, displayParameters) {
    # attach descriptive columns
    # Samplers
    l <-
      GetDataObject("samplingLocation")[, .(SamplingLocationID, Sampler = Name)]
    detail <-
      merge(detail, l, by = "SamplingLocationID", all.x = TRUE)
    rm(l)
    
    # containers
    c <-
      GetDataObject("sampleContainer")[, .(SampleID,
                                           ContainerNumber,
                                           ContGr,
                                           ContainerTypeName)]
    detail <-
      merge(detail,
            c,
            by = c("SampleID", "ContainerNumber"),
            all.x = TRUE)
    rm(c)
    
    # sort rows
    detail <- detail[order(SampleID, ContainerNumber)]
    
    # add row names
    detail[, ShinyRowName := as.character(.I)]
    
    # add a total row
    total <-
      detail[, .(
        units = sum(units),
        gr = sum(gr),
        ContGr = sum(ContGr),
        ShinyRowName = "Total"
      )]
    detail <- rbind(total, detail, fill = TRUE)
    
    # select and order column names to retain
    colNames <- c("ShinyRowName", "SampleID", "ContainerNumber", "ContainerTypeName", "units", "gr", "ContGr", "x")
    detail <- detail[, ..colNames]

    # ordered list of statistical variables in the report
    colDisplayNames <- DTGetDisplayColumns(statisticID = statisticID,
                                           variables = colNames,
                                           type = "obs",
                                           displayParameters = displayParameters)
    colDisplayNames[1] <- ""
    colDisplayNames[2] <- "Sample ID"
    colDisplayNames[3] <- "Container #"
    colDisplayNames[4] <- "Container Type"
    
    dt <- datatable(
      detail,
      colname = colDisplayNames,
      style = "default",
      rownames = FALSE,
      escape = TRUE,
      extensions = 'Buttons',
      options = list(
        dom = 'Bt',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = -1L
      )
    ) %>%
      formatStyle(
        columns = "ShinyRowName",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
    
    dt <- DTPostProcess(shinyTable = dt,
                        statisticID = statisticID,
                        type = "sum",
                        displayParameters = displayParameters)
    
    
    return(dt)
  }
  
  #' @name GetProductDetailTable
  #'
  #' @return
  #' a DT table formatted for presentation in shiny containing one line for each
  #' line of data contributing to that statistic
  #'
  #' @param
  #' detail:             a datatable with one row per data line
  #' displayParameters:  a table of display parameters to be used (see ShinyHelperFunctions)
  #'
  #' @description
  #' This function is meant to be used for product based statistics
  #' Adds descriptive columns to the data table
  #' Selects and orders the data columns for display purposes
  #' Formats the values according the statistic being displayed
  #' Creates a total row which appears as the first row of the display
  GetProductDetailTable <-
    function(detail, statisticID, displayParameters)
    {
      # attach descriptive columns
      # Samplers
      l <-
        GetDataObject("samplingLocation")[, .(SamplingLocationID, Sampler = Name)]
      detail <-
        merge(detail, l, by = "SamplingLocationID", all.x = TRUE)
      rm(l)
      
      # Products
      p <-
        GetDataObject("product")[, .(ProductID, SubproductID, Product = FullName)]
      detail <-
        merge(detail,
              p,
              by = c("ProductID", "SubproductID"),
              all.x = TRUE)
      rm(p)
      
      # sort rows
      detail <-
        detail[order(SampleID, ContainerNumber, SampleProductID)]
      
      # add row names
      detail[, ShinyRowName := as.character(.I)]
      
      # add a total row
      total <-
        detail[, .(
          obs = sum(obs),
          units = sum(units),
          gr = sum(gr),
          ShinyRowName = "Total"
        )]
      detail <- rbind(total, detail, fill = TRUE)

      # select and order column names to retain
      colNames <- c("ShinyRowName", "SampleID", "ContainerNumber", 
                    "SampleProductID", "Product", "obs", "units",
                    "gr", "x")
      detail <- detail[, ..colNames]

      
      displayColumnNames <- DTGetDisplayColumns(statisticID = statisticID,
                                                variables = colNames, 
                                                type = "obs", 
                                                displayParameters = displayParameters)
      displayColumnNames[1] <- ""
      displayColumnNames[2] <- "Sample ID"
      displayColumnNames[3] <- "Container #"
      displayColumnNames[4] <- "Sample Product ID"
      displayColumnNames[5] <- "Product"

      dt <- datatable(
        detail,
        colname = displayColumnNames,
        style = "default",
        rownames = FALSE,
        escape = TRUE,
        extensions = 'Buttons',
        options = list(
          dom = 'Bt',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = -1L
        )
      ) %>%
        formatStyle(
          columns = "ShinyRowName",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
      dt <- DTPostProcess(shinyTable = dt,
                          statisticID = statisticID,
                          type = "sum",
                          displayParameters = displayParameters)
      return(dt)
    }
  
  # Main Entry Point -----------------------------------------------------------------------------------------------------------------------------------------
  
  # convert user input parameters to IDs
  stratumID = as.integer(stratumSelection)
  nodeID = as.integer(nodeSelection)
  statisticID <- as.integer(statSelection)
  
  # get the requested observations
  nodeObs <- GetDataObject("nodeObs")[NodeID == nodeID &
                                        StratumID == stratumID &
                                        StatisticID == statisticID]
  
  # determine which descriptive statistics are available for this node/statistic combo and calculate
  nodeStats <-
    GetDataObject("nodeStatistic")[NodeID == nodeID &
                                     StatisticID == statisticID]  # should retrieve a single record since the unique key is specified
  meanFlag <- as.logical(nodeStats[1, MeanAvailable])
  stDevFlag <- as.logical(nodeStats[1, StDevAvailable])
  
  # calculate the summary statistics - use same column names as in MultiStratumTable
  summary <- nodeObs[, .(
    datalines = sum(datalines),
    obs = sum(obs),
    units = sum(units),
    gr = sum(gr),
    meanAvailable = ifelse(meanFlag, "Yes", "No"),
    stDevAvailable = ifelse(stDevFlag, "Yes", "No"),
    mean = ifelse(meanFlag, sum(x) / sum(obs), NA),
    var = ifelse(stDevFlag, var(x), NA),
    stdDev = ifelse(stDevFlag, sd(x), NA)
  )]
  summary[, lowerCI := ifelse(stDevFlag, lowerCI(mean, stdDev, obs), NA)]
  summary[, upperCI := ifelse(stDevFlag, upperCI(mean, stdDev, obs), NA)]
  summary[, CIPct := ifelse(stDevFlag, (upperCI - mean) / mean, NA)]
  
  # convert to Shiny DT tables
  displayParameters <-
    DTImportDisplayParameters()[StatisticID == statisticID]   # more efficient to retrieve them just once
  detailTable <-
    GetDetailTable(nodeObs, statisticID, displayParameters)
  summaryTable <-
    GetSummaryTable(summary, statisticID, displayParameters)
  reports <-
    list(detailTable = detailTable, summaryTable = summaryTable)
  save(reports, file = "temp")
  return(reports)
}