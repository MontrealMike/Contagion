
source("R/UtilityFunctions.R")
source("R/ShinyHelperFunctions.R")

pdfTableTest <- function(){
  nodeSelection <- "1000"
  level <- -1
  geoSelection  = "C1"
  timeSelection = "Y-2016"
  weighting = "forecast"
  # includeOESFlag = FALSE
  drk <- GetDetailReportKable(nodeSelection, level, geoSelection, timeSelection, weighting)
  return(drk)
}

pdfRenderTest <- function() {
  rmarkdown::render(input = "./markdown/qrtest.Rmd",
                    params = list(time = "Y-2016",
                                  weighting = "forecast")
                    )
}

GetDetailReportKable <- function(nodeSelection, level, geoSelection, timeSelection, weighting) {
  
  # main entry point ---------------------------------------------------------
  
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
  statsList <- c(1, 2, 6, 7, 3, 8)
  
  # get the data
  source("R/PrepareMultistratStats.R")
  reportData <- MultiStratumTable(nodeSelectionTable = nodeList,
                                  stratumSelectionTable = msl, 
                                  statisticList = statsList,
                                  weighting = weighting)
  
  # shape and convert the data
  setkey(nodeTable, NodeID)
  setkey(reportData, NodeID)
  
  # Initial columns
  contents <- reportData[StatisticID == 1, 
                         .(NodeID, units, gr)]
  setkey(contents, NodeID)
  contents <- contents[nodeTable[, .(NodeID, name = shortname, level, sortOrder)]]
  
  # Statistic columns - convert the statisticID variable to columns
  meanCols <- reshape(reportData[StatisticID %in% statsList, .(NodeID, StatisticID, meanCol = mean)],
                      idvar = "NodeID", timevar = "StatisticID", direction = "wide")
  setkey(meanCols, NodeID)
  contents <- merge(contents, meanCols, by = "NodeID")

  # row and column ordering
  contents <- contents[order(sortOrder)]
  colNames <- c("name", "units", "gr", paste0("meanCol.", statsList), "level")
  contents <- contents[ , ..colNames]  # select and order columns
  
  # extract hierarchy level and remove column
  levels <- contents$level 
  contents[ ,level := NULL]
  
  # Prepare report columns specs
  varNames <- c("Product", "units", "gr", rep("mean", times = length(statsList)))
  statisticID <- c(NA, 1, 1, statsList)
  
  # Set column names, column formats and Scale data to standard units
  library(scales)
  displayParameters <- DTImportDisplayParameters()
  columnAlignmentVector <- character(length(varNames))
  for (col in 1:length(statisticID)) {
    dp <- DTGetDisplayParameters(statisticID = statisticID[[col]], variableName = varNames[[col]], displayParameters = displayParameters)
    if (length(dp) > 0) {
      columnAlignmentVector[col] <- "r"
      names(contents)[col] <- dp$DisplayName # column name
      switch(dp$FormatType,
             "C" = {
               contents[[names(contents)[col]]] <- comma(
                 contents[[names(contents)[col]]],
                 scale = dp$ConversionFactor,
                 accuracy = dp$Accuracy)
             },
             "P" = {
               contents[[names(contents)[col]]] <- percent(
                 contents[[names(contents)[col]]],
                 accuracy = dp$Accuracy)
             }
      )
    } else {
      columnAlignmentVector[col] <- "l"
    }
  }
  
  
  # set the caption for the report
  caption = DTGetReportCaption(mainTitle = "Detail Report",  
                               geoSelection = geoSelection, 
                               timeSelection = timeSelection,
                               weighting = weighting,
                               pdfCaption = TRUE)

  # format the data into Kable
  library(kableExtra)
  
  # this may generate a warning for levels that are not found in the actual data
  # the warning is harmless
  tbl <- kable(contents,
               format = "latex",
               booktabs = T,
               align = columnAlignmentVector,
               caption = "Detail Report") %>%
    kable_styling(latex_options = c("scale_down"),
                  font_size = 10) %>%
    row_spec(which(levels==1), bold = T, background ="#90EE90", font_size = 10) %>%
    row_spec(which(levels==2), bold = F, background ="#00FA9A", font_size = 9) %>%
    row_spec(which(levels==3), bold = F, font_size = 8) %>%
    row_spec(which(levels==4), bold = F, font_size = 7) %>%
    row_spec(which(levels==5), bold = F, italic = T, font_size = 7) %>%
    add_indent(which(levels>2)) %>%
    add_indent(which(levels>3)) %>%
    add_indent(which(levels>4))

  return(tbl)
}
