#' @name  LoadData
#' @author Michael Bleau
#'
#' Loads data from text files or through ODBC connections to a database
#' 
#' 2017-06-14 (MRB)
#'  - added loading SamplingOrganization
#'  - added loading SamplingLocation
#'  
#'  2018-08-15 (MRB)
#'   - added unicode encoding parameter to odbc and read.csv

require("RODBC", quietly = TRUE)
require("data.table", quietly = TRUE)
source("R/UtilityFunctions.R")

LoadEPRASampleData <- function(forcetxt = FALSE){
  
  GetEPRAConnection <- function(){
    #connection <- odbcDriverConnect('driver={SQL Server};server=10.252.30.4;database=EPRASAMPUAT;Uid=webuser;Pwd=Password1')
    #connection <- odbcDriverConnect('driver={SQL Server};server=10.252.30.4;database=EPRASAMPTEST;Uid=webuser;Pwd=Password1')
    #connection <- odbcDriverConnect('driver={SQL Server};server=10.252.30.4;database=EPRASAMPPROD;Uid=webuser;Pwd=Password1')
    # PRODconnection <- odbcDriverConnect('driver={SQL Server};server=10.252.5.20;database=EPRASAMPPRODN;Uid=webuser;DBMSencoding = "ISO-8859-1";Pwd=cAj2@*@Su5')
    connection <- odbcDriverConnect('driver={SQL Server};server=10.252.50.6;database=EPRASAMPPRODN1;DBMSencoding = "ISO-8859-1";Uid=webuser;Pwd=Password1')
    return(connection)
  }
  
  CloseEPRAConnection <- function(connection){
    if (!is.null(connection))
      odbcClose(connection)
  }
  
  LoadTable <- function(subjectarea, connection = NULL, path = ""){
    if (is.null(connection)){
      filename = paste(path, "ToR-", subjectarea, ".txt", sep = "")
      t <- as.data.table(read.csv(filename, as.is=TRUE, fileEncoding = "ISO-8859-1"))
    } else {
      # sql <- GetEPRASQL(subjectarea)
      filename = paste(path, "ToR-", subjectarea, ".sql", sep = "")
      sql <- readChar(filename, file.info(filename)$size)
      t <- as.data.table(sqlQuery(connection, sql, stringsAsFactors = FALSE))
    }
    return(t)
  }

  # main entry point
  os <- getOS()
  if(os == "linux" & !forcetxt) {
    connection = NULL
  } else if(os == "windows"){
    connection <- GetEPRAConnection()
  } else {
    connection = NULL
  }
  path = "import/"
    
  eprarawdata <- list(brand = LoadTable("Brand", connection, path),
                      hierarchy = LoadTable("Hierarchy", connection, path),
                      product = LoadTable("Product", connection, path),
                      productStatistic = LoadTable("ProductStatistic", connection, path),
                      quarter = LoadTable("Quarter", connection, path),
                      sample = LoadTable("Sample", connection, path),
                      sampleContainer = LoadTable("SampleContainer", connection, path),
                      sampleProduct = LoadTable("SampleProduct", connection, path),
                      statistic = LoadTable("Statistic", connection, path),
                      # statStratum = LoadTable("StatStratum", connection, path),
                      stratum = LoadTable("Stratum", connection, path),
                      subprogram = LoadTable("Subprogram", connection, path),
                      samplingOrganization = LoadTable("SamplingOrganization", connection, path),
                      samplingLocation = LoadTable("SamplingLocation", connection, path)
  )
  CloseEPRAConnection(connection)
  return(eprarawdata)
}