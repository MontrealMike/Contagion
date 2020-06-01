#' EPRASampling
#'
#' Michael Bleau
#' 2015-03-02
#
#' Functions to load, enhance and save database data in RData format
#' 
#' 2016-06-14 (MRB)
#'  - addded changes to load sampleLocation and sampleOrganization tables
#'  - added changes to enable calculation of statistics by sample location
#'
#' 2017-11-26 (MRB)
#'  - added stratumID field to sample, sampleProduct and sampleContainer
#'  - this turns out to simplify many later selections
#'
#' 2018-02-01 (MRB)
#'  - eliminate superroot records from NodeLeafnode table
#'  
#' 2018-08-10 (MRB)
#'   - return timing data
#'   
#' 2018-10-12 (MRB)
#'   - added option to strip out OES data - defaults to true
#'   
#' 2018-10-23 (MRB)
#'   - modified EnhanceQuarter function to give half values to quarter table



PrepareData  <- function(removeOES = TRUE) {
  
  #' @section Data Validation
  #' @description functions starting with 'Validate' check the imported data for validity and returns a list of errors
  
  #' @name GetValidateErrorTable
  #' 
  #' @description Creates an empty data table formatted to receive data errors
  #' Table columns are: table - the name of the table in which the error is found
  #'                    desc  - an error message
  #'                    ID    - the ID of the record which caused the error (this could be left blank)
  #'                    
  #' @return An empty data.table in standard format for inputErrors                    
  GetValidateErrorTable <- function(){
    errorTable <- data.table(table = character(), desc = character(), ID = integer())
  }  
  
  #' @name ValidateIDs
  #' 
  #' @param 
  #'  tableName - character string with the table name that will be added to any error message
  #'  IDColumn - vector of ID values
  #' 
  #' @description The rules for a valid ID field are the same for all tables.
  #' - The field must contain integers
  #' - Values must be greater than 0
  #' - There must not be any NAs or duplicates
  #' 
  #' @return an error table (in standard format) populated with any found errors
  ValidateIDs <- function(tableName, IDColumn){
    errors <- GetValidateErrorTable()
    
    # Make sure IDs are integer
    if (!is.integer(IDColumn)){
      errors <- rbind(errors, data.table(tableName, 
                                         "IDs are not integer",
                                         ID = NA))
    }
    
    # There are no NAs
    if (length(IDColumn[is.na(IDColumn)]) > 0) {
      errors <- rbind(errors, data.table(tableName, 
                                         "There are IDs with NA values",
                                         ID = NA))
    }
    
    # Values are > 0
    if (length(IDColumn[IDColumn < 1 & !is.na(IDColumn)]) > 0) {
      errors <- rbind(errors, data.table(tableName, 
                                         "There are IDs with 0 or negative values",
                                         ID = NA))
    }
    
    # Make sure each id key is unique
    dups <- duplicated(IDColumn)
    if (length(dups[dups == TRUE]) > 0) {
      errors <- rbind(errors, data.table(table = tableName,
                                         desc = "One or more duplicate ID numbers",
                                         ID = first(IDColumn[dups])))
    }
    return(errors)
  }
  
  #' @name ValidateHierarchyInterity
  #' 
  #' @param hierarchy - the hierarchy table as imported
  #' 
  #' @description validates the integrity of the data structure:
  #' - There must be one and only one super root
  #' - The super root cannot have a parent (ParentID == Null)
  #' - Each direct child of the super root starts a master hierarchie - all descendants of a 
  #'   master hierarchy must point to that master using the masterHierarchyID fields
  #' - For each hierarchy row (except super root), the chain of parentIDs must lead to the 
  #'   super root
  #' - Only leaf rows (those with no children) can point at a product
  #' - Only children attached to products (ProductID is not null) may have as parent a record with an OrphanProductFlag set)
  #' - There must be only one Orphan product record in a hierarchy and 
  #'   it must be a direct child of the master Hierarchy (ParentID == MasterHierarchyID).
  #' @return an error table (in standard format) populated with any found errors
  ValidateHierarchyIntegrity <- function(hierarchy)  {
    
    # check for valid ID - if errors are found, stop here.  No point in checking the rest
    errors <- ValidateIDs("hierarchy", hierarchy$ID)
    if (nrow(errors) > 0) return(errors)
    
    # check for unique Superroot with a null Parent
    sr <- hierarchy[SuperRootFlag == 1]
    if (nrow(sr) == 0)
    {
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = "No super root record",
                                         ID = NA))
    } else if (nrow(sr) > 1)
    {
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = "Multiple super root records",
                                         ID = NA))
    } else if (!is.na(sr[1, ParentID]))
    {
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = "Super root ParentID should be NA",
                                         ID = sr[1, ID]))
    }
    
    # check orphan records for each master hierarchy
    mh <- hierarchy[ParentID == sr[1, ID]]
    setkey(mh, ID)
    directOrphans <- hierarchy[ParentID == masterHierarchyID & OrphanProductFlag == 1]
    setkey(directOrphans, masterHierarchyID)
    multipleOrphans <- directOrphans[ , .(count = .N), by = masterHierarchyID ][count > 1]
    if (nrow(multipleOrphans) > 0 ){
      moList <- paste(directOrphans[masterHierarchyID == multipleOrphans[1, masterHierarchyID], ID], collapse = "/")  # creates string of missing products
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = paste("Master hierarchy record has more than one orphan descendant: ", moList),
                                         ID = multipleOrphans[1, masterHierarchyID]))
    }
    
    missingOrphanRecords <- mh[!directOrphans] 
    if (nrow(missingOrphanRecords) > 0) {
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = "Master hierarchy record has no Orphan child record",
                                         ID = missingOrphanRecords[1, ID]))
    }
    
    indirectOrphanRecords <- hierarchy[ParentID != masterHierarchyID & OrphanProductFlag == 1]
    if (nrow(indirectOrphanRecords) > 0) {
      errors <- rbind(errors, data.table(table = "hierarchy",
                                         desc = "Record with Orphan Product Flag is not a direct descendant of a Master Hierarchy",
                                         ID = indirectOrphanRecords[1, ID]))
    }
    rm(sr, directOrphans, multipleOrphans, missingOrphanRecords, indirectOrphanRecords) # clean up a bit
    
    # add a temporary column to flag parent rows - default all but SuperRoot to false
    # when this flag is set to true, we know 
    #  1 - that this is a parent (not a leaf) and
    #  2 - that the hierarchy from this point up has been processed
    hierarchy[, IsParent := as.logical(SuperRootFlag == 1)]
    
    # Make sure each hierarchy item can be tracked up to super root
    for (i in 1:nrow(hierarchy))  # for each hierarchy record ...
    {
      # skip the superroot record
      if (hierarchy[i, SuperRootFlag] == 1) next();
      
      levelCount = 0
      this = hierarchy[i]
      
      while (TRUE)                        # ... follow the parentID chain ... 
      {
        # Meet the parent (yuck)
        parent = hierarchy[ID == this[1, ParentID]]
        
        # check for broken link or infinite loop (levels > 100)
        if (nrow(parent) == 0 | levelCount > 100) {
          errors <- rbind(errors, data.table(table = "hierarchy",
                                             desc = "damaged hierarchy",
                                             ID = this[1, ID]))
          break() # out of while loop
        }
        if (parent[1,SuperRootFlag] == 1) # ... until you reach super root (yeah) ...
        {
          break() # out of while loop
        }
        
        # a parent record is, by definition, not a leaf.  It should not point to a product ID
        if (!is.na(parent[1, ProductID])) { 
          errors <- rbind(errors, data.table(table = "hierarchy",
                                             desc = "parent should not point to product",
                                             ID = parent[1, ID]))
        }
        
        # a parent record is, by definition, not a leaf.  It cannot have the OrphanProduct flag set to 1
        if (parent[1, OrphanProductFlag] == 1 & is.null(this[1, ProductID])) { 
          errors <- rbind(errors, data.table(table = "hierarchy",
                                             desc = "non product record should be child of OrphanProductFlag",
                                             ID = this[1, ID]))
        }
        
        # a child must be a direct descendant of its masterHierarchy (ParentID == masterHierarchyID)
        if(this[1, masterHierarchyID] != parent[1, masterHierarchyID]) {
          errors <- rbind(errors, data.table(table = "hierarchy",
                                             desc = "Invalid masterHierarchyID",
                                             ID = this[1, ID]))
        }
        
        # see if we have already processed from here on up.  If so, skip the rest
        if (parent[1, IsParent] == TRUE) break()
        
        # otherwise set the parent flag in the hierarchy table
        hierarchy[ID == parent[1,ID], IsParent := TRUE]
        
        levelCount <- levelCount + 1
        this = parent
      }
      
    }
    return(errors)
  }
  
  #' @name ValidateHierarchyCompleteness
  #' 
  #' @param
  #'  hierarchy  - the hierarchy table
  #'  productIDs - a vector of all productIDs (may contain duplicates)
  #' 
  #' @description Ensures that hierarchies represent all products
  #' - level 0 of the hierarchy is the super root
  #' - each level 1 of the hierarchy is the start of a master hierarchy
  #' - there must be one and only one leaf in each master hierarchy for each productID
  #' 
  #' @return an error table (in standard format) populated with any found errors
  ValidateHierarchyCompleteness <- function(hierarchy, productIDs) {
    
    # the product table's unique key is ProductID, SubproductID so the ProductID column can contain duplicates
    # which we eliminate 
    productIDs <- unique(productIDs)
    # now we can check the productIDs - if they are invalid - no sense in going further
    errors <- ValidateIDs("product", productIDs)
    if (nrow(errors) > 0) return(errors)
    
    productIDs <- data.table(ProductID = productIDs) # converting to a data.table really helps with comparisons
    setkey(productIDs, ProductID)
    
    superrootID <- hierarchy[SuperRootFlag == 1, ID]
    
    # Iterate master hierarchies
    masterHierarchies <- hierarchy[ParentID == superrootID]
    for (i in 1:nrow(masterHierarchies))
    {
      # list all the products in a hierarchy
      masterProductIDs <- setkey(hierarchy[masterHierarchyID == masterHierarchies[i, ID] & !is.na(ProductID), 
                                           .(ID, ProductID)], ProductID)
      
      # check for duplicates
      dups <- duplicated(masterProductIDs[, ProductID])
      if (length(dups[dups == TRUE]) > 0) {
        dupProducts <- paste(masterProductIDs[dups == TRUE, ProductID], collapse = "/") # create string of duplicate products
        errors <- rbind(errors, data.table(table = "hierarchy",
                                           desc = paste("Master hierachy records duplicate productIDs: ", dupProducts),
                                           ID = masterHierarchies[i, ID]))
      } 
      
      
      # check for matching keys using awesome datatable feature
      invalidProductID <- masterProductIDs[!productIDs]
      if (nrow(invalidProductID)>0) {
        errors <- rbind(errors, data.table(table = "hierarchy",
                                           desc = "Master hierachy contains invalid ProductID values",
                                           ID = invalidProductID[1, ID]))
      }
      
      # check for completeness
      if (nrow(masterProductIDs) < nrow(productIDs)) {
        missingProducts <- paste(productIDs[!masterProductIDs, ProductID], collapse = "/")  # creates string of missing products
        errors <- rbind(errors, data.table(table = "hierarchy",
                                           desc = paste("Master hierachy is missing these ProductIDs: ", missingProducts),
                                           ID = masterHierarchies[i, ID]))
      }
    }
    return(errors)    
  }
  
  #' @name ValidateStratumTable
  #' 
  #' @param
  #'    sample - sample table
  #'    stratum - stratum table
  #'    
  #' @description validates that each sample record can be joined to a stratum record (unique key QuarterID, SubprogramdID)
  #' 
  #' @return an error table (in standard format) populated with any found errors
  ValidateStratumTable <-  function(sample, stratum) {
    errors <- GetValidateErrorTable()
    sampleStratum <- unique(sample[, .(QuarterID, SubprogramID)])
    setkey(sampleStratum, QuarterID, SubprogramID)
    setkey(stratum, QuarterID, SubprogramID)
    missingStratum <- sampleStratum[!stratum]
    
    if (nrow(missingStratum) > 0) {
      missingStratumList <- paste(missingStratum[, .(Stratum = paste0("Q", QuarterID, " S", SubprogramID))], sep = "/")
      errors <- rbind(errors, data.table(table = "stratum",
                                         desc = paste("Stratum table is missing these records: ", missingStratumList), 
                                         ID = NA))
    }
    return(errors)
  }
  
  
  #' @section Data enhancement
  
  EnhanceSample <- function(sample, stratum){
    
    # first convert dates to characters (in case they are considered factors) and flag the 
    # sample[, SampleDate := as.character(SampleDate)]
    # sample[, OrderDate := as.character(OrderDate)]
    # sample[, ExpectedArrivalDate := as.character(ExpectedArrivalDate)]
    
    sample[OrderDate == "", OrderDate := NA]
    sample[ExpectedArrivalDate == "", ExpectedArrivalDate := NA]
    
    # now make them dates
    sample[, SampleDate := as.Date(SampleDate)]
    sample[, OrderDate := as.Date(OrderDate)]
    sample[, ExpectedArrivalDate := as.Date(ExpectedArrivalDate)]
    setnames(sample, "OrderDate", "RequestDate")
    setnames(sample, "SamplingLocationId", "SamplingLocationID")
    
    # MRB 2017-11-24 add stratumID to sample record
    s <- stratum[, .(StratumID, SubprogramID, QuarterID)]
    sample <- merge(sample, s, by = c("SubprogramID", "QuarterID"))
    return(sample)
  }
  
  EnhanceSampleProduct <- function(sampleProduct, product, container, enhancedSample) {
    
    # the next lines of code were added as a result of changes to the sampling database (version 003.015 access)
    # the sampleProduct table now contains an OutlierFlag variable - outliers are eliminated from the table
    # and the OutlierFlag variable itself is removed to avoid confusion with the container OutlierFlag that
    # will be used in later steps
    # MB 2017-02-15
    sampleProduct <- sampleProduct[OutlierFlag == 0]
    
    # new line for sample QTY clean up of NA values November 27, 2017  Andrew Livinstone - Then commented out 12/5 for testing
    sampleProduct <- sampleProduct[is.na(SampleQty), SampleQty := 0]
    
    # sampleProduct <- sampleProduct[is.na(OutlierFlag)]
    sampleProduct[,OutlierFlag := NULL]
    
    container <- merge(container, enhancedSample, by = "SampleID")
    
    # new line for container clean up of NA outlier flags November 24, 2017 Andrew Livinstone - Then commented out 12/5 for testing
    container[is.na(OutlierFlag), OutlierFlag := 0]
    
    #        sampleProduct <- sampleProduct[ProductID != 581] # debug
    
    # setkey(sampleProduct, SampleID, ContainerNumber)  #AL adding key for dt
    # setkey(container, SampleID, ContainerNumber)      #AL adding key for dt
    # sampleProduct <- sampleProduct[container, nomatch=0] #AL Merge of SampleProduct,Container using DT functions  Attempt 2 (inner join)
    # #sampleProduct <- sampleProduct[container] #AL Merge of SampleProduct,Container using DT functions  Attempt 1 (Right outer join)
    sampleProduct <- merge(sampleProduct, container, by=c("SampleID", "ContainerNumber"))  # AL Original MIke Code
    # sampleProduct <- sampleProduct[OutlierFlag == 0]  # replaced with next line MB 2017-01-06
    sampleProduct <- sampleProduct[OutlierFlag == 0 & Gr > 0]  # ignore outliers and 0 weight observations
    sampleProduct[MfgDate == "", MfgDate := NA]
    sampleProduct[, MfgDate := as.Date(MfgDate)]
    sampleProduct[, SampleQty := as.integer(SampleQty)]
    
    # fix for ProductSample record with SampleProductID == 127371
    # This record has SampleQty == na due to an unexplained erroneous entry in sample worksheet 643 (file name: BC_Co_2016_4.xlsx)
    # the next code line sets the SampleQty to 1.  
    # Note that this line of code will NOT match access reports.  To do so, the line following it should be uncommented.
    # Should the users decide to make an outlier of it, then this line will not have any effect since the 
    # record will have been eliminated in a previous step (line 52 at time of writing)
    # Should the users wish to correct and reload the worksheet then this line will have no effect since the is.na condition will be false.
    # MB 2017-02-17
    sampleProduct[SampleProductID == 127371 & is.na(SampleQty), SampleQty := 1]
    # sampleProduct[SampleProductID == 127371, SampleQty := 0] # debug - this compares to Access - but is wrong!
    
    # Kludgy fix for samples without BrandID
    sampleProduct[is.na(UpdatedBrandID), UpdatedBrandID := 16]      # 16 is the unknown brand
    
    # assign subproduct IDs
    sampleProduct <- DetermineSubproduct(product, sampleProduct)[,.(SampleProductID,
                                                                    ProductID,
                                                                    SubproductID,
                                                                    SampleID,
                                                                    ContainerNumber,
                                                                    StratumID,
                                                                    QuarterID,
                                                                    SubprogramID,
                                                                    SamplingLocationID,
                                                                    SampleQty,
                                                                    Gr,
                                                                    DiagonalCm,
                                                                    SampleDate,
                                                                    MfgDate,
                                                                    BrandID = UpdatedBrandID,
                                                                    DamageScore,
                                                                    CannibalizationScore)]
  }
  
  EnhanceStratum <- function(stratum, enhancedQuarter, subprogram, esampleContainer, sample) {
    strat <- merge(enhancedQuarter, stratum, by ="QuarterID")
    strat <- merge(subprogram, strat, by = "SubprogramID")
    strat[ , FullName := paste(ProgramName, SubprogramName, "-", PeriodName)]
    
    # calculate weighting factors
    # there is currently a bug in the access database that causes outlier containers to be erroneously considered
    # in the calculation of factors
    # the following factors will therefore be calculated
    # fcstWeighting, actWeighting, unWeighting, fcstWeightingBug, actWeightingBug
    # TDS: To Date Sample
    
    contSample <- merge(esampleContainer, sample, by=c("SampleID", "QuarterID", "SubprogramID"))
    # prodContSample <- merge(sampleProduct, contSample, by=c("SampleID", "ContainerNumber"), suffixes=c(".prod", ".cont"))
    
    # This code replaced by the code below.  This code for reasons I have not fully figured out double counts some Weight (TDSGr)
    # which affects the calculation of weighting factors
    # MB 2017-01-08
    # cont <- prodContSample[ , 
    #                         .(units = sum(SampleQty),
    #                           dataLines = .N,
    #                           TDSGr = sum(Gr)
    #                         ),
    #                         by = .(SampleID, ContainerNumber, QuarterID, SubprogramID)]
    # 
    # sq <- cont[,
    #            .(units = sum(units),
    #              containers = .N,
    #              dataLines = sum(dataLines),
    #              TDSGr = sum(TDSGr)
    #            ),
    #            by = .(QuarterID, SubprogramID)]
    
    # NA Container units fix for SK Q3 variance Andrew LIvingstone 11/25/2017
    contSample[is.na(ContUnits), ContUnits := 0]
    
    sq <- contSample[,
                     .(units = sum(ContUnits),
                       containers = .N,
                       dataLines = sum(dataLines),
                       TDSGr = sum(ContGr)
                     ),
                     by = .(QuarterID, SubprogramID)]
    strat <- merge(strat, sq, by = c("QuarterID", "SubprogramID"), all.x = TRUE)
    
    strat[,
          ':=' (
            fcstWeighting = FcstTonnes * 1000 * 1000 / TDSGr,
            actWeighting = ActualTonnes * 1000 * 1000 / TDSGr,
            unWeighting = 1
          )]
    return(strat)
  }
  
  EnhanceStratumOld <- function(stratum, quarter, subprogram, sampleProduct, SampleContainer, sample) {
    strat <- merge(quarter, stratum, by ="QuarterID")
    strat <- merge(subprogram, strat, by = "SubprogramID")
    strat[ , FullName := paste(ProgramName, SubprogramName, "-", PeriodName)]
    
    # calculate weighting factors
    # there is currently a bug in the access database that causes outlier containers to be erroneously considered
    # in the calculation of factors
    # the following factors will therefore be calculated
    # fcstWeighting, actWeighting, unWeighting, fcstWeightingBug, actWeightingBug
    # TDS: To Date Sample
    
    contSample <- merge(sampleContainer, sample, by="SampleID")
    prodContSample <- merge(sampleProduct, contSample, by=c("SampleID", "ContainerNumber"), suffixes=c(".prod", ".cont"))
    cont <- prodContSample[ , 
                            .(units = sum(SampleQty),
                              dataLines = .N,
                              TDSGr = sum(Gr * (1 - OutlierFlag)),
                              TDSGrBug = sum(Gr)),
                            by = .(SampleID, ContainerNumber, QuarterID, SubprogramID)]
    
    sq <- cont[,
               .(units = sum(units),
                 containers = .N,
                 dataLines = sum(dataLines),
                 TDSGr = sum(TDSGr),
                 TDSGrBug = sum(TDSGrBug)),
               by = .(QuarterID, SubprogramID)]
    strat <- merge(strat, sq, by = c("QuarterID", "SubprogramID"), all.x = TRUE)
    
    strat[,
          ':=' (
            fcstWeighting = FcstTonnes * 1000 * 1000 / TDSGr,
            fcstWeightingBug = FcstTonnes * 1000 * 1000 / TDSGrBug,
            actWeighting = ActualTonnes * 1000 * 1000 / TDSGr,
            actWeightingBug = ActualTonnes * 1000 * 1000 / TDSGrBug,
            unWeighting = 1
          )]
    return(strat)
  }
  
  EnhanceProduct <- function(product) {
    setnames(product, old="ProductSubCategoryRangeID", new = "SubproductID")  
    product[, FullName := paste(ProductName, SubcategoryDescription)]
    product[, LowerRangeLimit := -0.00001] 
    product <- product[order(ProductID, RangeLimit)]
    
    for (i in 2:nrow(product)){
      if(product[i - 1, ProductID] == product[i, ProductID]){
        lrl  <- product[i - 1, RangeLimit]
        product[i, LowerRangeLimit := lrl]
      }
    }
    return(product)
  }
  
  EnhanceProductStatistic <- function(productStatistic) {
    setnames(productStatistic, old="ProductId", new = "ProductID")
    #        productStatistic <- productStatistic[ProductID != 581] # debug
  }
  
  EnhanceBrand <- function(brand){
    # brand$BrandID <- factor(brand$BrandID, labels = brand$NameE)
    return(brand)
  }
  
  EnhanceQuarter <- function(quarter){
    #quarter$PeriodStartDate <- as.Date(quarter$PeriodStartDate, format = "YYYY%MM%DD%HH%MM%SS%MMM")
    #quarter$PeriodEndDate <- as.Date(quarter$PeriodEndDate, format = "YYYY%MM%DD%HH%MM%SS%MMM")
    quarter$PeriodStartDate <- as.Date(quarter$PeriodStartDate)
    quarter$PeriodEndDate <- as.Date(quarter$PeriodEndDate)    
    quarter[, Year := as.integer(format(PeriodStartDate, format = "%Y"))]
    quarter[, QuarterValue := quarter(PeriodStartDate)]
    quarter[, HalfValue := as.integer((QuarterValue - 1) / 2) + 1]
    quarter[, QuarterName := paste0(Year, "-Q", QuarterValue)]
    quarter[, HalfName := paste0(Year, "-H", HalfValue)]
    return(quarter)
  }
  
  EnhanceSubprogram <- function(subprogram){
    program <- subprogram[,.(SubprogramCount = .N), by=ProgramID]
    subprogram <- merge(program, subprogram, by = "ProgramID")
    subprogram[, FullName := ifelse(SubprogramCount > 1, 
                                    paste(ProgramShortname, SubprogramShortname),
                                    ProgramShortname)
               ]
    return(subprogram)
  }
  
  EnhanceStatistic <- function(statistic){
    # add rows to statistics table for statistics not managed in the EPRA database
    cwtStatID <- 200L
    if (nrow(statistic[StatisticID == cwtStatID])) {
      cat(paste("Error - adding Container Weight statistic ID, ID", cwtStatID, "already used."))
      return(statistic)
    }
    
    statRow <- data.table(StatisticID = cwtStatID,
                          NameE = "Container weight",
                          NameF = "",
                          DescriptionE = "Average container wt (Kg)",
                          DescriptionF = "",
                          FormatE = "Standard",
                          FormatF = "Standard",
                          AppliesTo = "Container",
                          uomID = 1L,
                          StatisticType = "Continuous")
    es = rbind(statistic, statRow)
    # es$StatisticID <- factor(es$StatisticID, labels = es$NameE)
    return(es)
  }
  
  EnhanceContainer <- function(container, enhancedsample, enhancedsampleProduct) {
    container <- container[OutlierFlag == 0]  # remove outliers
    # setkey(container, SampleID, ContainerNumber)
    # setkey(enhancedsample, SampleID)
    container <- merge(enhancedsample[, .(SampleID, StratumID, SubprogramID, QuarterID, SamplingLocationID)], container, by = "SampleID")
    
    # this gets the actually sampled wt for each container
    setkey(enhancedsampleProduct, SampleID, ContainerNumber)
    # replaced with next line MB 2017-01-08 (See changes in Enhance Stratum for further details)  
    # cSummaryStats <- enhancedsampleProduct[, .(ContGr = sum(Gr), ContUnits = sum(SampleQty) ), by = c("SampleID", "ContainerNumber")]
    cSummaryStats <- enhancedsampleProduct[, .(ContGr = sum(Gr), ContUnits = sum(SampleQty), dataLines = .N ), by = c("SampleID", "ContainerNumber")]
    
    setkey(container, SampleID, ContainerNumber)
    container <- merge(container, cSummaryStats, all.x = TRUE, by = c("SampleID", "ContainerNumber"))
    
    # eliminates containers with no productSample data.Ideally, these should not exists or at least be flagged as outliers
    # This code at time of writing only applied to a single container: See WorksheetID 288, SampleID 496, Container # 3.  MB 2017-01-27
    # This code replaces the following 3 code lines
    container <- container[!is.na(dataLines)]  # remove outliers
    
    # convert na values to 0 to cope with case where container is empty.  See WorksheetID 288, SampleID 496, Container # 3.  MB 2017-01-27
    # container[is.na(ContGr), ContGr := 0]
    # container[is.na(ContUnits), ContUnits := 0]
    # container[is.na(dataLines), dataLines := 0] 
    
    return(container)
  }
  
  CreateContainerType <- function(container){
    containerType <- as.data.table(unique(container[, .(ContainerTypeID, ContainerTypeName)]))
    return(containerType)
  }
  
  #' @Section Dynamic categories - Nodes and sub products
  DetermineSubproduct <- function(prod, prodSample){
    ps <- merge(x = prod, y = prodSample, by = "ProductID", all.y = TRUE, allow.cartesian = TRUE)
    
    # eliminate null productID values (corrupt database data)
    ps <- ps[!is.na(ProductID)]
    
    # classify by type of subproduct
    noSubPS <- ps[AppliesTo == "None"]
    
    # 0 diagonals should be treated as missing in the Access database but they are not
    diagonalPS <- ps[AppliesTo == "Diagonal" & ((DiagonalCm > LowerRangeLimit & DiagonalCm <= RangeLimit) | (LowerRangeLimit == 0 & DiagonalCm == 0))]
    weightPS <- ps[AppliesTo == "Weight" & Gr > LowerRangeLimit & Gr <= RangeLimit]       
    
    ps <- rbind(noSubPS, diagonalPS, weightPS)
    return(ps)
  }
  
  # expands the hierarchy to include dynamically determined subproducts
  GenerateNode <- function(hierarchy, product){
    #        product <- product[ProductID != 581] # todo investigate 581 it is evil
    setkey(hierarchy, ProductID)
    setkey(product, ProductID)
    
    # outer join products without subproducts
    pNone <- product[AppliesTo == "None"]
    hp <- pNone[hierarchy] # outer join
    hp[, NodeID := ID * 100]
    hp[, ParentnodeID := ParentID * 100]
    hp[, NodeName := as.character(NameE)]
    
    # add subproducts
    hs <- hierarchy[product[AppliesTo != "None"]]
    hs[, NodeID := ID * 100 + HashId]
    hs[, ParentnodeID := ID * 100]
    hs[, NodeName := as.character(paste(NameE, SubcategoryDescription))]
    hs[, SortOrder := HashId]
    
    node <- rbind(hp, hs)
    
    node <- node[, .(SuperRootFlag, ParentnodeID, NodeID, ProductID, SubproductID, NodeName, SortOrder, OrphanProductFlag)]
    # node$NodeID <- factor(node$NodeID, labels = node$NodeName)
    
    setkey(node, NodeID)
    return(node)
  }
  
  #' @name AddCustomHierarchiesToNode
  #' 
  #' @description loads custom hierachies from a text file 
  #'              and appends them to a node file
  #'              
  #' @param node a node table
  #' 
  #' @return an extended node table and an error table
  #' 
  #' @details if any load errors are reported, the original node table is returned
  
  AddCustomHierarchiesToNode <- function(node, enahancedproduct) {
    source("R/LoadCustomHierarchy.R")
    filename = CustomHierachyFile()
    return(LoadCustomHierarchy(node, enhancedproduct, filename))
  }
  
  # returns a nested list of nodes
  NodeData <- function(node) {
    
    Children <- function(nodeID) {
      c <- as.list(node[ParentnodeID == nodeID][, NodeID])
      c
    }
    
    # tricky recursive function to build tree
    # we must remember that each time we append to the tree to
    # increment the index
    NodeTree <- function(nodeID) {
      tree <- list()
      tree <- append(tree, as.integer(nodeID))
      children <- Children(nodeID)
      if (length(children > 0)) {
        for (i in 1:length(children)) {
          childTree <- NodeTree(children[[i]])
          if (length(childTree) > 0) {
            tree <- append(tree, list(childTree))
          }
        }
      }
      tree
    }
    
    PrintTree <- function(tree, level = 0) {
      for (i in 1:length(tree)) {
        if (is.list(tree[[i]])) {
          cat(" (")
          PrintTree(tree[[i]], level + 1)
          cat(") ")
        } else {
          cat(tree[[i]])
        }
      }
    }
    
    # returns a datable that, for each node, lists all it's subnodes
    NodeSubnodeBranch <- function(tree, level = 0) {
      ns <- data.table(NodeID = as.integer(), SubnodeID = as.integer())
      if (length(tree) > 1) {
        for (i in 2:length(tree)) {
          nsi <- data.table(SubnodeID = as.integer(unlist(tree[[i]])))
          nsi[, NodeID := as.integer(tree[[1]])]
          subns <- NodeSubnodeBranch(tree[[i]], level + 1)
          ns <- rbind(ns, nsi, subns)
        }
      }
      ns
    }
    
    # adds the leaf nodes to Node subnode
    NodeSubnodeLeaf <- function(n, ns) {
      ns <- merge(n, ns, by = "NodeID", all.x = TRUE, allow.cartesian = TRUE)
      ns[is.na(SubnodeID), SubnodeID := NodeID]
      # MRB 2018-02-01 eliminate Superoot records from NodeSubnode - they lead to confusing and
      # un-needed nodeObs records downstream
      # ns[, c("SuperRootFlag", "ParentnodeID") := NULL]
    }
    
    # returns a datable that, lists all the leaf nodes for a node
    NodeLeafnode <- function(node, nodeSubnode){
      leafNode <- node[!is.na(SubproductID), .(NodeID, ProductID, SubproductID)]
      
      # MRB 2018-02-01 eliminate Superoot records from NodeSubnode - they lead to confusing and
      # un-needed nodeObs records downstream
      # nodeSubnode <- nodeSubnode[,.(NodeID, SubnodeID)]
      nodeSubnode <- nodeSubnode[SuperRootFlag == 0, .(NodeID, SubnodeID)]
      setkey(leafNode, NodeID)
      setkey(nodeSubnode, SubnodeID)
      nln <- nodeSubnode[leafNode]
      nln <- nln[, .(NodeID, LeafnodeID = SubnodeID, ProductID, SubproductID)]
      nln
    }
    
    # find the root - assumes there is only one
    root <- node[SuperRootFlag == 1][, NodeID]
    tree <- NodeTree(root)
    #     print(PrintTree(tree))
    ns <- NodeSubnodeBranch(tree)
    ns <- NodeSubnodeLeaf(node, ns)
    nln <- NodeLeafnode(node, ns)
    nodeData <- list(nodeTree = tree, nodeSubnode = ns, nodeLeafnode = nln)
  }
  
  #' @name: GenerateNodeStatistic
  #' 
  #' for each node in the node hierarchy determines which statistics apply
  #' the statistics that apply are the lowest common denominator of the leaf nodes
  
  GenerateNodeStatistic <- function(node, productStatistic, statistic){
    # recursive function to traverse node hierarchies and build the node statistic table
    # stats available
    # the function gets to the leaf nodes and starts buiding the table from the leaf nodes up
    # the leaf nodes get the settings of the products they represent
    # higher nodes get the minumum setting of their combined immediate children nodes
    SetNodeStats <- function(nodeID, productStatistic, node){
      nodeStatistic <- data.table(NodeID = integer(), StatisticID = integer(), MeanAvailable = logical(), StDevAvailable = logical())         
      childNodes <- node[ParentnodeID == nodeID, NodeID]
      #          print(paste(nodeID, length(childNodes)))
      if (length(childNodes) > 0) {
        for (i in 1:length(childNodes)) {
          nodeStatistic <- rbind(nodeStatistic, SetNodeStats(childNodes[i], productStatistic, node))
        }
        myNodeStatistic <- nodeStatistic[NodeID %in% childNodes,
                                         .(
                                           NodeID = nodeID,
                                           MeanAvailable = as.logical(min(MeanAvailable)),
                                           StDevAvailable = as.logical(min(StDevAvailable))
                                         ),
                                         by = StatisticID]
      } else {
        myNodeStatistic <- productStatistic[ProductId == node[NodeID == nodeID, ProductID],
                                            .(
                                              NodeID = nodeID,
                                              MeanAvailable = as.logical(MeanAvailable),
                                              StDevAvailable = as.logical(StDevAvailable)
                                            ),
                                            by = StatisticID]
      }
      nodeStatistic <- rbind(myNodeStatistic, nodeStatistic)
    }
    
    # main entry point
    superRootID <- node[SuperRootFlag == 1, NodeID]
    nodeStatistic <- SetNodeStats(superRootID, productStatistic, node)
    nodeStatistic[NodeID == superRootID, MeanAvailable := FALSE]
    nodeStatistic[NodeID == superRootID, StDevAvailable := FALSE]
    #     nodeStatistic$NodeID <- factor(nodeStatistic$NodeID, levels = node$NodeID, labels = node$NodeName)
    #     nodeStatistic$StatisticID <- factor(nodeStatistic$StatisticID, levels = statistic$StatisticID, labels = statistic$NameE)
    return(nodeStatistic)
  }
  
  # removes OES data from all applicable data tables
  # WARNING: the hard-coded OESProgramID parameter determines which records will be deleted in this routine
  RemoveOESData <- function(rawdata, OESprogramID = 3) {
    
    # Get lists of IDs of records to be deleted
    OESsubprogramID <- rawdata$subprogram[ProgramID == OESprogramID, SubprogramID] # list of SubprogramID's to be deleted
    OESsampleID <- rawdata$sample[SubprogramID %in% OESsubprogramID, SampleID]     # list of SamppleID's to be deleted
    
    # delete the records from the applicable data tables using appropriate keys
    rawdata$sampleProduct <- rawdata$sampleProduct[!SampleID %in% OESsampleID]
    rawdata$sampleContainer <- rawdata$sampleContainer[!SampleID %in% OESsampleID]
    rawdata$sample <- rawdata$sample[!SampleID %in% OESsampleID]
    rawdata$stratum <- rawdata$stratum[!SubprogramID %in% OESsubprogramID]
    rawdata$subprogram <- rawdata$subprogram[ProgramID != OESprogramID]
    
    return(rawdata)
  }
  
  # Main ----------------------------------------------
  
  # eprarawdata is a list of data tables as loaded from the database
  source("R/LoadData.R")
  
  dataDateTimeStamp <- list(start = Sys.time(), end = Sys.time())
  eprarawdata <- LoadEPRASampleData()
  
  # validate data
  # the following function calls ensure the integrity of the imported data
  importErrors <- ValidateHierarchyIntegrity(eprarawdata$hierarchy)
  importErrors <- rbind(importErrors, ValidateHierarchyCompleteness(eprarawdata$hierarchy, eprarawdata$product$ProductID))
  importErrors <- rbind(importErrors, ValidateStratumTable(eprarawdata$sample, eprarawdata$stratum))
  
  # remove all OES data (if asked for)
  if (removeOES) {
    eprarawdata <- RemoveOESData(eprarawdata)    
  }
  
  # epradata is the raw data with a variety of enhancements to simplify analysis
  # the idea is that all analysis should use the epradata tables
  enhancedsample <- EnhanceSample(eprarawdata$sample, eprarawdata$stratum)
  enhancedproduct <- EnhanceProduct(eprarawdata$product)
  enhancedBrand <- EnhanceBrand(eprarawdata$brand)
  enhancedsampleProduct <- EnhanceSampleProduct(eprarawdata$sampleProduct, enhancedproduct, eprarawdata$sampleContainer, enhancedsample)
  enhancedsampleContainer <- EnhanceContainer(eprarawdata$sampleContainer, enhancedsample, enhancedsampleProduct)
  enhancedStatistic <- EnhanceStatistic(eprarawdata$statistic)
  containerType <- CreateContainerType(eprarawdata$sampleContainer)
  enhancedQuarter <- EnhanceQuarter(eprarawdata$quarter)
  
  # a lot of work to get the node data right
  originalNode <- GenerateNode(eprarawdata$hierarchy, enhancedproduct)
  expandedNodeData <- AddCustomHierarchiesToNode(originalNode, enhancedproduct)
  importErrors <- rbind(importErrors, expandedNodeData$errors)
  enhancednode <- expandedNodeData$node
  enhancednodeData <- NodeData(enhancednode)
  enhancednodeSubnode <- enhancednodeData$nodeSubnode
  enhancednodeTree <- enhancednodeData$nodeTree
  enhancednodeLeafnode <- enhancednodeData$nodeLeafnode
  enhancedNodeStatistic <- GenerateNodeStatistic(enhancednode, eprarawdata$productStatistic, eprarawdata$statistic)
  
  # if there are data validation errors - warn the user
  if (nrow(importErrors) > 0) {
    cat("\n")
    cat(" +---------------- WARNING ------------------------------------+\n")
    cat(" |  Errors were found in the imported data                     |\n")
    cat(" |  Check the Errors table in the data/EPRASamplingErrors file |\n")
    cat(" |  Use GetDataObject('importErrors')                          |\n")
    cat(" +-------------------------------------------------------------+\n")
  }
  dataDateTimeStamp$end <- Sys.time()
  save(importErrors, dataDateTimeStamp, file = EPRASamplingErrorFile())
  
  epradata <- list(brand = enhancedBrand,
                   hierarchy = eprarawdata$hierarchy,
                   product = enhancedproduct,
                   productStatistic = EnhanceProductStatistic(eprarawdata$productStatistic),
                   quarter = enhancedQuarter,
                   sample = enhancedsample,
                   sampleContainer = enhancedsampleContainer,
                   containerType = containerType,
                   sampleProduct = enhancedsampleProduct,
                   statistic = enhancedStatistic,
                   statStratum = eprarawdata$statStratum,
                   stratum = EnhanceStratum(eprarawdata$stratum, enhancedQuarter, eprarawdata$subprogram, 
                                            enhancedsampleContainer, eprarawdata$sample),
                   subprogram = EnhanceSubprogram(eprarawdata$subprogram),
                   node = enhancednode,
                   nodeSubnode = enhancednodeSubnode,
                   nodeTree = enhancednodeTree,
                   nodeLeafnode = enhancednodeLeafnode,
                   nodeStatistic = enhancedNodeStatistic,
                   samplingOrganization = eprarawdata$samplingOrganization,
                   samplingLocation = eprarawdata$samplingLocation
  )
  
  dataDateTimeStamp$end = Sys.time()
  save(eprarawdata, epradata, dataDateTimeStamp, file = EPRASamplingDataFile())
  rm(eprarawdata, epradata)
  return(dataDateTimeStamp)
}

