#' ------------------------------------------------------------------
#' @name LoadCustomHierarchy.R
#' 
#' @author Michael Bleau
#' @createdate 2018-01-17
#' 
#' @description 
#' Functions to load one or more custom product hierarchies, 
#' validate them and attach them to an existing node hierarchy table
#' 
#' 

#' @name lchTest
#' 
#' @description test the the code in this file
#' 
#' @return node table

lchTest <- function() {
  rm(list = ls())
  source("R/UtilityFunctions.R")
  
  # filename <- "import/CustomHierarchy.csv"
  filename <- "test/CustomHierarchyTest/Test 6c.csv"
  originalnodeTable <- GetDataObject("node")
  productTable <- GetDataObject("product")
  n <- LoadCustomHierarchy(originalnodeTable, productTable, filename)
}

#' @name LoadCustomHierarchy
#' 
#' @description loads a one (or more) custom hierarchies from a file 
#'              validates and appends to the provided node table
#'              
#' @param 
#'    node - the existing node table
#'    product - the product table
#'    filename - the name of the csv file containing the custom hierarchy
#' 
#' @return new node table with appended hierarchy
#' 
#' @details 
#'   the validation code in this program is very similar to validation code in the PrepareData.R file
#'   ideally, this code should somehow be merged to prevent duplication
#' 

LoadCustomHierarchy <- function(node, product, filename) {
  #' @name GetValidateErrorTable
  #' 
  #' @description Creates an empty data table formatted to receive data errors
  #' Table columns are: table - the name of the table in which the error is found
  #'                    desc  - an error message
  #'                    ID    - the ID of the record which caused the error (this could be left blank)
  #'                    
  #' @return An empty data.table in standard format for inputErrors        
  #'             
  GetValidateErrorTable <- function(){
    errorTable <- data.table(table = character(), desc = character(), ID = integer())
    return(errorTable)
  }  
  
  #' @name LoadValidFile
  #' 
  #' @param filename - the full pathname of the file to be loaded
  #' 
  #' @description reads the csv file and and validates for syntax
  #' 
  #' @return custom node table
  #' 
  #' @details 
  #'    - check for column count
  #'    - names columns - (file column names are ignored)
  #'    - check for duplicate node IDs
  #'    - check for broken hierarchies
  #'    - check for hierarchy loops (e.g. child to parent back to child)
  #'    - ProductID/SubproductID can only exist on leaf nodes
  #'    - ProductID must always be accompanied by SubproductID (and vice-versa)
  #'    - at least one root node (ParentnodeID = 0)
  #'    
  #'    File structure
  #'    - csv format
  #'    - 6 columns
  #'    - rows with all null contents are ignored (even if contents in quotes)
  #'    - First row is column names
  #'      - Column 1 - ParentnodeID - Integer
  #'      - Column 2 - NodeID - Integer
  #'      - Column 3 - NodeName - String
  #'      - Column 4 - SortOrder - Integer
  #'      - Column 5 - ProductID - Integer (0 means NA)
  #'      - Column 6 - SubproductID - Integer (0 means NA)
  LoadValidFile <- function(filename) {
    f <- as.data.table(read.csv(filename, stringsAsFactors = FALSE))
    
    errors <- GetValidateErrorTable()
    # check for column count
    if(ncol(f) != 6) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Wrong number of columns, expected 6",
                                         ID = NA))
      
      return(list(f = NA, errors = errors))             
    }
    
    # name columns
    colnames = c("ParentnodeID", "NodeID", "NodeName", 
                 "SortOrder", "ProductID", "SubproductID")
    names(f) <- colnames
    
    # check for non-integer ID fields
    niID <- f[is.na(as.integer(NodeID)) & NodeID != "", NodeID]
    if (length(niID) > 0) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Non-Integer NodeID found",
                                         ID = niID[[1]]))
    }
    niID <- f[is.na(as.integer(ParentnodeID)) & ParentnodeID != "", ParentnodeID]
    if (length(niID) > 0) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Non-Integer ParentnodeID found",
                                         ID = niID[[1]]))
    }
    
    # delete NA NodeID rows - this is not considered an error
    f[ , NodeID := as.integer(NodeID)]
    f <- f[!is.na(NodeID)]
    
    # replace 0s with NAs
    f <- f[ProductID == 0, ProductID := NA]
    f <- f[SubproductID == 0, SubproductID := NA]
    
    # check for duplicate nodeIDs
    dups <- anyDuplicated(f[ , .(NodeID)])
    if (dups > 0 ) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Duplicate NodeID found",
                                         ID = f[dups, NodeID]))
    }
    
    # check for minimum of 1 root node (ParentNodeID = 0)
    if (nrow(f[ParentnodeID == 0])  == 0) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "No root node found (with ParentnodeID = 0)",
                                         ID = NA))
    }
    
    # Product and Subproduct IDs must both have values or both be NA
    incompleteProducts <- f[is.na(ProductID) != is.na(SubproductID), NodeID]
    if (length(incompleteProducts) > 0) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Missing product or subproduct ID",
                                         ID = incompleteProducts[[1]]))
    }
    
    # a Node cannot have itself as a parent (duh)
    invalidParents <- f[ParentnodeID == NodeID, NodeID]
    if (length(invalidParents) > 0) {
      errors <- rbind(errors, data.table(table = filename,
                                         desc = "Node is its own parent",
                                         ID = invalidParents[[1]]))
    }
    
    # Make sure each node item can be tracked up to a root
    
    # add a temporary column to flag parent rows - default all but Root to false
    # when this flag is set to true, we know 
    #  1 - that this is a parent (not a leaf) and
    #  2 - that the hierarchy from this point up has been processed
    f[ , IsParent := as.logical(ParentnodeID == 0)]
    for (i in 1:nrow(f))  # for each node record ...
    {
      # skip the root(s)
      if (f[i, ParentnodeID] == 0) next()
      
      levelCount = 0
      this = f[i]
      
      while (TRUE)                        # ... follow the parentID chain ... 
      {
        # Meet the parent (yuck)
        parent = f[NodeID == this[1, ParentnodeID]]
        
        # check for broken link or infinite loop (levels > 100)
        if (nrow(parent) == 0 | levelCount > 100) {
          errors <- rbind(errors, data.table(table = filename,
                                             desc = "Damaged hierarchy", 
                                             ID = this[1, NodeID]))
          break() # out of while loop
        }
        
        if (parent[1, NodeID] == 0) # ... until you reach super root (yeah) ...
        {
          break() # out of while loop
        }
        
        # a parent record is, by definition, not a leaf.  It should not point to a product ID
        if (!is.na(parent[1, ProductID]) | !is.na(parent[1, SubproductID])) { 
          errors <- rbind(errors, data.table(table = filename,
                                             desc = "ProductID/SubproductID only valid in leaf nodes", 
                                             ID = parent[1, NodeID]))
        }
        
        # see if we have already processed from here on up.  If so, skip the rest
        if (parent[1, IsParent] == TRUE) break()
        
        # otherwise set the parent flag in the hierarchy table
        f[NodeID == parent[1, NodeID], IsParent := TRUE]
        
        levelCount <- levelCount + 1
        this = parent
      }
      
    }
    
    # delete temp column
    f[ , IsParent := NULL]
    return(list(f = f, errors = errors))
  }
  
  #' @name ValidateHierarchyCompleteness
  #' 
  #' @param
  #'  customHieracrhy  - the hierarchy table
  #'  product - a product table
  #' 
  #' @description Ensures that each hierarchie properly represents all valid product/subproduct combinations
  #' 
  #' @return an error table (in standard format) populated with any found errors
  #' 
  #' @details 
  #'  - check for duplicate product/subproduct combinations in each hierarchy
  #'  - check for product/subproduct combinations that do not exist in the product table
  #'  - check for missing product/subproduct combinations
  
  ValidateHierarchyCompleteness <- function (customHierarchy, product) {
    
    # recursive function to build a vector of leaf node ids for any nodeID
    # it assumes the nodeID is a branch
    GetLeaves <- function(hierarchy, nodeID, depth = 0) {
      # get the children
      children <- hierarchy[ParentnodeID == nodeID]
      leaves = NULL
      
      if (nrow(children) > 0) {
        for (i in 1:nrow(children) ) {
          if (!is.na(children[i, ProductID])) {
            # it is a leaf, add the NodeID to the list
            leaves <- c(leaves, children[i, NodeID])
          } else {
            if (depth < 10)
            {
              # it's a branch, go deeper
              leaves <- c(leaves, GetLeaves(hierarchy, children[i, NodeID], depth + 1))
            } else {
              print("????????????????????????????????????? Depth error ??????????????????????????????????????????????")
            }
          }
        }
      }
      return(leaves)
    }
    
    errors <- GetValidateErrorTable()
    setkey(product, ProductID, SubproductID)
    masterHierarchies <- customHierarchy[ParentnodeID == 0]
    
    for (i in nrow(masterHierarchies)) {
      leaves <- GetLeaves(customHierarchy, masterHierarchies[i, NodeID])
      leafTable <- customHierarchy[NodeID %in% leaves, .(ProductID, SubproductID)]
      setkey(leafTable, ProductID, SubproductID)
      
      # check for duplicates
      dups <- duplicated(leafTable[])
      if (length(dups[dups == TRUE]) > 0) {
        dupProducts <- paste(leafTable[dups == TRUE, ProductID], collapse = "/") # create string of duplicate products
        errors <- rbind(errors, data.table(table = "customHierarchy",
                                           desc = paste("Custom hierachy records duplicate productIDs: ", dupProducts),
                                           ID = masterHierarchies[i, NodeID]))
      } 
      
      
      # check for matching keys using awesome datatable feature
      invalidProductID <- leafTable[!product]
      if (nrow(invalidProductID)>0) {
        errors <- rbind(errors, data.table(table = "customHierarchy",
                                           desc = paste("Hierachy contains invalid ProductID/SubproductID values",
                                                        "ProductID: ", invalidProductID[1, ProductID], 
                                                        "SubproductID", invalidProductID[1, SubproductID]),
                                           ID = NA))
      }
      
      # check for completeness
      if (nrow(leafTable) < nrow(product)) {
        missingProducts <- paste(product[!leafTable][, ProductID], collapse = "/")  # creates string of missing products
        errors <- rbind(errors, data.table(table = "customHierarchy",
                                           desc = paste("Master hierachy is missing these ProductIDs: ", missingProducts),
                                           ID = masterHierarchies[i, NodeID]))
      }
      
    }
    return(errors)    
  }
  
  #' @name DisplayErrors
  #' 
  #' @description prints contents of error table
  DisplayErrors <- function(errortable) {
    print(errortable)
  }
  
  #' @name AppendToNodeTable
  #' 
  #' @description appends the custom hierarchy to the node table
  #' 
  #' @param 
  #'  node - the orginal node table
  #'  customHierarchy - the table to be appended
  #'  
  #' @return the new node table
  #' 
  #' @details 
  #'  the tricky part here is to avoid collisions between NodeIDs in the
  #'  existing table and the customHierarchy
  #'  this is done by finding the maximum ID value in the exixting table and incrmenting all
  #'  custom hierarchy IDs by some value greater than that.
  #'  
  #'  another twist is to adjust the sort order of the root so that 
  #'  it comes after all other hierarchies
  #'  
  #'  finally, we must set the ParentnodeID(s) in the customhierarchy
  #'  to the superroot nodeID in the original
  #' 
  AppendToNodeTable <- function(node, customHierarchy) {
    
    # get the max sort order for Master hierarchies in the node table
    superRootID <- first(node[SuperRootFlag == 1, NodeID])
    nextSortOrder = max(node[ParentnodeID == superRootID, SortOrder]) + 1
    customHierarchy[ParentnodeID == 0, SortOrder := SortOrder + nextSortOrder]
    
    # get max NodeID from node table and round it up to next decimal
    # e.g 1, 8, 9 round up to 10 - 12, 34, 98 round up to 100  - 123, 799, 999 round up to 1000 etc.
    maxNodeID <- max(node[,NodeID])
    maxNodeID <- 10^(round(log10(maxNodeID))+1)
    
    # increment custom hierarchy NodeID and ParentnodeID
    customHierarchy[ , ":=" (NodeID = NodeID + maxNodeID, 
                             ParentnodeID = ParentnodeID + maxNodeID)]
    
    # point the ParentnodeID of customHierarchy masters to the superroot
    customHierarchy[ParentnodeID == maxNodeID, ParentnodeID := superRootID]
    
    # add missing columns to customHierarchy
    customHierarchy[ , ":=" (SuperRootFlag = 0, 
                             OrphanProductFlag = 0)]
    
    # combine the tables
    newNode <- rbind(node, customHierarchy)
    
    return(newNode)
  }
  
  
  #' main entry point ---------------------------------------------------
  customHierarchyData <- LoadValidFile(filename)
  if (nrow(customHierarchyData$errors)) {
    DisplayErrors(customHierarchyData$errors)
    return(list(node = node, errors = customHierarchyData$errors))
  }
  
  completenessErrors <- ValidateHierarchyCompleteness(customHierarchyData$f, product)
  if (nrow(completenessErrors) > 0) {
    DisplayErrors(completenessErrors)
    return(list(node = node, errors = completenessErrors))
  }
  
  newNode <- AppendToNodeTable(node, customHierarchyData$f)
  
  return(list(node = newNode, errors = GetValidateErrorTable()))
}