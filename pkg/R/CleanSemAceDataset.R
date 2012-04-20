CleanSemAceDataset <-
function( dsDirty, dsGroupSummary, m1Name, m2Name, rName="R" ) {
  rLevelsToInclude <- dsGroupSummary[dsGroupSummary$Included, rName]
  
  #It's necessary to drop the missing Groups & unnecessary columns.  Missing M1s & M2s are dropped for the sake of memory space.
  oldColumnNames <- c(rName, m1Name, m2Name)
  newColumnNames <- c("R", "M1", "M2")
  selectedRows <- (!is.na(dsDirty[, rName])) & (dsDirty[, rName] %in%  rLevelsToInclude) & (!is.na(dsDirty[, m1Name])) & (!is.na(dsDirty[, m2Name]))
  dsClean <- dsDirty[selectedRows, oldColumnNames] 
  
  colnames(dsClean) <- newColumnNames
  
  dsClean <- dsClean[order(dsClean$R), ] #TODO: Rewrite overall code so this statement is not longer necessary anyomre.
  
  #This helper function allows for slight imprecision from floating-point arithmetic.
  EqualApprox <- function( target, current, toleranceAbsolute=1e-8) {  
    return( abs(target-current) < toleranceAbsolute ) 
  }
  
  #rLevelsToExclude <- dsGroupSummary[!dsGroupSummary$Included, 'R']
  
  #This loop assigns a GroupID, depending on their R value. TODO: possibly rewrite and vectorize with plyr.
  dsClean$GroupID <- NA
  for( groupIndex in seq_along(rLevelsToInclude) ) {
    r <- rLevelsToInclude[groupIndex]
    memberIndices <- sapply(dsClean$R, EqualApprox, r)
    dsClean$GroupID[memberIndices] <- groupIndex
  }
  
  return( dsClean )
}
