RenameNlsyColumn <-
function( dataFrame, nlsyRNumber, newColumnName ) {
  index <- VerifyColumnExists(dataFrame=dataFrame, columnName=nlsyRNumber)
  colnames(dataFrame)[index] <- newColumnName
  return( dataFrame )
  #colnames(ds)[colnames(ds)=='C0000100'] <- "SubjectID"
}
