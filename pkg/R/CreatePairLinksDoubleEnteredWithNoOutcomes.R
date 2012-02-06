CreatePairLinksDoubleEnteredWithNoOutcomes <-
function( linksPairDataset, linksNames=c("ExtendedID", "R", "RelationshipPath") ) {
  ValidatePairLinks(linksPairDataset)
  
  dsLinksLeftHand <- subset(linksPairDataset, select=c("Subject1Tag","Subject2Tag", linksNames)) #'Lefthand' is my slang for Subjec1Tag is less than the Subject2Tag
  dsLinksRightHand <- subset(linksPairDataset, select=c("Subject1Tag", "Subject2Tag", linksNames))
  
  ds <- rbind(dsLinksLeftHand, dsLinksRightHand) #'RowBind' the two datasets
  ds <- ds[, c("Subject1Tag", "Subject2Tag")]
  rm(dsLinksLeftHand, dsLinksRightHand)
  return( ds )
}
