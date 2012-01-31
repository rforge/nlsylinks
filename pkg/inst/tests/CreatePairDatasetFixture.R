#options(digits=17)
#expect_equal(object=10.01, expected=10, tolerance=.002, scale=1) #Absolute Difference
#expect_equal(object=10.01, expected=10, tolerance=.002, scale=NULL) #Relative Difference

LoadPairFile <- function( ) {
  #directory <- "F:/Projects/Nls/Links2011/Analysis/Df/2012-01-13/"
  #pathLinks <- paste(directory, "Links2011V28.csv", sep="")
  #dsLinks <- read.csv(pathLinks)
  data(Links79Pair)
  return( Links79Pair )
}
LoadDVFile <- function( ) {
#   directory <- "F:/Projects/Nls/Links2011/Analysis/Df/2012-01-13/"
#   pathDv <-  paste(directory, "BMI_Sex_Intell.csv", sep="")
#   dsDv <- read.csv(pathDv)
  data(ExtraDVs79)
  return( ExtraDVs79 )
}
LoadDefaultDVNames <- function( ) {
  return( c("Weight", "WeightStandardized") )
}

###########
context("CreatePairLinksDoubleEntered")
###########
test_that("CreatePairLinks -Normal Scenario", {
  dsLinks <- LoadPairFile()
  dsDVs <- LoadDVFile()
  dsDVs$SubjectTag <- CreateSubjectTag(subjectID=dsDVs$SubjectID, generation=dsDVs$Generation)
  dsLinksWithExtraDV <- CreatePairLinksDoubledEntered(dvNames=LoadDefaultDVNames(), dvDataset=dsDVs, linksPairDataset=dsLinks)
  expect_equal(nrow(dsLinksWithExtraDV), 22150, info="The number of rows in the pairs links should be correct.")
  expect_equal(ncol(dsLinksWithExtraDV), 9, info="The number of columns in the pairs links should be correct.")  
  
  expectedColumnNames <- c("Subject1Tag", "Subject2Tag", "ExtendedID", "R", "RelationshipPath", "Weight_1", "WeightStandardized_1", "Weight_2", "WeightStandardized_2")
  actualColumnNames <- colnames(dsLinksWithExtraDV)
  expect_equal(actualColumnNames, expectedColumnNames, info="The column names, and their order, should be correct.")
  
  
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_1, na.rm=T), -0.009590724, tolerance=1e-7, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_1, na.rm=T), 0, tolerance=.01, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_1, na.rm=T), mean(dsLinksWithExtraDV$WeightStandardized_2, na.rm=T))#, tolerance=.01, scale=1)
  
  expect_equal(mean(dsLinksWithExtraDV$Weight_1, na.rm=T), 161.9422, tolerance=1e-5, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$Weight_1, na.rm=T), mean(dsLinksWithExtraDV$Weight_2, na.rm=T))#, tolerance=.01, scale=1)  

  #The following aren't tested against meaningful values, but they do provide some regression testing.
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$Subject1Tag), na.rm=T), 13156877637)
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$Subject1Tag), na.rm=T), sum(as.numeric(dsLinksWithExtraDV$Subject2Tag, na.rm=T)))  
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$ExtendedID), na.rm=T), 131494202)
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$R), na.rm=T), 8813.5)  
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$RelationshipPath), na.rm=T), 22150)

})


###########
context("CreatePairLinksSingleEntered")
###########
test_that("CreatePairLinks -Normal Scenario", {
  dsLinks <- LoadPairFile()
  dsDVs <- LoadDVFile()
  dsDVs$SubjectTag <- CreateSubjectTag(subjectID=dsDVs$SubjectID, generation=dsDVs$Generation)
  dsLinksWithExtraDV <- CreatePairLinksSingleEntered(dvNames=LoadDefaultDVNames(), dvDataset=dsDVs, linksPairDataset=dsLinks)
  expect_equal(nrow(dsLinksWithExtraDV), 11075, info="The number of rows in the pairs links should be correct.")
  expect_equal(ncol(dsLinksWithExtraDV), 9, info="The number of columns in the pairs links should be correct.")  
  
  expectedColumnNames <- c("Subject1Tag", "Subject2Tag", "ExtendedID", "R", "RelationshipPath", "Weight_1", "WeightStandardized_1", "Weight_2", "WeightStandardized_2")
  actualColumnNames <- colnames(dsLinksWithExtraDV)
  expect_equal(actualColumnNames, expectedColumnNames, info="The column names, and their order, should be correct.")
  
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_1, na.rm=T), 0.0065548062774566469, tolerance=1e-15, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_1, na.rm=T), 0, tolerance=.01, scale=1)  
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_2, na.rm=T),  -0.028869654720259843, tolerance=1e-15, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$WeightStandardized_2, na.rm=T), 0, tolerance=.03, scale=1)
    
  expect_equal(mean(dsLinksWithExtraDV$Weight_1, na.rm=T),  167.02198798594583, tolerance=1e-15, scale=1)
  expect_equal(mean(dsLinksWithExtraDV$Weight_2, na.rm=T), 155.87657328461225, tolerance=1e-15, scale=1)
  
  #The following aren't tested against meaningful values, but they do provide some regression testing.
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$Subject1Tag), na.rm=T), 6578429612)
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$Subject2Tag), na.rm=T), 6578448025)  
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$ExtendedID), na.rm=T), 131494202/2)
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$R), na.rm=T), 8813.5/2)  
  expect_equal(sum(as.numeric(dsLinksWithExtraDV$RelationshipPath), na.rm=T), 22150/2)
})

###########
context("ValidatePairLinks")
###########
test_that("ValidatePairLinks -Normal Scenario", {
  dsLinks <- LoadPairFile()
  expect_true(ValidatePairLinks(dsLinks))
})

test_that("Zero rows", {
  dsLinks <- LoadPairFile()
  dsLinks <- dsLinks[0,]
  expect_error(ValidatePairLinks(dsLinks), "The linksPair data frame should have at least one row, but does not.")
})

test_that("Bad Subject1Tag", {
  dsLinks <- LoadPairFile()
  expect_true(ValidatePairLinks(dsLinks))
  colnames(dsLinks)[colnames(dsLinks)=="Subject1Tag"] <- "Bad"
  expect_error(ValidatePairLinks(dsLinks), "The column 'Subject1Tag' should exist in the linksPair file, but does not.")
})

test_that("Bad Subject2Tag", {
  dsLinks <- LoadPairFile()
  expect_true(ValidatePairLinks(dsLinks))
  colnames(dsLinks)[colnames(dsLinks)=="Subject2Tag"] <- "Bad"
  expect_error(ValidatePairLinks(dsLinks), "The column 'Subject2Tag' should exist in the linksPair file, but does not.")
})

test_that("Bad R", {
  dsLinks <- LoadPairFile()
  expect_true(ValidatePairLinks(dsLinks))
  colnames(dsLinks)[colnames(dsLinks)=="R"] <- "Bad"
  expect_error(ValidatePairLinks(dsLinks), "The column 'R' should exist in the linksPair file, but does not.")
})

# test_that("Bad MultipleBirth", {
#   dsLinks <- LoadPairFile()
#   expect_true(ValidatePairLinks(dsLinks))
#   colnames(dsLinks)[colnames(dsLinks)=="MultipleBirth"] <- "Bad"
#   expect_error(ValidatePairLinks(dsLinks), "The column 'MultipleBirth' should exist in the linksPair file, but does not.")
# })
