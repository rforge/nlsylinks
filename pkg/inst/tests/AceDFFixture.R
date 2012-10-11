

###########
context("DF Method 1")
###########
test_that("DFMethod1 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.97281725241557127#0.97878157157751255468
  expectedCSquared <- -0.02425670538213379 #-0.026817134106613928907
  expectedESquared <- 0.051439452966562493 #0.048035562529101349938
  expectedRowCount <- 16784
  unique(dsDF$R)
  #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]  
  oName_1 <- "MathStandardized_1"
  oName_2 <- "MathStandardized_2"  
  
  actual <- DeFriesFulkerMethod1(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)  
  #actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R)
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod1", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  #actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method="DeFriesFulkerMethod1")
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod1 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.83663437391582696
  expectedCSquared <- -0.10750177051367063 #-0.10290415590385164346
  expectedESquared <- 0.27086739659784365 #0.2772510226235687103
  expectedRowCount <- 14008
  oName_1 <- "Weight_1"
  oName_2 <- "Weight_2"
  
  actual <- DeFriesFulkerMethod1(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)  
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod1", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod1 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.71849562607810991 #0.72423466314756224094
  expectedCSquared <-  -0.023244051592718619 #-0.025559809577272250142
  expectedESquared <- 0.30474842551460868 #0.30132514642970997798
  expectedRowCount <- 14008
  oName_1 <- "WeightStandardized_1"
  oName_2 <- "WeightStandardized_2"
  
  actual <- DeFriesFulkerMethod1(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)  
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
    
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod1", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod1 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.67402146265628393 
  expectedCSquared <- -0.013283359091640816 
  expectedESquared <- 0.33926189643535687 
  expectedRowCount <- 6958
  oName_1 <- "WeightStandardizedForAge19To25_1"
  oName_2 <- "WeightStandardizedForAge19To25_2"
  
  actual <- DeFriesFulkerMethod1(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)  
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod1", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})



###########
context("DF Method 3")
###########
test_that("DFMethod3 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.84796879015144833 
  expectedCSquared <- 0.044772351172503257 
  expectedESquared <- 0.10725885867604845 
  expectedRowCount <- 16784
  #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]
  oName_1 <- "MathStandardized_1"
  oName_2 <- "MathStandardized_2"  
  
  actual <- DeFriesFulkerMethod3(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_true(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod3", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_true(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod3 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.77850187817631544 
  expectedCSquared <- -0.078527914387033285 
  expectedESquared <- 0.30002603621071788 
  expectedRowCount <- 14008
  #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]
  oName_1 <- "Weight_1"
  oName_2 <- "Weight_2"  
  
  actual <- DeFriesFulkerMethod3(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod3", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod3 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.68701113301218675 
  expectedCSquared <- -0.0068612691168715764 
  expectedESquared <- 0.31985013610468482 
  expectedRowCount <- 14008
  #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]
  oName_1 <- "WeightStandardized_1"
  oName_2 <- "WeightStandardized_2"  
  
  actual <- DeFriesFulkerMethod3(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_false(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod3", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_false(object=slot(actualFromWrapper, "WithinBounds"))
})

test_that("DFMethod3 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsFull <- Links79Pair[Links79Pair$RelationshipPath=='Gen2Siblings', ]
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=dsFull, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.64047731125483631 
  expectedCSquared <- 0.0035255270911337348 
  expectedESquared <- 0.35599716165402995 
  expectedRowCount <- 6958
  #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]
  oName_1 <- "WeightStandardizedForAge19To25_1"
  oName_2 <- "WeightStandardizedForAge19To25_2"  
  
  actual <- DeFriesFulkerMethod3(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actual, "Unity"))
  expect_true(object=slot(actual, "WithinBounds"))
  #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod3", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
  expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
  expect_true(object=slot(actualFromWrapper, "Unity"))
  expect_true(object=slot(actualFromWrapper, "WithinBounds"))
})

# cat("expectedASquared <-", actual@ASquared, 
#     "\nexpectedCSquared <-", actual@CSquared,
#     "\nexpectedESquared <-", actual@ESquared,
#     "\nexpectedRowCount <-", actual@CaseCount)


# slot(actual, "ASquared")
# slot(actual, "CSquared")
# slot(actual, "ESquared")
# slot(actual, "CaseCount")



# ###########
# context("DF Method 3 w/ dataset")
# ###########
# test_that("DFMethod3 -MathStandardized", {
#   dsOutcomes <- ExtraOutcomes79
#   dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
#   dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
#   expectedASquared <- 0.8595078
#   expectedCSquared <- 0.03879863
#   expectedESquared <- 0.1016935
#   expectedRowCount <- 16588
#   #dsDF <- dsDF[dsDF$R %in% c(0, .25, .375, .5, 1), ]
#   
#   oName_1 <- "MathStandardized_1"
#   oName_2 <- "MathStandardized_2"  
#   actual <- DF3Ds(dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
#   expect_equal(object=slot(actual, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
#   expect_equal(object=slot(actual, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
#   expect_equal(object=slot(actual, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)  
#   expect_equal(object=slot(actual, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
#   expect_true(object=slot(actual, "Unity"))
#   expect_true(object=slot(actual, "WithinBounds"))
#   #expect_equal(object=actual$ASquared, expected=expectedASquared, tolerance=1e-6, scale=1)
#   
#   actualFromWrapper <- AceUnivariate(method="DeFriesFulkerMethod3", dataSet=dsDF, oName_1=oName_1, oName_2=oName_2)
#   expect_equal(object=slot(actualFromWrapper, "ASquared"), expected=expectedASquared, tolerance=1e-6, scale=1)
#   expect_equal(object=slot(actualFromWrapper, "CSquared"), expected=expectedCSquared, tolerance=1e-6, scale=1)
#   expect_equal(object=slot(actualFromWrapper, "ESquared"), expected=expectedESquared, tolerance=1e-6, scale=1)
#   expect_equal(object=slot(actualFromWrapper, "CaseCount"), expected=expectedRowCount, tolerance=1e-6, scale=1)
#   expect_true(object=slot(actualFromWrapper, "Unity"))
#   expect_true(object=slot(actualFromWrapper, "WithinBounds"))
# })
