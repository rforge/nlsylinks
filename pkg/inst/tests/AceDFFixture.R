

###########
context("DF Method 1")
###########
test_that("DFMethod1 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.9779665
  expectedCSquared <- -0.02715555
  expectedESquared <- 0.04918908
  expectedRowCount <- 16588
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.8383875
  expectedCSquared <- -0.112406
  expectedESquared <- 0.2740185
  expectedRowCount <- 13942
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.7256313
  expectedCSquared <- -0.02654524
  expectedESquared <- 0.300914
  expectedRowCount <- 13942
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.6677171
  expectedCSquared <-  -0.01072933
  expectedESquared <- 0.3430122
  expectedRowCount <- 6956
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.8595078
  expectedCSquared <- 0.03879863
  expectedESquared <- 0.1016935
  expectedRowCount <- 16588
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.7780892
  expectedCSquared <- -0.08201297
  expectedESquared <- 0.3039238
  expectedRowCount <- 13942
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.6936874
  expectedCSquared <- -0.00987871
  expectedESquared <- 0.3161913
  expectedRowCount <- 13942
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.633936
  expectedCSquared <-  0.006209162
  expectedESquared <- 0.3598549
  expectedRowCount <- 6956
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
