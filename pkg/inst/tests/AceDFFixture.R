

###########
context("DF Method 1")
###########
test_that("DFMethod1 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.97878157157751255468
  expectedCSquared <- -0.026817134106613928907
  expectedESquared <- 0.048035562529101349938
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.82565313328028289153
  expectedCSquared <- -0.10290415590385164346
  expectedESquared <- 0.2772510226235687103
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.72423466314756224094
  expectedCSquared <- -0.025559809577272250142
  expectedESquared <- 0.30132514642970997798
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.66785026165254879338
  expectedCSquared <-  -0.010839574638099961149
  expectedESquared <- 0.34298931298555113134
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.85643507600784796185
  expectedCSquared <- 0.040925736575665019878
  expectedESquared <- 0.10263918741648703215
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.76895962684027663325
  expectedCSquared <- -0.074614810657907204572
  expectedESquared <- 0.30565518381763057132
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.69300023325910620198
  expectedCSquared <- -0.0092843440352404042748
  expectedESquared <- 0.31628411077613416413
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
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedASquared <- 0.63408057021044650359
  expectedCSquared <-  0.0060927344451816986201
  expectedESquared <- 0.35982669534437183856
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
