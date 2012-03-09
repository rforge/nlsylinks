

###########
context("DF Method 1")
###########
test_that("DFMethod1 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.9779665
  expectedCSquared <- -0.02715555
  expectedESquared <- 0.04918908
  expectedRowCount <- 16588
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method="DeFriesFulkerMethod1")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)  
})

test_that("DFMethod1 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.8383875
  expectedCSquared <- -0.112406
  expectedESquared <- 0.2740185
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R, method="DeFriesFulkerMethod1")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.7256313
  expectedCSquared <- -0.02654524
  expectedESquared <- 0.300914
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)  
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R, method="DeFriesFulkerMethod1")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.6677171
  expectedCSquared <-  -0.01072933
  expectedESquared <- 0.3430122
  expectedRowCount <- 6956
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R, method="DeFriesFulkerMethod1")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})


###########
context("DF Method 3")
###########
test_that("DFMethod3 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.8595078
  expectedCSquared <- 0.03879863
  expectedESquared <- 0.1016935
  expectedRowCount <- 16588
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method="DeFriesFulkerMethod3")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)  
})

test_that("DFMethod3 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.7780892
  expectedCSquared <- -0.08201297
  expectedESquared <- 0.3039238
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)  
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R, method="DeFriesFulkerMethod3")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)    
})

test_that("DFMethod3 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.6936874
  expectedCSquared <- -0.00987871
  expectedESquared <- 0.3161913
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R, method="DeFriesFulkerMethod3")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod3 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.633936
  expectedCSquared <-  0.006209162
  expectedESquared <- 0.3598549
  expectedRowCount <- 6956
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)  
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
  
  actualFromWrapper <- AceUnivariate(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R, method="DeFriesFulkerMethod3")
  expect_equal(object=actualFromWrapper$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$ESquared, expected=expectedESquared, tolerance=1e-6, scale=1)
  expect_equal(object=actualFromWrapper$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})




