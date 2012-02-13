

###########
context("DF Method 1")
###########
test_that("DFMethod1 -MathStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.9779665
  expectedCSquared <- -0.02715555
  expectedRowCount <- 16588
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.8383875
  expectedCSquared <- -0.112406
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.7256313
  expectedCSquared <- -0.02654524
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.6677171
  expectedCSquared <-  -0.01072933
  expectedRowCount <- 6956
  
  actual <- DeFriesFulkerMethod1(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
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
  expectedRowCount <- 16588
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -Weight", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.7780892
  expectedCSquared <- -0.08201297
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$Weight_1, outcomeForSubject2=dsDF$Weight_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardized", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.6936874
  expectedCSquared <- -0.00987871
  expectedRowCount <- 13942
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$WeightStandardized_1, outcomeForSubject2=dsDF$WeightStandardized_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})

test_that("DFMethod1 -WeightStandardizedAdult", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  expectedHSquared <- 0.633936
  expectedCSquared <-  0.006209162
  expectedRowCount <- 6956
  
  actual <- DeFriesFulkerMethod3(outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, relatedness=dsDF$R)
  expect_equal(object=actual$HSquared, expected=expectedHSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$CSquared, expected=expectedCSquared, tolerance=1e-6, scale=1)
  expect_equal(object=actual$RowCount, expected=expectedRowCount, tolerance=1e-6, scale=1)
})




