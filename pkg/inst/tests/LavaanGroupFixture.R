require(stringr)

###########
context("Lavaan")
###########
test_that("AceLavaanGroup -MathStandardized", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "MathStandardized_1" #Stands for Manifest1
  m2Name <- "MathStandardized_2" #Stands for Manifest2
  
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  rLevels <- dsGroupSummary[dsGroupSummary$Included, "R"]
  dsClean <- CleanSemAceDataset(dsDirty=dsFull, dsGroupSummary=dsGroupSummary, m1Name=m1Name, m2Name=m2Name)
  
  ace <- AceLavaanGroup(dsClean, rLevels, m1Name, m2Name)
  
  expectedASquared <- 0.670103215171409
  expectedCSquared  <- 0.11670604326754
  expectedESquared <- 0.213190741561051
  expectedCaseCount <- 8292
    
  expect_equal(object=ace@ASquared, expected=expectedASquared, scale=1)
  expect_equal(object=ace@CSquared, expected=expectedCSquared, scale=1)
  expect_equal(object=ace@ESquared, expected=expectedESquared, scale=1)
  expect_equal(object=ace@CaseCount, expected=expectedCaseCount, scale=1)
  expect_equal(object=slot(ace, "CaseCount"), expected=expectedCaseCount, scale=1)
  expect_true(object=slot(ace, "Unity"))
  expect_true(object=slot(ace, "WithinBounds"))
})

test_that("AceLavaanGroup -WeightStandardizedForAge19To25", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "WeightStandardizedForAge19To25_1"
  m2Name <- "WeightStandardizedForAge19To25_2"
  
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  rLevels <- dsGroupSummary[dsGroupSummary$Included, "R"]
  dsClean <- CleanSemAceDataset(dsDirty=dsFull, dsGroupSummary=dsGroupSummary, m1Name=m1Name, m2Name=m2Name)
  
  ace <- AceLavaanGroup(dsClean, rLevels, m1Name, m2Name)
  
  expectedASquared <- 0.687801119966999
  expectedCSquared  <- 7.10884537223939e-15
  expectedESquared <- 0.312198880032994
  expectedCaseCount <- 3478
  
  expect_equal(object=ace@ASquared, expected=expectedASquared, scale=1)
  expect_equal(object=ace@CSquared, expected=expectedCSquared, scale=1)
  expect_equal(object=ace@ESquared, expected=expectedESquared, scale=1)
  expect_equal(object=ace@CaseCount, expected=expectedCaseCount, scale=1)
  expect_true(object=slot(ace, "Unity"))
  expect_true(object=slot(ace, "WithinBounds"))
})
# str_c(ace@ASquared)
# str_c(ace@CSquared)
# str_c(ace@ESquared)
# str_c(ace@CaseCount)
