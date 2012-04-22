#require(stringr)

###########
context("Lavaan")
###########
test_that("AceLavaanGroup -MathStandardized", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  oName_1 <- "MathStandardized_1" #Stands for Manifest1
  oName_2 <- "MathStandardized_2" #Stands for Manifest2
  
  dsGroupSummary <- RGroupSummary(dsFull, oName_1, oName_2)
#   rLevels <- dsGroupSummary[dsGroupSummary$Included, "R"]
  dsClean <- CleanSemAceDataset(dsDirty=dsFull, dsGroupSummary=dsGroupSummary, oName_1=oName_1, oName_2=oName_2)
  
  ace <- AceLavaanGroup(dsClean)
  
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
  oName_1 <- "WeightStandardizedForAge19To25_1"
  oName_2 <- "WeightStandardizedForAge19To25_2"
  
  dsGroupSummary <- RGroupSummary(dsFull, oName_1, oName_2)
#   rLevels <- dsGroupSummary[dsGroupSummary$Included, "R"]
  dsClean <- CleanSemAceDataset(dsDirty=dsFull, dsGroupSummary=dsGroupSummary, oName_1=oName_1, oName_2=oName_2)
  
  ace <- AceLavaanGroup(dsClean)
  
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
