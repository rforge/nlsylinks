
###########
context("Clean Ace Sem Dataset")
###########
test_that("CleanSemAceDataset MathStandardized", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "MathStandardized_1" #Stands for Manifest1
  m2Name <- "MathStandardized_2" #Stands for Manifest2
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  
  dsClean <- CleanSemAceDataset( dsDirty=dsFull, dsGroupSummary, m1Name, m2Name, rName="R" )
  
  expectedRowCount <- 8292
  expectedColumnNames <- c('R', 'M1', 'M2', 'GroupID')
  expectedCompleteRows <- expectedRowCount
  expectedMeanR <- 0.418701760733237
  expectedMeanM1 <- 98.1445972021225
  expectedMeanM2 <- 98.6287988422576
  expectedMeanGroupID <- 2.34165460684998
  
  expect_equal(object=nrow(dsClean), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsClean), expected=expectedColumnNames, scale=1)
  expect_equal(object=mean(dsClean$R), expected=expectedMeanR, scale=1)
  expect_equal(object=mean(dsClean$M1), expected=expectedMeanM1, scale=1)
  expect_equal(object=mean(dsClean$M2), expected=expectedMeanM2, scale=1)
  expect_equal(object=mean(dsClean$GroupID), expected=expectedMeanGroupID, scale=1)  
  expect_equal(object=nrow(subset(dsClean, !is.na(R) & !is.na(M1) & !is.na(M2) & !is.na(GroupID))), expected=expectedCompleteRows, scale=1)
})
test_that("CleanSemAceDataset WeightStandardizedForAge19To25", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "WeightStandardizedForAge19To25_1" #Stands for Manifest1
  m2Name <- "WeightStandardizedForAge19To25_2" #Stands for Manifest2
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  
  dsClean <- CleanSemAceDataset( dsDirty=dsFull, dsGroupSummary, m1Name, m2Name, rName="R" )
  
  expectedRowCount <- 3478
  expectedColumnNames <- c('R', 'M1', 'M2', 'GroupID')
  expectedCompleteRows <- expectedRowCount
  expectedMeanR <- 0.4252443933295
  expectedMeanM1 <- 0.0761793972446809
  expectedMeanM2 <- -0.0275629058510638
  expectedMeanGroupID <- 2.39074180563542
  
  expect_equal(object=nrow(dsClean), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsClean), expected=expectedColumnNames, scale=1)
  expect_equal(object=mean(dsClean$R), expected=expectedMeanR, scale=1)
  expect_equal(object=mean(dsClean$M1), expected=expectedMeanM1, scale=1)
  expect_equal(object=mean(dsClean$M2), expected=expectedMeanM2, scale=1)
  expect_equal(object=mean(dsClean$GroupID), expected=expectedMeanGroupID, scale=1)
  expect_equal(object=nrow(subset(dsClean, !is.na(R) & !is.na(M1) & !is.na(M2) & !is.na(GroupID))), expected=expectedCompleteRows, scale=1)
})
# require(stringr)
# nrow(dsClean)
# str_c(colnames(dsClean), collapse="', '")
# str_c(mean(dsClean$R))
# str_c(mean(dsClean$M1))
# str_c(mean(dsClean$M2))
# str_c(mean(dsClean$GroupID))

###########
context("R Group Summary")
###########
test_that("Group Summary MathStandardized", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "MathStandardized_1" #Stands for Manifest1
  m2Name <- "MathStandardized_2" #Stands for Manifest2
   
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  
  expectedRowCount <- 5
  expectedColumnNames <- c('R', 'Included', 'PairCount', 'M1Variance', 'M2Variance', 'M1M2Covariance', 'Correlation', 'Determinant', 'PosDefinite')
  expectedR <- c(.25, .375, .5, .75, 1)
  expectedIncluded <- c(T, T, T, F, T)
  expectedPairCount <- c(2719, 43, 5508, 2, 22)
  expectedM1Variance <- c(169.129066138835, 187.720930232558, 230.966317246723, 220.5, 319.194805194805)
  expectedM2Variance <- c(207.02327149774, 220.93023255814, 233.349179640516, 18, 343.116883116883)
  expectedM1M2Covariance <- c(40.6604752591322, 28.6633444075305, 107.598223901738, 63, 277.588744588745)
  expectedCorrelation <- c(0.217297005505206, 0.140748228806615, 0.463476414530474, 1, 0.838789337853393)
  expectedDeterminant <- c(33360.3783291208, 40651.6414596847, 42318.4228673054, 0, 32465.6155431869)
  expectedPosDefinite <- expectedIncluded
    
  expect_equal(object=nrow(dsGroupSummary), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsGroupSummary), expected=expectedColumnNames, scale=1)
  expect_equal(object=dsGroupSummary$R, expected=expectedR, scale=1)
  expect_equal(object=dsGroupSummary$Included, expected=expectedIncluded, scale=1)
  expect_equal(object=dsGroupSummary$PairCount, expected=expectedPairCount, scale=1)
  expect_equal(object=dsGroupSummary$M1Variance, expected=expectedM1Variance, scale=1)
  expect_equal(object=dsGroupSummary$M2Variance, expected=expectedM2Variance, scale=1)
  expect_equal(object=dsGroupSummary$M1M2Covariance, expected=expectedM1M2Covariance, scale=1)
  expect_equal(object=dsGroupSummary$Correlation, expected=expectedCorrelation, scale=1)
  expect_equal(object=dsGroupSummary$Determinant, expected=expectedDeterminant, scale=1)
  expect_equal(object=dsGroupSummary$PosDefinite, expected=expectedPosDefinite, scale=1)
})

test_that("Group Summary WeightStandardizedForAge19To25", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "WeightStandardizedForAge19To25_1" #Stands for Manifest1
  m2Name <- "WeightStandardizedForAge19To25_2" #Stands for Manifest2
  
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
  
  expectedRowCount <- 5
  expectedColumnNames <- c('R', 'Included', 'PairCount', 'M1Variance', 'M2Variance', 'M1M2Covariance', 'Correlation', 'Determinant', 'PosDefinite')
  expectedR <- c(.25, .375, .5, .75, 1)
  expectedIncluded <- c(T, T, T, F, T)
  expectedPairCount <- c(1052, 28, 2385, 0, 13)
  expectedM1Variance <- c(1.22296030966474, 1.82981402660781, 0.963610386613974, NA, 1.16811811494777)
  expectedM2Variance <- c(1.02980729264135, 1.07270462669624, 0.906660963145878, NA, 1.51474929993746)
  expectedM1M2Covariance <- c(0.192417483156412, 0.054415964689028, 0.300094417580917, NA, 1.27081120357811)
  expectedCorrelation <- c(0.171459031782169, 0.0388403182414852, 0.32105890616282, NA, 0.955360487068942)
  expectedDeterminant <- c(1.22238895767942, 1.95988887512283, 0.783611261761568, NA, 0.154444981721754)
  expectedPosDefinite <- expectedIncluded
  
  expect_equal(object=nrow(dsGroupSummary), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsGroupSummary), expected=expectedColumnNames, scale=1)
  expect_equal(object=dsGroupSummary$R, expected=expectedR, scale=1)
  expect_equal(object=dsGroupSummary$Included, expected=expectedIncluded, scale=1)
  expect_equal(object=dsGroupSummary$PairCount, expected=expectedPairCount, scale=1)
  expect_equal(object=dsGroupSummary$M1Variance, expected=expectedM1Variance, scale=1)
  expect_equal(object=dsGroupSummary$M2Variance, expected=expectedM2Variance, scale=1)
  expect_equal(object=dsGroupSummary$M1M2Covariance, expected=expectedM1M2Covariance, scale=1)
  expect_equal(object=dsGroupSummary$Correlation, expected=expectedCorrelation, scale=1)
  expect_equal(object=dsGroupSummary$Determinant, expected=expectedDeterminant, scale=1)
  expect_equal(object=dsGroupSummary$PosDefinite, expected=expectedPosDefinite, scale=1)
})

test_that("Group Summary Changed Variable Name for 'R'", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "WeightStandardizedForAge19To25_1" #Stands for Manifest1
  m2Name <- "WeightStandardizedForAge19To25_2" #Stands for Manifest2
  rName <- "RRR"
  dsFull <- RenameNlsyColumn(dsFull, "R", rName)
                         
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name, rName)
  
  expectedRowCount <- 5
  expectedColumnNames <- c('RRR', 'Included', 'PairCount', 'M1Variance', 'M2Variance', 'M1M2Covariance', 'Correlation', 'Determinant', 'PosDefinite')
  expectedR <- c(.25, .375, .5, .75, 1)
  expectedIncluded <- c(T, T, T, F, T)
  expectedPairCount <- c(1052, 28, 2385, 0, 13)
  expectedM1Variance <- c(1.22296030966474, 1.82981402660781, 0.963610386613974, NA, 1.16811811494777)
  expectedM2Variance <- c(1.02980729264135, 1.07270462669624, 0.906660963145878, NA, 1.51474929993746)
  expectedM1M2Covariance <- c(0.192417483156412, 0.054415964689028, 0.300094417580917, NA, 1.27081120357811)
  expectedCorrelation <- c(0.171459031782169, 0.0388403182414852, 0.32105890616282, NA, 0.955360487068942)
  expectedDeterminant <- c(1.22238895767942, 1.95988887512283, 0.783611261761568, NA, 0.154444981721754)
  expectedPosDefinite <- expectedIncluded
  
  expect_equal(object=nrow(dsGroupSummary), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsGroupSummary), expected=expectedColumnNames, scale=1)
  expect_equal(object=dsGroupSummary[, rName], expected=expectedR, scale=1)
  expect_equal(object=dsGroupSummary$Included, expected=expectedIncluded, scale=1)
  expect_equal(object=dsGroupSummary$PairCount, expected=expectedPairCount, scale=1)
  expect_equal(object=dsGroupSummary$M1Variance, expected=expectedM1Variance, scale=1)
  expect_equal(object=dsGroupSummary$M2Variance, expected=expectedM2Variance, scale=1)
  expect_equal(object=dsGroupSummary$M1M2Covariance, expected=expectedM1M2Covariance, scale=1)
  expect_equal(object=dsGroupSummary$Correlation, expected=expectedCorrelation, scale=1)
  expect_equal(object=dsGroupSummary$Determinant, expected=expectedDeterminant, scale=1)
  expect_equal(object=dsGroupSummary$PosDefinite, expected=expectedPosDefinite, scale=1)
})

test_that("Single Group Summary MathStandardized", {
  dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
  m1Name <- "MathStandardized_1" #Stands for Manifest1
  m2Name <- "MathStandardized_2" #Stands for Manifest2
  dsFull$DummyGroup <- 1
  rName <- "DummyGroup"
  
  dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name, rName)
  
  expectedRowCount <- 1
  expectedColumnNames <- c(rName, 'Included', 'PairCount', 'M1Variance', 'M2Variance', 'M1M2Covariance', 'Correlation', 'Determinant', 'PosDefinite')
  expectedR <- 1
  expectedIncluded <- T
  expectedPairCount <- 8392
  expectedM1Variance <- 216.465970550521
  expectedM2Variance <- 229.298827935283
  expectedM1M2Covariance <- 90.9026634829023
  expectedCorrelation <- 0.4080194579033
  expectedDeterminant <- 41372.099106822
  expectedPosDefinite <- expectedIncluded
  
  expect_equal(object=nrow(dsGroupSummary), expected=expectedRowCount, scale=1)
  expect_equal(object=colnames(dsGroupSummary), expected=expectedColumnNames, scale=1)
  expect_equal(object=dsGroupSummary[, rName], expected=expectedR, scale=1)
  
  expect_equal(object=dsGroupSummary$Included, expected=expectedIncluded, scale=1)
  expect_equal(object=dsGroupSummary$PairCount, expected=expectedPairCount, scale=1)
  expect_equal(object=dsGroupSummary$M1Variance, expected=expectedM1Variance, scale=1)
  expect_equal(object=dsGroupSummary$M2Variance, expected=expectedM2Variance, scale=1)
  expect_equal(object=dsGroupSummary$M1M2Covariance, expected=expectedM1M2Covariance, scale=1)
  expect_equal(object=dsGroupSummary$Correlation, expected=expectedCorrelation, scale=1)
  expect_equal(object=dsGroupSummary$Determinant, expected=expectedDeterminant, scale=1)
  expect_equal(object=dsGroupSummary$PosDefinite, expected=expectedPosDefinite, scale=1)
})

# require(stringr)
# dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
# str_c(colnames(dsGroupSummary), collapse="', '")
# str_c(dsGroupSummary[, rName], collapse=", ")
# str_c(dsGroupSummary$Included, collapse=", ")
# str_c(dsGroupSummary$PairCount, collapse=", ")
# str_c(dsGroupSummary$M1Variance, collapse=", ")
# str_c(dsGroupSummary$M2Variance, collapse=", ")
# str_c(dsGroupSummary$M1M2Covariance, collapse=", ")
# str_c(dsGroupSummary$Correlation, collapse=", ")
# str_c(dsGroupSummary$Determinant, collapse=", ")
# str_c(dsGroupSummary$PosDefinite, collapse=", ")
