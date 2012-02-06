
# LoadPairFile <- function( ) {
#   #directory <- "F:/Projects/Nls/Links2011/Analysis/Df/2012-01-13/"
#   #pathLinks <- paste(directory, "Links2011V28.csv", sep="")
#   #dsLinks <- read.csv(pathLinks)
#   data(Links79Pair)
#   return( Links79Pair )
# }


###########
context("Utilities")
###########
test_that("CreateSubjectTag -Scenario 1", {
  ids <- c(1:10, 1:10)
  generation <- c(rep(1, 10), rep(2, 10))
  expected <- c(1:10*100, 1:10)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})
test_that("CreateSubjectTag -Scenario 2", {
  ids <- c(71:80, 1:10)
  generation <- c(rep(2, 10), rep(1, 10))
  expected <- c(71:80, 1:10*100)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})
test_that("CreateSubjectTag -Scenario 3", {
  ids <- c(NA, NA, 71:80, NA, 1:10, NA)
  generation <- c(rep(2, 12), rep(1, 12))
  expected <- c(NA, NA,71:80, NA, 1:10*100, NA)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})
test_that("CreateSubjectTag -Scenario 4", {
  ids <- c(71:82, 1:12)
  generation <- c(NA, NA, rep(2, 10), NA, rep(1, 10), NA)
  expected <- c(NA, NA,73:82, NA, 2:11*100, NA)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})
test_that("CreateSubjectTag -Scenario 5", {
  ids <- c(71:82, 10001:10012)
  generation <- c(NA, NA, rep(1, 10), NA, rep(2, 10), NA)
  expected <- c(NA, NA,73:82*100, NA, 10002:10011, NA)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})

test_that("CreateSubjectTag -Scenario 6", {
  ids <- c(71:82, 10001:10012)
  generation <- c( rep(1, 12),rep(2, 12))
  expected <- c(71:82*100, 10001:10012)
  expect_equal(expected, CreateSubjectTag(ids, generation))
  expect_equivalent(expected, CreateSubjectTag(ids, generation))
})

test_that("CreateSubjectTag -With ExtraOutcomes79", {
  data(ExtraOutcomes79)
  
  actual <- CreateSubjectTag(subjectID=ExtraOutcomes79$SubjectID, generation=ExtraOutcomes79$Generation)
  expected <- ExpectedSubjectTags
  
  expect_equal(expected, actual)
  #cat(PrintVector(ExtraOutcomes79$SubjectTag))  
})


# test_that("IncludeSubjectTag -Scenario 1", {
#   ids <- c(1:10, 1:10)
#   generation <- c(rep(1, 10), rep(2, 10))
#   expected <- c(1:10*100, 1:10)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, IncludeSubjectTag(ExtraOutcomes79))
# })
# test_that("CreateSubjectTag -Scenario 2", {
#   ids <- c(71:80, 1:10)
#   generation <- c(rep(2, 10), rep(1, 10))
#   expected <- c(71:80, 1:10*100)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, CreateSubjectTag(ids, generation))
# })
# test_that("CreateSubjectTag -Scenario 3", {
#   ids <- c(NA, NA, 71:80, NA, 1:10, NA)
#   generation <- c(rep(2, 12), rep(1, 12))
#   expected <- c(NA, NA,71:80, NA, 1:10*100, NA)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, CreateSubjectTag(ids, generation))
# })
# test_that("CreateSubjectTag -Scenario 4", {
#   ids <- c(71:82, 1:12)
#   generation <- c(NA, NA, rep(2, 10), NA, rep(1, 10), NA)
#   expected <- c(NA, NA,73:82, NA, 2:11*100, NA)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, CreateSubjectTag(ids, generation))
# })
# test_that("CreateSubjectTag -Scenario 5", {
#   ids <- c(71:82, 10001:10012)
#   generation <- c(NA, NA, rep(1, 10), NA, rep(2, 10), NA)
#   expected <- c(NA, NA,73:82*100, NA, 10002:10011, NA)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, CreateSubjectTag(ids, generation))
# })
# 
# test_that("CreateSubjectTag -Scenario 6", {
#   ids <- c(71:82, 10001:10012)
#   generation <- c( rep(1, 12),rep(2, 12))
#   expected <- c(71:82*100, 10001:10012)
#   expect_equal(expected, CreateSubjectTag(ids, generation))
#   expect_equivalent(expected, CreateSubjectTag(ids, generation))
# })
# 
# test_that("CreateSubjectTag -With ExtraOutcomes79", {
#   data(ExtraOutcomes79)
#   
#   actual <- CreateSubjectTag(subjectID=ExtraOutcomes79$SubjectID, generation=ExtraOutcomes79$Generation)
#   expected <- ExpectedSubjectTags
#   
#   expect_equal(expected, actual)
#   #cat(PrintVector(ExtraOutcomes79$SubjectTag))  
# })



# 
# test_that("Zero rows", {
#   dsLinks <- LoadPairFile()
#   dsLinks <- dsLinks[0,]
#   expect_error(ValidatePairLinks(dsLinks), "The linksPair file should have at least one row, but does not.")
# })
# 
# test_that("Bad Subject1Tag", {
#   dsLinks <- LoadPairFile()
#   expect_true(ValidatePairLinks(dsLinks))
#   colnames(dsLinks)[colnames(dsLinks)=="Subject1Tag"] <- "Bad"
#   expect_error(ValidatePairLinks(dsLinks), "The column 'Subject1Tag' should exist in the linksPair file, but does not.")
# })
# 
# test_that("Bad Subject2Tag", {
#   dsLinks <- LoadPairFile()
#   expect_true(ValidatePairLinks(dsLinks))
#   colnames(dsLinks)[colnames(dsLinks)=="Subject2Tag"] <- "Bad"
#   expect_error(ValidatePairLinks(dsLinks), "The column 'Subject2Tag' should exist in the linksPair file, but does not.")
# })
# 
# test_that("Bad R", {
#   dsLinks <- LoadPairFile()
#   expect_true(ValidatePairLinks(dsLinks))
#   colnames(dsLinks)[colnames(dsLinks)=="R"] <- "Bad"
#   expect_error(ValidatePairLinks(dsLinks), "The column 'R' should exist in the linksPair file, but does not.")
# })
# 
# # test_that("Bad MultipleBirth", {
# #   dsLinks <- LoadPairFile()
# #   expect_true(ValidatePairLinks(dsLinks))
# #   colnames(dsLinks)[colnames(dsLinks)=="MultipleBirth"] <- "Bad"
# #   expect_error(ValidatePairLinks(dsLinks), "The column 'MultipleBirth' should exist in the linksPair file, but does not.")
# # })
