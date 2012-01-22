
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

###########
context("CreatePairLinks")
###########
# test_that("CreatePairLinks -Normal Scenario", {
#   dsLinks <- LoadPairFile()
#   dsDVs <- LoadDVFile()
#   dsLinksWithExtraDV <- CreatePairLinks(dvNames="", dvDataset=dsDVs, linksPairDataset=dsLinks)
#   expect_equal(nrow(dsLinksWithExtraDV), 22150, info="The number of rows in the pairs links should be correct.")
# })



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
  expect_error(ValidatePairLinks(dsLinks), "The linksPair file should have at least one row, but does not.")
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
