

###########
context("Ace Wrapper Exceptions")
###########
test_that("AceUnivariate -NULL method", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  
  expect_error(
    AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method=NULL),
    "The method argument must contain exactly one element when calling the AceUnivariate function.  It contained 0 elements."
  )
})
test_that("AceUnivariate -blank method", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  
  expect_error(
    AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method=""),
    "The method argument must contain exactly one element when calling the AceUnivariate function.  It was blank."
  )
})
test_that("AceUnivariate -unrecognized name method", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  
  expect_error(
    AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method="ddddd"),
    "The method argument, 'ddddd' was not recognized as a valid option to the AceUnivariate function."
  )
})
test_that("AceUnivariate -multiple methods passed", {
  dsOutcomes <- ExtraOutcomes79
  dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
  dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
  
  expect_error(
    AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method=c("DeFriesFulkerMethod1", "DeFriesFulkerMethod3")),
    "The method argument must contain exactly one element when calling the AceUnivariate function.  It contained 2 elements."
  )
})