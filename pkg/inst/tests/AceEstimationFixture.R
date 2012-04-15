

###########
context("Ace Estimation")
###########
test_that("AceUnivariate -NULL method", {
  #CreateAceEstimate(.5, .2, .1, 4)
#   dsOutcomes <- ExtraOutcomes79
#   dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,generation=dsOutcomes$Generation)
#   dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))
#   
#   expect_error(
#     AceUnivariate(outcomeForSubject1=dsDF$MathStandardized_1, outcomeForSubject2=dsDF$MathStandardized_2, relatedness=dsDF$R, method=NULL),
#     "The method argument must contain exactly one element when calling the AceUnivariate function.  It contained 0 elements."
#   )
})

aSquared <- .5
cSquared  <- .2
eSquared <-.1
caseCount <- 20
componentSum <- aSquared + cSquared + eSquared
#print(class(caseCount))

unity <- ( abs(componentSum - 1.0) < 0 )
withinBounds <- (0 <= min(aSquared, cSquared, eSquared)) && (max( aSquared, cSquared, eSquared) <= 1)
est <-new("AceEstimate", aSquared, cSquared, eSquared, caseCount, unity, withinBounds) 
est@ASquared
est
showClass("AceEstimate")