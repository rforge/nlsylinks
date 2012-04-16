

###########
context("Ace Estimation")
###########
test_that("CreateAceEstimate -Plain", {
  aSquared <- .5
  cSquared  <- .3
  eSquared <- .2
  caseCount <- 20
  
  est <- CreateAceEstimate(aSquared, cSquared, eSquared, caseCount)
  expect_equal(object=slot(est, "ASquared"), expected=aSquared, scale=1)
  expect_equal(object=slot(est, "CSquared"), expected=cSquared, scale=1)
  expect_equal(object=slot(est, "ESquared"), expected=eSquared, scale=1)
  expect_equal(object=slot(est, "CaseCount"), expected=caseCount, scale=1)
  expect_true(object=slot(est, "Unity"))
  expect_true(object=slot(est, "WithinBounds"))
})
test_that("CreateAceEstimate -Not Unity", {
  aSquared <- .5
  cSquared  <- .2
  eSquared <- .1
  caseCount <- 20
  
  est <- CreateAceEstimate(aSquared, cSquared, eSquared, caseCount)
  expect_equal(object=slot(est, "ASquared"), expected=aSquared, scale=1)
  expect_equal(object=slot(est, "CSquared"), expected=cSquared, scale=1)
  expect_equal(object=slot(est, "ESquared"), expected=eSquared, scale=1)
  expect_equal(object=slot(est, "CaseCount"), expected=caseCount, scale=1)
  expect_false(object=slot(est, "Unity"))
  expect_true(object=slot(est, "WithinBounds"))
})
test_that("CreateAceEstimate -Outside bounds", {
  aSquared <- .7
  cSquared  <- .5
  eSquared <- -.2
  caseCount <- 20
  
  est <- CreateAceEstimate(aSquared, cSquared, eSquared, caseCount)
  expect_equal(object=slot(est, "ASquared"), expected=aSquared, scale=1)
  expect_equal(object=slot(est, "CSquared"), expected=cSquared, scale=1)
  expect_equal(object=slot(est, "ESquared"), expected=eSquared, scale=1)
  expect_equal(object=slot(est, "CaseCount"), expected=caseCount, scale=1)
  expect_true(object=slot(est, "Unity"))
  expect_false(object=slot(est, "WithinBounds"))
})
test_that("CreateAceEstimate -Outside bounds & no unity", {
  aSquared <- .4
  cSquared  <- .5
  eSquared <- -.2
  caseCount <- 20
  
  est <- CreateAceEstimate(aSquared, cSquared, eSquared, caseCount)
  expect_equal(object=slot(est, "ASquared"), expected=aSquared, scale=1)
  expect_equal(object=slot(est, "CSquared"), expected=cSquared, scale=1)
  expect_equal(object=slot(est, "ESquared"), expected=eSquared, scale=1)
  expect_equal(object=slot(est, "CaseCount"), expected=caseCount, scale=1)
  expect_false(object=slot(est, "Unity"))
  expect_false(object=slot(est, "WithinBounds"))
})

# aSquared <- .5
# cSquared  <- .2
# eSquared <- .1
# caseCount <- 20
# componentSum <- aSquared + cSquared + eSquared
# #print(class(caseCount))
# 
# unity <- ( abs(componentSum - 1.0) < 0 )
# withinBounds <- (0 <= min(aSquared, cSquared, eSquared)) && (max( aSquared, cSquared, eSquared) <= 1)
# est <-new("AceEstimate", aSquared, cSquared, eSquared, caseCount, unity, withinBounds) 
# est@ASquared
# est
# show(est)
# print(est)
# 
# est2 <- CreateAceEstimate(.5, .2, .1, 4)
# est2@ASquared
# est2
# showClass("AceEstimate")

# showMethods(GetEstimate)
# showMethods(print)
# showMethods(show)