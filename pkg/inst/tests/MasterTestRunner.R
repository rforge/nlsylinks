rm(list=ls(all=TRUE)) #Clear all the variables before starting a new run.

require(testthat)
#test_dir("F:/Projects/RDev/NlsyLinksStaging/Static/tests")
#trace(ValidatePairLinks)


detach("package:NlsyLinks")
require(NlsyLinks)
?NlsyLinks

source("F:/Projects/RDev/NlsyLinksStaging/Content/ToBeIncorporated.R")

#expect_true(FALSE, "AAAAAAAAAAAAAAAAAAAAAAAAAAAA This test should fail during installation or checking?")

test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/CreatePairDatasetFixture.R")
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/UtilitiesFixture.R")
#untrace(ValidatePairLinks)
