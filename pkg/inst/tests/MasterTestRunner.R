rm(list=ls(all=TRUE)) #Clear all the variables before starting a new run.

require(testthat)
#test_dir("F:/Projects/RDev/NlsyLinksStaging/Static/tests")
#trace(ValidatePairLinks)


try(detach("package:NlsyLinks"), silent=TRUE)
require(NlsyLinks)
?NlsyLinks

source("F:/Projects/RDev/NlsyLinksStaging/Content/ToBeIncorporated.R")

#expect_true(FALSE, "AAAAAAAAAAAAAAAAAAAAAAAAAAAA This test should fail during installation or checking?")

rm(list=ls(all=TRUE))
#source("F:/Projects/RDev/NlsyLinksStaging/Content/ToBeIncorporated.R")
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/CreatePairDatasetFixture.R")

rm(list=ls(all=TRUE))
#source("F:/Projects/RDev/NlsyLinksStaging/Content/ToBeIncorporated.R")
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/DFFixture.R")

rm(list=ls(all=TRUE))
#source("F:/Projects/RDev/NlsyLinksStaging/Content/ToBeIncorporated.R")
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/AceWrapperExceptions.R")


rm(list=ls(all=TRUE))
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/OutcomeDatasetFixture.R")

rm(list=ls(all=TRUE))
source("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/ExpectedVectors.R")
test_file("F:/Projects/RDev/NlsyLinksStaging/Static/inst/tests/UtilitiesFixture.R")

