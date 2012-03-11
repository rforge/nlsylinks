rm(list=ls(all=TRUE)) #Clear all the variables before starting a new run.

require(testthat)
#test_dir("F:/Projects/RDev/NlsyLinksStaging/Static/tests")
#trace(ValidatePairLinks)

directory <- "F:/Projects/RDev/NlsyLinksStaging/"
directoryTests <- paste(directory, "Static/inst/tests/", sep="")
pathToBeIncorporated <- paste(directory, "Content/ToBeIncorporated.R", sep="")

ClearMostVariables <- function( ) {
  rm(list=ls(all=TRUE)[!(ls(all=TRUE) %in% c("ClearMostVariables", "directoryTests", "pathToBeIncorporated" ))])
}

try(detach("package:NlsyLinks"), silent=TRUE)
require(NlsyLinks)
?NlsyLinks

source(pathToBeIncorporated)

#expect_true(FALSE, "AAAAAAAAAAAAAAAAAAAAAAAAAAAA This test should fail during installation or checking?")

ClearMostVariables()
#source(pathToBeIncorporated)
test_file(paste(directoryTests, "CreatePairDatasetFixture.R", sep=""))

ClearMostVariables()
#source(pathToBeIncorporated)
test_file(paste(directoryTests, "DFFixture.R", sep=""))

ClearMostVariables()
#source(pathToBeIncorporated)
test_file(paste(directoryTests, "AceWrapperExceptions.R", sep=""))

ClearMostVariables()
test_file(paste(directoryTests, "OutcomeDatasetFixture.R", sep=""))

ClearMostVariables()
test_file(paste(directoryTests, "ReadCsvFixture.R", sep=""))

ClearMostVariables()
source(paste(directoryTests, "ExpectedVectors.R", sep=""))
test_file(paste(directoryTests, "ColumnUtilitiesFixture.R", sep=""))

