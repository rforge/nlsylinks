\name{CreatePairLinksSingleEntered}
\alias{CreatePairLinksSingleEntered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreatePairLinksSingleEntered(outcomeDataset, linksPairDataset, outcomeNames, linksNames = c("ExtendedID", "R", "RelationshipPath"), validateOutcomeDataset = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcomeDataset}{
%%     ~~Describe \code{outcomeDataset} here~~
}
  \item{linksPairDataset}{
%%     ~~Describe \code{linksPairDataset} here~~
}
  \item{outcomeNames}{
%%     ~~Describe \code{outcomeNames} here~~
}
  \item{linksNames}{
%%     ~~Describe \code{linksNames} here~~
}
  \item{validateOutcomeDataset}{
%%     ~~Describe \code{validateOutcomeDataset} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (outcomeDataset, linksPairDataset, outcomeNames, linksNames = c("ExtendedID", 
    "R", "RelationshipPath"), validateOutcomeDataset = TRUE) 
{
    ValidatePairLinks(linksPairDataset)
    if (validateOutcomeDataset) 
        ValidateOutcomeDataset(dsOutcome = outcomeDataset, outcomeNames = outcomeNames)
    dsLinksLeftHand <- subset(linksPairDataset, select = c("Subject1Tag", 
        "Subject2Tag", linksNames))
    dsOutcomeSubject1 <- subset(outcomeDataset, select = c("SubjectTag", 
        outcomeNames))
    dsOutcomeSubject2 <- subset(outcomeDataset, select = c("SubjectTag", 
        outcomeNames))
    for (j in 1:ncol(dsOutcomeSubject1)) {
        columnName <- colnames(dsOutcomeSubject1)[j]
        if (columnName \%in\% outcomeNames) {
            colnames(dsOutcomeSubject1)[colnames(dsOutcomeSubject1) == 
                columnName] <- paste(columnName, "_1", sep = "")
            colnames(dsOutcomeSubject2)[colnames(dsOutcomeSubject2) == 
                columnName] <- paste(columnName, "_2", sep = "")
        }
    }
    ds <- merge(x = dsLinksLeftHand, y = dsOutcomeSubject1, by.x = "Subject1Tag", 
        by.y = "SubjectTag")
    ds <- merge(x = ds, y = dsOutcomeSubject2, by.x = "Subject2Tag", 
        by.y = "SubjectTag")
    rm(dsLinksLeftHand, dsOutcomeSubject1, dsOutcomeSubject2)
    firstTwoNames <- c("Subject1Tag", "Subject2Tag")
    remaining <- setdiff(colnames(ds), firstTwoNames)
    ds <- ds[, c(firstTwoNames, remaining)]
    return(ds)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
