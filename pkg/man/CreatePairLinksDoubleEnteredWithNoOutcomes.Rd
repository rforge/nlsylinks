\name{CreatePairLinksDoubleEnteredWithNoOutcomes}
\alias{CreatePairLinksDoubleEnteredWithNoOutcomes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreatePairLinksDoubleEnteredWithNoOutcomes(linksPairDataset, linksNames = c("ExtendedID", "R", "RelationshipPath"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{linksPairDataset}{
%%     ~~Describe \code{linksPairDataset} here~~
}
  \item{linksNames}{
%%     ~~Describe \code{linksNames} here~~
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
function (linksPairDataset, linksNames = c("ExtendedID", "R", 
    "RelationshipPath")) 
{
    ValidatePairLinks(linksPairDataset)
    dsLinksLeftHand <- subset(linksPairDataset, select = c("Subject1Tag", 
        "Subject2Tag", linksNames))
    dsLinksRightHand <- subset(linksPairDataset, select = c("Subject2Tag", 
        "Subject1Tag", linksNames))
    colnames(dsLinksRightHand)[colnames(dsLinksRightHand) == 
        "Subject1Tag"] <- "SubjectTempTag"
    colnames(dsLinksRightHand)[colnames(dsLinksRightHand) == 
        "Subject2Tag"] <- "Subject1Tag"
    colnames(dsLinksRightHand)[colnames(dsLinksRightHand) == 
        "SubjectTempTag"] <- "Subject2Tag"
    ds <- rbind(dsLinksLeftHand, dsLinksRightHand)
    ds <- ds[, c("Subject1Tag", "Subject2Tag", linksNames)]
    rm(dsLinksLeftHand, dsLinksRightHand)
    return(ds)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
