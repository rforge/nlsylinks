\name{ValidatePairLinks}
\alias{ValidatePairLinks}

\title{
Validates the schema of a links for pairs of relatives
}
\description{
A helper function that verifies the linking dataset contains (A) the essential columns exist, and (B) at least one row.  It is called by \code{CreatePairLinks}.
}
\usage{
ValidatePairLinks(linksPair)
}
\arguments{
  \item{linksPair}{The \code{data.frame} to validate.}
}
\details{
Typical use of \pkg{NlsyLinks} will not require this function, since a valid paired links are supplied for each supported sample (ie, \code{\link{Links79Pair}}).

The \pkg{NlsyLinks} uses several types of linking schemas.  This function validates the type where each relative subject has their own row.

The following four columns must be present: (1) \code{Subect1Tag}, (2) \code{Subect2Tag}, (3) \code{R}, and (4) \code{MultipleBirth}.  They must have a \code{numeric} mode/datatype.
}
\value{
Returns \code{TRUE} if the validation passes.
Returns an error (and associated descriptive message) if it false.
}
\author{
Will Beasley
}


\seealso{
\code{\link{Links79Pair}},\code{\link{Links79PairExpanded}},
}
\examples{
##Need something here
}
\keyword{ validation }
