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
  \item{linksPair}{The data.frame to validate.}
}
\details{
Typical use of \pkg{NlsyLinks} will not require this function, since a valid paired links are supplied for each supported sample (ie, \code{\link{Links79Pair}}).

The \pkg{NlsyLinks} uses several types of linking schemas.  This function validates the type where each relative subject has their own row.

The following four columns must be present: (1)\code{Subect1Tag}, (1)\code{Subect2Tag}, (1)\code{R}, and (4)\code{MultipleBirth}.  They must have a \code{numeric} mode/datatype.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (linksPair) 
{
    if (!nrow(linksPair) > 0) 
        stop("The linksPair file should have at least one row, but does not.")
    columnNames <- colnames(linksPair)
    if (!any(columnNames == "Subject1Tag")) 
        stop("The column 'Subject1Tag' should exist in the linksPair file, but does not.")
    if (!any(columnNames == "Subject2Tag")) 
        stop("The column 'Subject2Tag' should exist in the linksPair file, but does not.")
    if (!any(columnNames == "R")) 
        stop("The column 'R' should exist in the linksPair file, but does not.")
    if (!any(columnNames == "MultipleBirth")) 
        stop("The column 'MultipleBirth' should exist in the linksPair file, but does not.")
    if (mode(linksPair$Subject1Tag) != "numeric") 
        stop("The column 'Subject1Tag' should have a 'numeric' mode, but does not.")
    if (mode(linksPair$Subject2Tag) != "numeric") 
        stop("The column 'Subject2Tag' should have a 'numeric' mode, but does not.")
    if (mode(linksPair$R) != "numeric") 
        stop("The column 'R' should have a 'numeric' mode, but does not.")
    if (mode(linksPair$MultipleBirth) != "numeric") 
        stop("The column 'MultipleBirth' should have a 'numeric' mode, but does not.")
    return(TRUE)
  }
}

