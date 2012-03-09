\name{Ace}
\alias{AceUnivariate}
\alias{DeFriesFulkerMethod1}
\alias{DeFriesFulkerMethod3}

\title{
Estimates the heritability of additive traits using a single variable.
}
\description{
An ACE model is the foundation of most Behavior Genetic research.  It estimates the additive heritability (with \emph{a}), common environment (with \emph{c}) and unshared heritability/environment (with \emph{e}).
}
\usage{
AceUnivariate(outcomeForSubject1, outcomeForSubject2, relatedness, 
  method=c("DeFriesFulkerMethod1", "DeFriesFulkerMethod3"))
DeFriesFulkerMethod1(outcomeForSubject1, outcomeForSubject2, relatedness)
DeFriesFulkerMethod3(outcomeForSubject1, outcomeForSubject2, relatedness)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcomeForSubject1}{The outcome variable corresponding to the first subject in the pair. }
  \item{outcomeForSubject2}{The outcome variable corresponding to the second subject in the pair. }
  \item{relatedness}{The relatedness coefficient for the pair (this is typically abbreviated as \code{R}).}
  \item{method}{The specific estimation technique.}
}
\details{
The \code{AceUnivariate} function is a wrapper that calls \code{DeFriesFulkerMethod1} or \code{DeFriesFulkerMethod3}.  Future versions will incorporate methods that use latent variable models.
}
\value{
Currently, a list is returned with the arguments \code{HSquared}, \code{CSquared}, \code{ESquared}, and \code{RowCount}.  In the future, this may be changed to an \code{S4} class.
}
\references{
Rodgers, Joseph Lee, & Kohler, Hans-Peter (2005).  Reformulating and simplifying the DF analysis model.  \href{http://www.springerlink.com/content/n3x1v1q282583366/}{\emph{Behavior Genetics, 35} (2), 211-217}.
}
\author{
Will Beasley
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(NlsyLinks) #Load the package into the current R session.
dsOutcomes <- ExtraOutcomes79
dsOutcomes$SubjectTag <- CreateSubjectTag(subjectID=dsOutcomes$SubjectID,
  generation=dsOutcomes$Generation)
dsDF <- CreatePairLinksDoubleEntered(outcomeDataset=dsOutcomes, linksPairDataset=Links79Pair, 
  outcomeNames=c("MathStandardized", "Weight", "WeightStandardized", "WeightStandardizedForAge19To25"))

estimatedAdultWeight <- DeFriesFulkerMethod3(
  outcomeForSubject1=dsDF$WeightStandardizedForAge19To25_1, 
  outcomeForSubject2=dsDF$WeightStandardizedForAge19To25_2, 
  relatedness=dsDF$R)  
# The HSquared and CSquared values should be 0.633936 and 0.006209162.

estimatedMath <- DeFriesFulkerMethod3(
  outcomeForSubject1=dsDF$MathStandardized_1, 
  outcomeForSubject2=dsDF$MathStandardized_2, 
  relatedness=dsDF$R)  
# The HSquared and CSquared values should be 0.8595078 and 0.03879863.
}
