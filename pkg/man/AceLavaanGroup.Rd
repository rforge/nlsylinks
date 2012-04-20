\name{AceLavaanGroup}
\alias{AceLavaanGroup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A simple multiple-group ACE model with the \pkg{lavaan} package.
}
\description{
This function uses the \pkg{lavaan} package to estimate a univariate ACE model, using multiple groups.  Each group has a unique value of \code{R} (i.e., the \emph{R}elatedness coefficient).
}
\usage{
AceLavaanGroup(dsClean, rLevels, m1Name, m2Name, rName="R", 
  estimateA=TRUE, estimateC=TRUE, printOutput=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dsClean}{The \code{data.frame} containing complete cases for the \code{R} groups to be included in the estimation.}
  \item{rLevels}{The values of \code{R} forming each group.}
  \item{m1Name}{The name of the manifest variable for the \emph{first} subject in each pair.}
  \item{m2Name}{The name of the manifest variable for the \emph{second} subject in each pair.}
  \item{rName}{The name of the variable containing the \code{R} value.}
  \item{estimateA}{Should the \emph{A} variance component be estimated?  A^2 represents the proportion of variability due to a shared genetic influence.}
  \item{estimateC}{Should the \emph{C} variance component be estimated?  C^2 represents the proportion of variability due to a shared environmental influence.}
  \item{printOutput}{Indicates if the estimated parameters and fit statistics are printed to the console.}
}
\details{
The variance component for \emph{E} is always estimated.

}
\value{
An \code{AceEstimate} object.
}
\references{
The \pkg{lavaan} package is developed by Yves Rosseel at Ghent University.  Two good starting points are the documentation (\url{http://cran.r-project.org/web/packages/lavaan/}) and his upcoming JSS paper (\url{http://users.ugent.be/~yrosseel/lavaan/lavaanJSSpreview.pdf}).
}
\author{
Will Beasley
}
%\note{
%%  ~~further notes~~
%}


\seealso{
Further ACE model details are discussed in our package's vignette (in the console, type \code{vignette("NlsyAce")}).
}
\examples{
dsFull <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
m1Name <- "MathStandardized_1" #Stands for Manifest1
m2Name <- "MathStandardized_2" #Stands for Manifest2

dsGroupSummary <- RGroupSummary(dsFull, m1Name, m2Name)
rLevels <- dsGroupSummary[dsGroupSummary$Included, "R"]
dsClean <- CleanSemAceDataset(dsDirty=dsFull, dsGroupSummary=dsGroupSummary, m1Name=m1Name, m2Name=m2Name)

ace <- AceLavaanGroup(dsClean, rLevels, m1Name, m2Name)
ace

#Should produce:
# [1] "Results of ACE estimation: [show]"
#     ASquared     CSquared     ESquared    CaseCount 
#    0.6701032    0.1167060    0.2131907 8292.0000000

library(lavaan) #Load the lavaan package to access lavaan-specific functions
GetDetails(ace)

#Exmaine fit stats like Chi-Squared, RMSEA, CFI, etc.
fitMeasures(GetDetails(ace)$lavaan)

#Examine low-level details like each group's individual parameter estimates and standard errors.
summary(GetDetails(ace)$lavaan)

#Extract low-level details. This may be useful when programming simulations.
inspect(GetDetails(ace)$lavaan, what="converged")
inspect(GetDetails(ace)$lavaan, what="coef")
}
\keyword{ ACE }

