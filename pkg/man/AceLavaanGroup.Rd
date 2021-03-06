% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/AceLavaanGroup.R
\name{AceLavaanGroup}
\alias{AceLavaanGroup}
\title{A simple multiple-group ACE model with the \pkg{lavaan} package.}
\usage{
AceLavaanGroup(dsClean, estimateA=TRUE, estimateC=TRUE, printOutput=FALSE)
}
\arguments{
\item{dsClean}{The \code{data.frame} containing complete cases for the \code{R} groups to be included in the estimation.}

\item{estimateA}{Should the \emph{A} variance component be estimated?  A^2 represents the proportion of variability due to a shared genetic influence.}

\item{estimateC}{Should the \emph{C} variance component be estimated?  C^2 represents the proportion of variability due to a shared environmental influence.}

\item{printOutput}{Indicates if the estimated parameters and fit statistics are printed to the console.}
}
\value{
An \code{AceEstimate} object.
}
\description{
This function uses the \pkg{lavaan} package to estimate a univariate ACE model, using multiple groups.
Each group has a unique value of \code{R} (i.e., the \emph{R}elatedness coefficient).
}
\details{
The variance component for \emph{E} is always estimated, while the \emph{A} and \emph{C} estimates can be fixed to zero (when \code{estimateA} and/or \emph{estimateC} are set to \code{FALSE}).
}
\note{
Currently, the variables in \code{dsClean} must be named \code{O1}, \code{O2} and \code{R}; the letter `O' stands for \emph{O}utcome.  This may not be as restrictive as it initially seems, because \code{dsClean} is intented to be produced by \code{CleanSemAceDataset}.  If this is too restrictive for your uses, we'd like to here about it (\emph{please email wibeasley at hotmail period com}).
}
\examples{
library(NlsyLinks) #Load the package into the current R session.
dsLinks <- Links79PairExpanded #Start with the built-in data.frame in NlsyLinks
dsLinks <- dsLinks[dsLinks$RelationshipPath=='Gen2Siblings', ] #Use only Gen2 Siblings (NLSY79-C)

oName_S1 <- "MathStandardized_S1" #Stands for Outcome1
oName_S2 <- "MathStandardized_S2" #Stands for Outcome2

dsGroupSummary <- RGroupSummary(dsLinks, oName_S1, oName_S2)
dsClean <- CleanSemAceDataset(dsDirty=dsLinks, dsGroupSummary, oName_S1, oName_S2)

ace <- AceLavaanGroup(dsClean)
ace

#Should produce:
# [1] "Results of ACE estimation: [show]"
#     ASquared     CSquared     ESquared    CaseCount
#    0.6681874    0.1181227    0.2136900 8390.0000000

library(lavaan) #Load the package to access methods of the lavaan class.
GetDetails(ace)

#Exmaine fit stats like Chi-Squared, RMSEA, CFI, etc.
fitMeasures(GetDetails(ace)) #The function 'fitMeasures' is defined in the lavaan package.

#Examine low-level details like each group's individual parameter estimates and standard errors.
summary(GetDetails(ace))

#Extract low-level details. This may be useful when programming simulations.
inspect(GetDetails(ace), what="converged") #The lavaan package defines 'inspect'.
inspect(GetDetails(ace), what="coef")
}
\author{
Will Beasley
}
\references{
The \pkg{lavaan} package is developed by Yves Rosseel at Ghent University.
Three good starting points are the package home page (\url{http://lavaan.ugent.be/}), the documentation (\url{http://cran.r-project.org/web/packages/lavaan/})
and the JSS paper.

Rosseel, Yves (2012), \href{http://www.jstatsoft.org/v48/i02/}{lavaan: An R Package for Structural Equation Modeling}. \emph{Journal of Statistical Software, 48}, (2), 1-36.
}
\seealso{
\code{\link{CleanSemAceDataset}}.  Further ACE model details are discussed in our package's \href{http://cran.r-project.org/web/packages/NlsyLinks/}{vignettes}.
}
\keyword{ACE}

