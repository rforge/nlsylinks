\name{AceEstimate-class}
\Rdversion{1.1}
\docType{class}
\alias{AceEstimate-class}
\alias{getEstimate,AceEstimate-method}
\alias{initialize,AceEstimate-method}
\alias{print,AceEstimate-method}
\alias{show,AceEstimate-method}

\title{Class \code{"AceEstimate"}}
\description{
A class containing information about a single univariate ACE model.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AceEstimate", aSquared, cSquared, eSquared, caseCount, unity, withinBounds, ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ASquared}:}{The proportion of variability due to a shared genetic influence (typically represented as a^2, or sometimes h^2). }
    \item{\code{CSquared}:}{The proportion of variability due to shared common environmental influence.}
    \item{\code{ESquared}:}{The proportion of variability due to unexplained/residual/error influence.}
    \item{\code{CaseCount}:}{The number cases contributing to this estimate.}
    \item{\code{Unity}:}{Indicates if the three components sum to 1.}
    \item{\code{WithinBounds}:}{Indicates if the three components are each bounded by [0, 1].}
  }
}
\section{Methods}{
  \describe{
    \item{GetEstimate}{\code{signature(object = "AceEstimate")}: ... }
    \item{initialize}{\code{signature(.Object = "AceEstimate")}: ... }
    \item{print}{\code{signature(x = "AceEstimate")}: ... }
    \item{show}{\code{signature(object = "AceEstimate")}: ... }
	 }
}
%\references{
%%  ~~put references to the literature/web site here~~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
%\examples{
%require(NlsyLinks)
%showClass("AceEstimate")
%}
\keyword{classes}
