\name{CreateAceEstimate}
\alias{CreateAceEstimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Instantiate an \code{\link{AceEstimate-class}} object.
}
\description{
Creates an instance of the \code{S4} class, \code{AceEstimate}  instantiating arguments set the parameter values estimated by the ACE model.
}
\usage{
CreateAceEstimate(aSquared, cSquared, eSquared, caseCount, unityTolerance = 1e-11)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aSquared}{The proportion of variability due to a shared genetic influence (typically represented as a^2, or sometimes h^2).}
  \item{cSquared}{The proportion of variability due to shared common environmental influence.}
  \item{eSquared}{The proportion of variability due to unexplained/residual/error influence.}
  \item{caseCount}{The number of cases used to estimate the model.}
  \item{unityTolerance}{
%%     ~~Describe \code{unityTolerance} here~~
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
An S4 object of \code{\link{AceEstimate-class}}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Will Beasley
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ACE }
