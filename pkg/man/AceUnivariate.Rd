\name{AceUnivariate}
\alias{AceUnivariate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AceUnivariate(outcomeForSubject1, outcomeForSubject2, relatedness, method = c("DeFriesFulkerMethod1", "DeFriesFulkerMethod3"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcomeForSubject1}{
%%     ~~Describe \code{outcomeForSubject1} here~~
}
  \item{outcomeForSubject2}{
%%     ~~Describe \code{outcomeForSubject2} here~~
}
  \item{relatedness}{
%%     ~~Describe \code{relatedness} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (outcomeForSubject1, outcomeForSubject2, relatedness, 
    method = c("DeFriesFulkerMethod1", "DeFriesFulkerMethod3")) 
{
    if (length(method) != 1) 
        stop(paste("The method argument must contain exactly one element when calling the AceUnivariate function.  It contained ", 
            length(method), " elements.", sep = ""))
    else if (method == "") 
        stop(paste("The method argument must contain exactly one element when calling the AceUnivariate function.  It was blank.", 
            sep = ""))
    else if (method == "DeFriesFulkerMethod1") 
        return(DeFriesFulkerMethod1(outcomeForSubject1, outcomeForSubject2, 
            relatedness))
    else if (method == "DeFriesFulkerMethod3") 
        return(DeFriesFulkerMethod3(outcomeForSubject1, outcomeForSubject2, 
            relatedness))
    else stop(paste("The method argument, '", method, "' was not recognized as a valid option to the AceUnivariate function.", 
        sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
