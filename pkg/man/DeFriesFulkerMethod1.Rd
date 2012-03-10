\name{DeFriesFulkerMethod1}
\alias{DeFriesFulkerMethod1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DeFriesFulkerMethod1(outcomeForSubject1, outcomeForSubject2, relatedness)
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
function (outcomeForSubject1, outcomeForSubject2, relatedness) 
{
    brief <- summary(lm(outcomeForSubject1 ~ 1 + outcomeForSubject2 + 
        relatedness + outcomeForSubject2 * relatedness))
    coeficients <- coef(brief)
    nDouble <- length(brief$residuals)
    b1 <- coeficients["outcomeForSubject2", "Estimate"]
    b3 <- coeficients["outcomeForSubject2:relatedness", "Estimate"]
    eSquared <- 1 - (b1 + b3)
    return(list(HSquared = b3, CSquared = b1, ESquared = eSquared, 
        RowCount = nDouble))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
