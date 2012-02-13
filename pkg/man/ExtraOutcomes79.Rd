\name{ExtraOutcomes79}
\alias{ExtraOutcomes79}
\docType{data}
\title{
Extra outcome variables in the NLSY79
}
\description{
This dataset is provided primarily to facilitate documentation examples.

Currently this dataset contains only Gen2 subjects.  However, it soon will include Gen1 subjects.  Code written using this dataset should NOT assume it contains only Gen2 subjects.  See below for an example of filtering by generation.
}
\usage{data(ExtraOutcomes79)}
\format{
  A data frame with 11,495 observations on the following 6 variables.  There is one row per subject.
  \describe{
    \item{\code{SubjectID}}{The ID value assigned by NLS to the first subject.  For Gen1 Subjects, this is their "CaseID" (ie, R00001.00).  For Gen2 subjects, this is their "CID" (ie, C00001.00).}
    \item{\code{Generation}}{The generation of the subject.  Values are either 1 or 2, representing Gen1 and Gen2.  Note that this variable is not a \code{factor} (in constrast with data frames like \code{\link{Links79Pair}}).   This dataset is supposed to mimick the dataset provided by the researcher, which typically will not have been converted to a \code{factor}.}
    \item{\code{MathStandardized}}{The standardized PIAT math score, with mean 100.}
    \item{\code{Weight}}{The subject's last recorded weight (in pounds).}
    \item{\code{WeightForAge19To25}}{The subject's weight, when aged between 19 and 25 years old (in pounds).  If multiple measurements were available between 19 & 25 (inclusive), the youngest was taken.}
    \item{\code{WeightStandardized}}{The subject's last recorded weight, standardized by gender (as a z-score).}
    \item{\code{WeightStandardizedForAge19To25}}{The subject's last recorded weight, standardized by gender, when aged between 19 and 25 years old (as a z-score).  If multiple measurements were available between 19 & 25 (inclusive), the youngest was taken.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
Gen1 information comes from the May 15, 2010 release of the \href{http://www.bls.gov/nls/nlsy79.htm}{NLSY79 sample}.  Gen2 information comes from the Sept 15, 2010 release of the \href{http://www.bls.gov/nls/nlsy79ch.htm}{NLSY79 Children and Young Adults sample}.  Data were extracted with the NLS Investigator (\url{https://www.nlsinfo.org/investigator/}). **Kelly, is this correct? **

For Gen2, an NLSY79 variable of \code{MathStandardized} is C00000.00? **Kelly, can you give me an RNum for this? **

For Gen2, an NLSY79 variable of \code{Weight} is C00000.00? **Kelly, can you give me an RNum for this too? **

Data were simplied (to one value per subject) by Kelly Meredith in January 2012.
}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
library(NlsyLinks) #Load the package into the current R session.
data(ExtraOutcomes79)  #Load the dataset from the NlsyLinks package.
gen2Outcomes <- subset(ExtraOutcomes79, Generation==2) #Create a dataset of only Gen2 subjects.
                  
plot(ExtraOutcomes79)
summary(ExtraOutcomes79)

oldPar <- par(mfrow=c(3,2))
hist(ExtraOutcomes79$Generation)
hist(ExtraOutcomes79$MathStandardized)
hist(ExtraOutcomes79$Weight)
hist(ExtraOutcomes79$WeightForAge19To25)
hist(ExtraOutcomes79$WeightStandardized)
hist(ExtraOutcomes79$WeightStandardizedForAge19To25)
par(oldPar)
}
\keyword{datasets}
