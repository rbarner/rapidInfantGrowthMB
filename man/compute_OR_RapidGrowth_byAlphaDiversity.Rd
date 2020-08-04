\name{compute_OR_RapidGrowth_byAlphaDiversity}
\alias{compute_OR_RapidGrowth_byAlphaDiversity}

\title{
Compute odds ratio of rapid growth status by alpha diversity indices.
}
\description{
This function will output the results of multiple logistic regression models determining association of rapid growth status and diversity indices. The models adjust for birthweight and birthlength.
}
\usage{
compute_OR_RapidGrowth_byAlphaDiversity()
}

\arguments{
None
}
\details{

}
\value{
The output will be a data frame with the following columns:
\item{allOutcomes }{Diversity indices}
\item{adjustedORList}{Odds ratio of rapid growth status by diversity index at 1-Month}
\item{pValORList}{P-value of odd ratio }
\item{adjORpValList}{FDR adjusted p-value of odd ratio }
}
\references{
}
\author{
Roshonda Barner Jones
}
\note{
}


\seealso{
}
\examples{
compute_OR_RapidGrowth_byAlphaDiversity()
}

\keyword{ ~manip }

