\name{compute_OR_RapidGrowth_bylefseTaxa}
\alias{compute_OR_RapidGrowth_bylefseTaxa}

\title{
Compute odds ratio of rapid growth status by taxa that were determined to be significantly different between rapid growth status.
}
\description{
This function will output the results of multiple logistic regression models determining association of rapid growth status and taxa that were determined to be significantly different between rapid growth status. The models adjust for birthweight and birthlength.
}
\usage{
compute_OR_RapidGrowth_bylefseTaxa()
}

\arguments{
None
}
\details{

}
\value{
The output will be a data frame with the following columns:
\item{allOutcomes }{Significant LEfSe taxa}
\item{adjustedORList}{Odds ratio of rapid growth status by taxa at 1-M}
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
compute_OR_RapidGrowth_bylefseTaxa()
}

\keyword{ ~manip }

